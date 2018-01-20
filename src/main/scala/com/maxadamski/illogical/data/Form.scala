package com.maxadamski.illogical

case class Pred(name: String, arguments: List[Term]) extends Form with WithArgs with Named

case class Not(form: Form) extends Form

case class Qu(token: QuToken, variable: Var, form: Form) extends Form

case class Op(leftForm: Form, token: OpToken, rightForm: Form) extends Form {

  def isAssociative = 
    token.isAssociative

  def isCommutative = 
    token.isCommutative

  def equals(o: Op) =
    token == o.token && ((leftForm == o.leftForm && rightForm == o.rightForm) || 
      (leftForm == o.rightForm && rightForm == o.leftForm && token.isCommutative))

  override def equals(o: Any) = o match {
    case o: Op => equals(o)
    case _ => false
  }

}

sealed abstract class Form extends Node with LogicLaws {

  def isAtom = this match {
    case _: Pred => true
    case _ => false
  }

  def isLiteral = this match {
    case Not(term) => term.isAtom
    case _ => this.isAtom
  }

  def isClause = this match {
    case Op(a, OR, b) => a.isLiteral && b.isLiteral
    case _ => false
  }

  def pnf: Form = Skolemizer.pnf(this)

  def cnf: Form = this.simplifyingOperators.simplifyingNegation match {
    case Op(Pred(_, _), _, Pred(_, _)) => 
      this
    // a | (b & c) === (a | b) & (a | c)
    case Op(p, OR, Op(q, AND, r)) => 
      Op(Op(p.cnf, OR, q.cnf), AND, Op(p.cnf, OR, r.cnf))
    case Op(Op(q, AND, r), OR, p) =>
      Op(Op(p.cnf, OR, q.cnf), AND, Op(p.cnf, OR, r.cnf))
    case Op(p, OR, q) => 
      Op(p.cnf, OR, q.cnf)
    case Qu(token, v, p) =>
      Qu(token, v, p.cnf)
    case Not(p) =>
      Not(p.cnf)
    case Pred(_, _) => 
      this
    case _ =>
      this
  }

  def substituting(g: Set[Sub]): Form = this match {
    case Op(p, t, q) => Op(p.substituting(g), t, q.substituting(g))
    case Qu(t, v, p) => Qu(t, v, p.substituting(g))
    case Pred(t, a) => Pred(t, a.map(_.substituting(g)))
    case Not(p) => Not(p.substituting(g))
  }

  def renaming(v1: Var, v2: Var): Form = this match {
    case Pred(t, args) =>
      Pred(t, args.map(_.renaming(v1, v2)))
    case Op(p, t, q) =>
      Op(p.renaming(v1, v2), t, q.renaming(v1, v2))
    case Qu(t, v, p) =>
      val vNew = if (v == v1) v2 else v
      Qu(t, vNew, p.renaming(v1, v2))
    case Not(p) =>
      Not(p.renaming(v1, v2))
  }


  def simplifyingBinaryOperator: Option[Form] = this match {
      case Op(_, NAND,_) => expand_nand(this)
      case Op(_, NOR, _) => expand_nor(this)
      case Op(_, XOR, _) => expand_xor(this)
      case Op(_, IMP, _) => expand_imp(this)
      case Op(_, EQV, _) => expand_eqv(this)
      case _ => None
  }

  def simplifyingOperators: Form = this match {
    case Op(_, NAND | NOR | XOR | IMP | EQV, _) =>
      // TODO: do something about the force unwrapping
      simplifyingBinaryOperator.get.simplifyingOperators

    case Op(p, t, q) =>
      val p2 = p.simplifyingOperators
      val q2 = q.simplifyingOperators
      Op(p2, t, q2)

    case Qu(t, v, p) =>
      val p2 = p.simplifyingOperators
      Qu(t, v, p2)

    case Not(p) =>
      val p2 = p.simplifyingOperators
      Not(p2)

    case Pred(_, _) =>
      this
  }

  def simplifyingNegation: Form = this match {
      // TODO: do something about the force unwrapping
    case Not(form) => form match {
      case Qu(_, _, _) =>
        expand_not_quantifier(this).get.simplifyingNegation
      case Not(_) =>
        expand_not_not(this).get
      case Op(_, AND, _) | Op(_, OR, _) =>
        expand_de_morgan(this).get
      case Op(_, _, _) =>
        simplifyingOperators.simplifyingNegation
      case Pred(_, _) =>
        this
    }
    case Op(form_a, token, form_b) =>
      Op(form_a.simplifyingNegation, token, form_b.simplifyingNegation)
    case Qu(token, variable, form) =>
      Qu(token, variable, form.simplifyingNegation)
    case Pred(_, _) =>
      this
  }

  override def toString: String = 
    TextFormatter.formatted(this)

}
