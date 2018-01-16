package com.maxadamski.illogical

////////////////////////////////////////////////
// grammar
////////////////////////////////////////////////


/*

Qu    -> quantifiertoken
Op    -> operator_token
Con   -> string
Var   -> string
Func  -> string
Pred  -> string

Args  -> Term | Term, Args
Term  -> Con | Var | Func(Args)
Form  -> Pred(Args) | Not Form | Form Op Form | Qu Var Form
Literal -> Atom | Not Atom
Clause  -> Literal OR Literal
Atom  -> Pred(Args)
#Form  -> Atom

*/

sealed abstract class QuToken
case object FORALL extends QuToken
case object EXISTS extends QuToken

sealed abstract class OpToken
case object OR extends OpToken
case object AND extends OpToken
case object NOR extends OpToken
case object NAND extends OpToken
case object XOR extends OpToken
case object IMP extends OpToken
case object EQV extends OpToken

sealed abstract class Node

case class Con(name: String) extends Term
case class Var(name: String) extends Term
case class Func(name: String, arguments: List[Term]) extends Term

case class Pred(name: String, arguments: List[Term]) extends Form
case class Not(form: Form) extends Form
case class Op(leftForm: Form, token: OpToken, rightForm: Form) extends Form
case class Qu(token: QuToken, variable: Var, form: Form) extends Form

case class PartialQu(token: QuToken, variable: Var) {
  def complete(form: Form): Form = Qu(token, variable, form)
}

////////////////////////////////////////////////
// LOGIC TRANSFORMATIONS
////////////////////////////////////////////////

trait LogicOps {
  val converse: Form => Option[Form] = { 
    case Op(a, token, b) => Some(Op(b, token, a))
    case _ => None
  }

  val inverse: Form => Option[Form] = {
    case Op(a, token, b) => Some(Op(Not(a), token, Not(b)))
    case _ => None
  }

  val expand_not_quantifier: Form => Option[Form] = {
    case Not(Qu(FORALL, v, p)) => Some(Qu(EXISTS, v, Not(p)))
    case Not(Qu(EXISTS, v, p)) => Some(Qu(FORALL, v, Not(p)))
    case _ => None
  }

  val expand_de_morgan: Form => Option[Form] = {
    case Not(Op(a, AND, b)) => Some(Op(Not(a), OR, Not(b)))
    case Not(Op(a, OR, b)) => Some(Op(Not(a), AND, Not(b)))
    case _ => None
  }

  val expand_not_not: Form => Option[Form] = {
    case Not(Not(a)) => Some(a)
    case _ => None
  }

  val expand_imp: Form => Option[Form] = { 
    case Op(a, IMP, b) => Some(Op(Not(a), OR, b))
    case _ => None
  }

  val expand_eqv: Form => Option[Form] = { 
    case Op(a, EQV, b) => Some(Not(Op(a, XOR, b)))
    case _ => None
  }

  val expand_nand: Form => Option[Form] = { 
    case Op(a, NAND,b) => Some(Not(Op(a, AND, b)))
    case _ => None
  }

  val expand_nor: Form => Option[Form] = { 
    case Op(a, NOR, b) => Some(Not(Op(a, OR, b)))
    case _ => None
  }

  val expand_xor: Form => Option[Form] = { 
    case Op(a, XOR, b) => Some(Op(Op(a, OR, b), AND, Op(a, NAND, b)))
    case _ => None
  }
}

////////////////////////////////////////////////
// TERM & FORM
////////////////////////////////////////////////


sealed abstract class Term extends Node {


}

sealed abstract class Form extends Node with LogicOps {
  def isAtom: Boolean = this match {
    case Pred(_, _) => true
    case _ => false
  }

  def isLiteral: Boolean = this match {
    case Not(term) => term.isAtom
    case _ => this.isAtom
  }

  def isClause: Boolean = this match {
    case Op(a, OR, b) => a.isLiteral && b.isLiteral
    case _ => false
  }

  def simplifyingBinaryOperator: Option[Form] = this match {
      case Op(_, NAND,_) => expand_nand(this)
      case Op(_, NOR, _) => expand_nor(this)
      case Op(_, XOR, _) => expand_xor(this)
      case Op(_, IMP, _) => expand_imp(this)
      case Op(_, EQV, _) => expand_eqv(this)
      case _ => None
  }

  
  def substitutingVariable(matcher: Var, replacement: Var): Form = this match {
    case Not(p) =>
      val p2 = p.substitutingVariable(matcher, replacement)
      Not(p2)

    case Op(p, token, q) =>
      val p2 = p.substitutingVariable(matcher, replacement)
      val q2 = q.substitutingVariable(matcher, replacement)
      Op(p2, token, q2)

    case Qu(token, variable, p) =>
      val variable2 = if (variable == matcher) replacement else variable
      val p2 = p.substitutingVariable(matcher, replacement)
      Qu(token, variable2, p2) 

    case Pred(name, args) =>
      val args2 = args.map { case `matcher` => replacement; case x => x }
      Pred(name, args2)
  }

  def prefixForm: Form = {
    val (suffix, quantifiers) = simplifyingOperators.simplifyingNegation.extractingQuantifiers
    suffix.wrappedInQuantifiers(quantifiers)
  }

  def cnf: Form = this.prefixForm match {
    // a | (b & c) === (a | b) & (a | c)
    case Op(p, OR, Op(q, AND, r)) => 
      Op(Op(p.cnf, OR, q.cnf), AND, Op(p.cnf, OR, r.cnf))
    case Op(Op(q, AND, r), OR, p) =>
      Op(Op(p.cnf, OR, q.cnf), AND, Op(p.cnf, OR, r.cnf))
    // !(a & b) === !a | !b

    case Qu(token, v, p) =>
      Qu(token, v, p.cnf)
    case Pred(_, _) => 
      this
    case Not(p) =>
      Not(p.cnf)
    case Op(p: Pred, OR, q: Pred) => 
      this
    case Op(p, OR, q) => 
      Op(p.cnf, OR, q.cnf)
    case _ =>
      this
  }

  def wrappedInQuantifiers(qs: List[PartialQu]): Form = qs match {
    case x :: _ => 
      qs.last.complete(this).wrappedInQuantifiers(qs.dropRight(1))
    case Nil => 
      this
  }

  def extractingQuantifiers: (Form, List[PartialQu]) = extractingQuantifiers(List())

  def extractingQuantifiers(qs: List[PartialQu]): (Form, List[PartialQu]) = this match {
    case Qu(token, variable, a) =>
      val (a_, qs_) = a.extractingQuantifiers(qs)
      (a_, PartialQu(token, variable) +: qs_)
    case Op(a, token, b) =>
      val (a_, a_qs) = a.extractingQuantifiers(qs)
      val (b_, b_qs) = b.extractingQuantifiers(qs)
      (Op(a_, token, b_), a_qs ++ b_qs)
    case Not(a) =>
      val (a_, qs_) = a.extractingQuantifiers(qs)
      (this, qs_)
    case Pred(_, _) =>
      (this, qs)
  }

  // expresses Form in terms of AND, OR and NOT
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
    case Not(form) => form match {
      case Qu(_, _, _) =>
        // TODO: do something about the force unwrapping
        expand_not_quantifier(this).get.simplifyingNegation
      case Not(_) =>
        // TODO: do something about the force unwrapping
        expand_not_not(this).get
      case Op(_, AND, _) | Op(_, OR, _) =>
        // TODO: do something about the force unwrapping
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
}

