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

sealed abstract class OpToken {
  def isCommutative = this match {
    case OR | AND | NOR | NAND | XOR => true
    case _ => false
  }

  def isAssociative = this match {
    case OR | AND | NOR | NAND | XOR => true
    case _ => false
  }
}

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
case class Qu(token: QuToken, variable: Var, form: Form) extends Form

case class Op(leftForm: Form, token: OpToken, rightForm: Form) extends Form {
  def isAssociative = token.isAssociative

  def isCommutative = token.isCommutative

  def equals(o: Op) =
    token == o.token && ((leftForm == o.leftForm && rightForm == o.rightForm) || (leftForm == o.rightForm && rightForm == o.leftForm && token.isCommutative))

  override def equals(o: Any) = o match {
    case other: Op => equals(other)
    case _ => false
  }
}

case class PartialQu(token: QuToken, variable: Var) {
  def complete(form: Form): Form = Qu(token, variable, form)
  def existential = token == EXISTS
  def universal = token == FORALL
}

case class Sub(v: Var, t: Term) {
  override def toString: String = 
    s"${v.name} <- ${t}"

  def sub(s: Set[Sub]) =
    (for { sub <- s.find(_.v == v) } yield Sub(v, sub.t)) getOrElse this
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

  override def toString: String = TextFormatter.fmt(this)

  def renaming(v1: Var, v2: Var): Term = this match {
    case Func(t, args) =>
      Func(t, args.map(_.renaming(v1, v2)))
    case `v1` => v2
    case _ => this
  }

  def sub(s: Set[Sub]): Term = this match {
    case Func(name, args) =>
      Func(name, args.map(_.sub(s)))
    case Var(name) =>
      (for { sub <- s.find(_.v == this) } yield sub.t) getOrElse this
    case _ => this
  }

  def contains(v: Var): Boolean = this match {
    case Func(name, args) => args.contains(contains(v))
    case Var(name) => v.name == name
    case _ => false
  }

}

sealed abstract class Form extends Node with LogicOps {

  override def toString: String = TextFormatter.formatted(this)

  def isAtom: Boolean = this match {
    case _: Pred => true
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

  def pnf: Form = {
    var (suffix, quantifiers) = simplifyingOperators.simplifyingNegation.extractingQuantifiers
    suffix.cnf.simplifyingOperators.simplifyingNegation.wrappedInQuantifiers(quantifiers)
  }

  def skolemized: Form = {
    var (suffix, quantifiers) = simplifyingOperators.simplifyingNegation.extractingQuantifiers
    var skolemsub = skolemSubstitution(quantifiers)
    quantifiers = quantifiers.filter(_.universal)
    suffix.cnf.simplifyingOperators.simplifyingNegation.sub(skolemsub).wrappedInQuantifiers(quantifiers)
  }

  def cnf: Form = this match {
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

  def sub(g: Set[Sub]): Form = this match {
    case Op(p, t, q) => Op(p.sub(g), t, q.sub(g))
    case Qu(t, v, p) => Qu(t, v, p.sub(g))
    case Not(p) => Not(p.sub(g))
    case Pred(t, a) => Pred(t, a.map(_.sub(g)))
  }

  def skolemSubstitution(qs: List[PartialQu]): Set[Sub] =
    skolemSubstitution(qs, Set())

  def skolemSubstitution(qs: List[PartialQu], g: Set[Sub]): Set[Sub] = {
    val es = qs.filter(_.existential)
    val vs = es.map(q => qs.slice(0, qs.indexOf(q))
      .filter(_.universal)
      .map(_.variable))
    val zipped = es.map(_.variable) zip vs
    zipped.zipWithIndex.map { case (e, i) => 
      e match {
        case (v, Nil) => Sub(v, Con("k" + (i + 1)))
        case (v, vs) => Sub(v, Func("s" + (i + 1), vs))
      }
    }.toSet
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

  def wrappedInQuantifiers(qs: List[PartialQu]): Form = qs match {
    case x :: _ => 
      qs.last.complete(this).wrappedInQuantifiers(qs.dropRight(1))
    case Nil => 
      this
  }

  def extractingQuantifiers: (Form, List[PartialQu]) = {
    val (form, vars, qus) = extractingQuantifiers(List(), List())
    (form, qus)
  }

  def extractingQuantifiers(vars: List[Var], qs: List[PartialQu]): (Form, List[Var], List[PartialQu]) = this match {
    case Qu(t, v, p) =>
      var newVar = v
      while (vars.contains(newVar)) newVar = Var(newVar.name + "'")
      val renamed = p.renaming(v, newVar)
      val (newP, newVars, newQs) = renamed.extractingQuantifiers(vars :+ newVar, qs)
      (newP,  newVars, PartialQu(t, newVar) +: newQs)

    case Op(p, t, q) =>
      val (newP, newPVars, newPQs) = p.extractingQuantifiers(vars, qs)
      val (newQ, newQVars, newQQs) = q.extractingQuantifiers(newPVars, qs)
      (Op(newP, t, newQ), newPVars ++ newQVars, newPQs ++ newQQs)
    case Not(p) =>
      val (newP, newVars, newQs) = p.extractingQuantifiers(vars, qs)
      (Not(newP), newVars, newQs)
    case Pred(_, _) =>
      (this, vars, qs)
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

object Unifier {
  def mgu(p: Form, q: Form): Option[Set[Sub]] = (p, q) match {
    case (p: Pred, q: Pred) => mgu(p, q)
    case _ => None
  }

  def mgu(p: Pred, q: Pred): Option[Set[Sub]] = {
    if (p.name != q.name) None else mgu(p.arguments, q.arguments)
  }

  def mgu(p: List[Term], q: List[Term]): Option[Set[Sub]] = {
    if (p.length != q.length) return None
    Some(Set())
  }

//  def sub(g: Set[Sub], s: Sub): Set[Sub] =
//    g.map(_.sub(Set(s)))
//
//
//    (p zip q).foreach {
//      // delete
//      case (u: Var, v: Var) if u == v => 
//        G -= 
//        G = G
//      // decompose
//      case (f: Func, g: Func) if f.name == g.name => 
//        G += (f.arguments zip g.arguments).toSet
//      // conflict
//      case (f: Func, g: Func) if f.name != g.name => 
//        return None
//      // swap
//      case (c: Con, v: Var) => 
//        G += Sub(v, c)
//      // eliminate
//      case (v: Var, t: Term) if !t.containsVar(v) && G.contains(_.v == v) => 
//        G = sub(G, Sub(v, t)) + Sub(v, t)
//
//
//   // g = g diff g.filter(==)
//   // g = g flatMap {
//   //   case (Func(f, fa), Func(g, ga)) if f == g => fa zip ga
//   // }
//   // if (g filter {
//   //   case (Func(f, fa), Func(g, ga)) if f != g => true
//   // }) return None
//   //
//   //
//
//   var g = (p zip q).toSet
//   var h = Set[(Term, Term)]()
//   g.foreach { 
//    case (x: Var, y: Var) if x == y => 
//      h = h
//    case (t: Term, v: Var) =>
//      h = h + ((v, t))
//    case (Func(f, fs), Func(h, hs)) if f != h => 
//      return None
//    case (x: Var, Func(f, fs)) if fs.contains(x) => 
//      None
//
//
//   }
//
//
//   
//   Some(h.flatten)
//  }
//
//  case class Eq(p: Term, q: Term) {
//
//  }
//
//
//  def unify(p: Term, q: Term, g: Set[(Term, Term)]): Option[Set[(Term, Term)]] = (p, q) match {
//    case (Var(x), Var(y)) if x == y => 
//      Some(g)
//
//    case (Func(f, fs), Func(h, hs)) if f == h => 
//      val unified = (fs zip hs).map { case (p, q) => unify(p, q, g) }
//      if (!unified.contains(None)) 
//        Some(g ++ unified.flatten.flatten) 
//      else 
//        None
//
//    case (Func(f, fs), Func(h, hs)) if f != h => 
//      None
//
//    case (t: Term, v: Var) =>
//      val unified = unify(v, t, Set())
//      if (unified != None) 
//        Some(g ++ unified.get)
//      else
//        None
//
//    case (x: Var, t: Term) if !t.contains(x) => 
//      val newg = g
//      val unified = unify(x, t, Set())
//      if (unified != None) 
//        Some(newg ++ unified.get)
//      else
//        None
//
//    case (x: Var, Func(f, fs)) if fs.contains(x) => 
//      None
//  }
}

