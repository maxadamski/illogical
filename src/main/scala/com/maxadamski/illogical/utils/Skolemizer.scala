package com.maxadamski.illogical

object Skolemizer {

  def skolemized(form: Form): Form = {
    val (suffix, qus) = partialPNF(form.simplifyingOperators.simplifyingNegation)
    val universalQus = qus.filter(_.isUniversal)
    val skolemized = suffix.cnf.sub(skolemSub(qus))
    wrapped(skolemized, universalQus)
  }

  def pnf(form: Form): Form = {
    val (suffix, qus) = partialPNF(form.simplifyingOperators.simplifyingNegation)
    wrapped(suffix, qus)
  }

  private def skolemSub(qs: List[PartialQu]): Set[Sub] = {
    val es = qs.filter(_.isExistential)
    val vs = es.map(q => qs.slice(0, qs.indexOf(q))
      .filter(_.isUniversal)
      .map(_.variable))
    val zipped = es.map(_.variable) zip vs
    zipped.zipWithIndex.map { case (e, i) => 
      e match {
        case (v, Nil) => Sub(v, Con("k" + (i + 1)))
        case (v, vs) => Sub(v, Func("s" + (i + 1), vs))
      }
    }.toSet
  }

  private def wrapped(form: Form, qs: List[PartialQu]): Form = qs match {
    case x :: _ => 
      wrapped(qs.last.complete(form), qs.dropRight(1))
    case Nil => 
      form
  }

  private def partialPNF(form: Form): (Form, List[PartialQu]) = {
    val (suffix, vars, qus) = partialPNF(form, List(), List())
    (suffix, qus)
  }

  private def partialPNF(form: Form, vars: List[Var], qs: List[PartialQu]): (Form, List[Var], List[PartialQu]) = form match {
    case Qu(t, v, p) =>
      var newVar = v
      while (vars.contains(newVar)) newVar = Var(newVar.name + "'")
      val renamed = p.renaming(v, newVar)
      val (newP, newVars, newQs) = partialPNF(renamed, vars :+ newVar, qs)
      (newP,  newVars, PartialQu(t, newVar) +: newQs)

    case Op(p, t, q) =>
      val (newP, newPVars, newPQs) = partialPNF(p, vars, qs)
      val (newQ, newQVars, newQQs) = partialPNF(q, newPVars, qs)
      (Op(newP, t, newQ), newPVars ++ newQVars, newPQs ++ newQQs)
    case Not(p) =>
      val (newP, newVars, newQs) = partialPNF(p, vars, qs)
      (Not(newP), newVars, newQs)
    case Pred(_, _) =>
      (form, vars, qs)
  }

}
