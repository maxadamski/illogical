package com.maxadamski.illogical

object Skolemizer {

  def skolemized(form: Form): Form = {
    val (suffix, qus) = form.simplifying.partialPNF
    val universalQus = qus.filter(_.isUniversal)
    val skolemized = suffix.cnf.substituting(skolemSub(qus))
    skolemized.wrapped(universalQus)
  }

  private def skolemSub(qs: List[PartialQu]): Set[Sub] = {
    val es = qs.filter(_.isExistential)
    val vs = es.map(q => qs.slice(0, qs.indexOf(q))
      .filter(_.isUniversal)
      .map(_.variable))
    val zipped = es.map(_.variable) zip vs

    var k = 0
    var s = 0
    zipped.zipWithIndex.map { case (e, i) => 
      e match {
        case (v, Nil) => 
          k += 1
          Sub(v, Con("k" + k))
        case (v, vs) => 
          s += 1
          Sub(v, Func("s" + s, vs))
      }
    }.toSet
  }


}
