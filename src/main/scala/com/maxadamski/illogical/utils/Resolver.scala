package com.maxadamski.illogical

object Resolver {
  def isTrue(form: Form) {
    isUnsat(Not(form))
  }

  def isUnsat(form: Form) {
    isUnsat(form.clauses)
  }

  def isUnsat(clauses: Set[Form]) {
    require(clauses.forall(_.isClause))

  }
}
