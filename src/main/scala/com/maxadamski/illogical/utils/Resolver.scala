package com.maxadamski.illogical

object Resolver {
  def isTrue(form: Form): Boolean = {
    _isUnsat(Skolemizer.skolemized(Not(form)).clauses)
  }

  def isUnsat(form: Form): Boolean = {
    _isUnsat(Skolemizer.skolemized(form).clauses)
  }

  def _isUnsat(clauses: Set[Form]): Boolean = {
    require(clauses.forall(_.isClause))
    var clauseSet: Set[Set[Form]] = clauses.map(_.literals)

    println("start")

    val max = 10
    var i = 0
    while (i < max) {
      clauseSet.foreach { set => println(set) }

      if (clauseSet.contains(Set())) {
        println("end")
        return true
      }

      val newClauses: Set[Set[Form]] = clauseSet.subsets(2).map({subset => 
        subset.toList match { case List(a, b) =>
          val mgu = Unifier.mgu(a, b)
          if (mgu != None && compliment(a ++ b)) {
            val x = (a ++ b).map(lit => lit.substituting(mgu.get))
            Some(removingCompliment(x))
          } else {
            None
          }
        }
      }).toSet.flatten

      if ((newClauses -- clauseSet).isEmpty) {
        println("no new clauses")
        return false
      }

      clauseSet ++= newClauses

      println()

      i += 1
    }

    false
  }

  private def compliment(lits: Set[Form]): Boolean = {
    lits.exists { 
      case Pred(n, _) => 
        lits exists { case Not(Pred(m, _)) => n == m; case _ => false }
      case Not(Pred(n, _)) => 
        lits exists { case Pred(m, _) => n == m; case _ => false }
      case _ => false
    }
  }

  private def complimentClauses(clauses: Set[Set[Form]]): Boolean = {
    false
  }

  private def removingCompliment(lits: Set[Form]): Set[Form] = {
    var remove = Set[Form]()
    lits.foreach {
      case lit @ Pred(n, p) => 
        val x = lits exists { case Not(Pred(m, _)) => n == m; case _ => false }
        if (x) remove ++= Set(lit, Not(lit))
      case lit @ Not(Pred(n, p)) => 
        val x = lits exists { case Pred(m, _) => n == m; case _ => false }
        if (x) remove ++= Set(lit, Pred(n, p))
      case _ => false
    }
    lits -- remove
  }
}
