package com.maxadamski.illogical

object Resolver {
  def isTrue(form: Form): Boolean = {
    _isUnsat(Skolemizer.skolemized(Not(form)).clauses)
  }

  def isUnsat(form: Form): Boolean = {
    _isUnsat(Skolemizer.skolemized(form).clauses)
  }

  def setRep[T](s: Set[T]): String = {
    s.toString
  }

  implicit class PrettySet[T](val set: Set[T]) {
    override def toString =
      "{" + set.mkString(", ") + "}"

  }

  def printSet[T](s: Set[T]): Unit =
    println(setRep(s))

  def listRep[T](l: List[T]): String = {
    "[" + l.mkString(", ") + "]"
  }

  def mgusRep[T](t: List[Set[T]]): String = {
    "[" + t.map(setRep).mkString(", ") + "]"
  }

  case class Clause(lits: Set[Form], comps: List[Set[Form]], mgus: List[Set[Sub]]) {
    override def toString = {
      s"""
      ${setRep(lits)}
      - comp: ${mgusRep(comps)}
      - mgus: ${mgusRep(mgus)}
      """
    }
  }

  def _isUnsat(clauses: Set[Form]): Boolean = {
    require(clauses.forall(_.isClause))
    var clauseSet: Set[Clause] = clauses.map(c => Clause(c.literals, List(), List()))

    println("\nstart")

    val max = 10
    var i = 0
    while (i < max) {
      println("\nall clauses:")
      //clauseSet.foreach(println)

      if (clauseSet.exists({ case Clause(s, _, _) if s.isEmpty => true; case _ => false })) {
        //println("end")
        println(clauseSet.collectFirst({ case c @ Clause(s, _, _) if s.isEmpty => c }))
        return true
      }

      val newClauses: Set[Clause] = clauseSet.subsets(2).map({subset => 
        subset.toList match { case List(a, b) =>
          val mgu = Unifier.mgu(a.lits, b.lits)
          if (mgu != None && compliment(a.lits ++ b.lits)) {
            val x = (a.lits ++ b.lits).map(lit => lit.substituting(mgu.get))
            Some(Clause(removingCompliment(x), a.comps ++ b.comps ++ List(a.lits, b.lits), a.mgus ++ b.mgus :+ mgu.get))
          } else {
            None
          }
        }
      }).toSet.flatten

      //println("\nnew clauses:")
      //(newClauses -- clauseSet).foreach(println)
      if ((newClauses -- clauseSet).isEmpty) {
        //println("- no new clauses")
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
