package com.maxadamski.illogical

object Unifier {
  def mgu(p: Form, q: Form): Option[Set[Sub]] = (p, q) match {
    case (p: Pred, q: Pred) => mgu(p, q)
    case _ => None
  }

  def mgu(p: Pred, q: Pred): Option[Set[Sub]] = {
    if (p.name == q.name && p.arguments.length == q.arguments.length) {
      mgu((p.arguments zip q.arguments).toSet)
    } else {
      None
    }
  }

  private def mgu(mgu: Set[(Term, Term)]): Option[Set[Sub]] = {
    var g = mgu

    while (true) {
      val partial = makeSubs(g)

      if (partial.forall(_ != None)) {
        val flat = partial.flatten

        if (conflict(flat))
          return None
        else if (flat.forall(_.isFinal(flat)))
          return Some(flat)
      }

      g.foreach {
        case (x: Var, y: Var) if x == y => 
          g -= ((x, y))
        case (x: Func, y: Func) if x.name == y.name => 
          g -= ((x, y))
          g ++= (x.arguments zip y.arguments)
        case (x: Func, y: Func) if x.name != y.name => 
          return None
        case (x: Term, y: Var) =>
          g -= ((x, y))
          g += ((y, x))
        case (x: Var, y: Term) if !y.contains(x) => 
          g -= ((x, y))
          g += ((x, y.substituting(makeSubs(g).flatten)))
        case (v: Var, f: Func) if f.contains(v) => 
          return None
        case _ =>
          return None
      }
    }

    return None
  }

  private def conflict(mgu: Set[Sub]) =
    mgu.groupBy(_.v).exists { case (_, sub) => sub.size > 1 }

  private def makeSubs(set: Set[(Term, Term)]) = set.map {
    case (v: Var, t: Term) => Some(Sub(v, t))
    case _ => None
  }
  
}

