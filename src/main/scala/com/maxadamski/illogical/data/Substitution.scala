package com.maxadamski.illogical

case class Sub(v: Var, t: Term) {

  def sub(s: Set[Sub]) =
    (for { sub <- s.find(_.v == v) } yield Sub(v, sub.t)) getOrElse this

  def isFinal(s: Set[Sub]) = 
    !s.exists(sub => t.contains(sub.v))

  override def toString: String = 
    s"${v.name} <- ${t}"

}

