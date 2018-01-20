package com.maxadamski.illogical

trait Named {
  val name: String

  require(name != "" && !name.contains(" "), s"Invalid name '$name'!")
}

trait WithArgs {
  val arguments: List[Term]

  require(!arguments.isEmpty, "Cannot pass empty argument list!")
}

case class Func(name: String, arguments: List[Term]) extends Term with WithArgs
case class Var(name: String) extends Term

case class Con(name: String) extends Term

sealed abstract class Term extends Node {

  def substituting(sub: Set[Sub]): Term = this match {
    case Func(name, args) => Func(name, args.map(_.substituting(sub)))
    case v: Var => termForVar(v, sub) getOrElse this
    case _ => this
  }

  def vars: Set[Var] = this match {
    case Func(_, args) => args.map(_.vars).toSet.flatten
    case v: Var => Set(v)
    case c: Con => Set()
  }

  def cons: Set[Con] = this match {
    case Func(_, args) => args.map(_.cons).toSet.flatten
    case c: Con => Set(c)
    case v: Var => Set()
  }
  
  def termForVar(v: Var, subs: Set[Sub]): Option[Term] =
    subs.find(_.v == v).map(_.t)

  def renaming(x: Var, y: Var) =
    substituting(Set(Sub(x, y)))

  def contains(v: Var) = 
    vars contains v

  override def toString = 
    TextFormatter.fmt(this)

}

