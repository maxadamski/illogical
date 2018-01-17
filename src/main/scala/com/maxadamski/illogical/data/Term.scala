package com.maxadamski.illogical

case class Func(name: String, arguments: List[Term]) extends Term
case class Var(name: String) extends Term
case class Con(name: String) extends Term

sealed abstract class Term extends Node {

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
    case Func(name, args) => args.exists { arg => arg.contains(v) }
    case Var(name) => v.name == name
    case _ => false
  }

  override def toString: String = 
    TextFormatter.fmt(this)

}

