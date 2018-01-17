package com.maxadamski.illogical

trait Formatter {
  def formatted(node: Form) = {
    fmt(node)
  }

  def fmt(n: Node): String = n match {
    case x: Func => x.name + args(x.arguments)
    case x: Pred => x.name + args(x.arguments)
    case x: Not => "¬" + fmt(x.form)
    case x: Con => x.name
    case x: Var => x.name
    case x: Op => brace(x.leftForm) + " " + symbol(x.token) + " " + brace(x.rightForm)
    case x: Qu => symbol(x.token) + fmt(x.variable) + "." + brace(x.form)
  }

  def symbol(x: OpToken) = x match {
    case OR   => "∨"
    case AND  => "∧"
    case NOR  => "↓"
    case NAND => "↑"
    case XOR  => "⊕"
    case IMP  => "→"
    case EQV  => "≡"
  }

  def symbol(x: QuToken) = x match {
    case FORALL => "∀"
    case EXISTS => "∃"
  }

  def brace(n: Node) = n match {
    case x: Op => "(" + fmt(x) + ")"
    case x => fmt(x)
  }

  def args(x: List[Term]) = {
    "(" + x.map(fmt).mkString(", ") + ")"
  }
}

object TextFormatter extends Formatter

object LatexFormatter extends Formatter {
  override def formatted(node: Form) = {
    "$$" + fmt(node) + "$$"
  }

  override def fmt(n: Node) = n match {
    case x: Qu => symbol(x.token) + "_{" + fmt(x.variable) + "} " + fmt(x.form)
    case x => super.fmt(x)
  }
}
