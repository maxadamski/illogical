package com.maxadamski.illogical

object Main {

  def printForm(x: Form): Unit = println(formatter.formatted(x))

  def parseForm(x: String): Option[Form] = Parser.parse(x) match {
    case Some(form) => 
      Some(form)
    case None => 
      println("invalid form!")
      None
  }

  val version = "0.1.0"

  val manpage = s"""
ILLOGICAL v$version MANUAL

1. commands:

analyze <FORM>
  -> displays every known property of given form

resolve <FORM> ; <FORM> ; ...
  -> executes the resolution algorithm
mts <FORM>
  -> executes the semantic tableaux algorithm

skolemize <FORM>
  -> displays FORM in the skolemized form
cnf <FORM>
  -> displays FORM in the conjuntive normal form
pnf <FORM>
  -> displays FORM in the prenex normal form
mgu <PRED> ; <PRED>
  -> displays the most general unifier

latex
  -> enables latex output
nolatex
  -> disables latex output
help
  -> displays this help
exit
  -> exits the program
"""

  val welcome = s"""
ILLOGICAL v$version - interactive mode

(type 'help' to display the manual)
"""

  var latex = false

  def formatter = 
    if (latex) LatexFormatter else TextFormatter

  def main(args: Array[String]) = {

    println(welcome)

    while (true) {
      print("illogical> ")
      val line = readLine()
      val s = line.split(" ", 2)
      val command = s(0)
      (s.head, s.tail.mkString(" ")) match {
        case ("exit", _) => 
          System.exit(0)
        case ("help", _) => 
          println(manpage)
        case ("latex", _) => 
          latex = true
        case ("nolatex", _) => 
          latex = false
        case ("analyze", form) => 
          parseForm(form)
            .foreach { f =>
              println("forms:")
              print("- regular:    "); printForm(f)
              print("- conjuntive: "); printForm(f.cnf)
              print("- prenex:     "); printForm(f.pnf)
              print("- skolemized: "); printForm(Skolemizer.skolemized(f))
              println()
              println("properties:")
              println("- model:      unknown")
              println("- satifiable: unknown")
              println("- true:       unknown")
              println("- consequent: unknown")
              println("- equivalent: unknown")
            }
        case ("resolve", form) => 
          parseForm(form)
            .foreach(f => printForm(f))
        case ("skolemize", form) => 
          parseForm(form)
            .foreach(f => printForm(Skolemizer.skolemized(f)))
        case ("cnf", form) => 
          parseForm(form)
            .foreach(f => printForm(f.cnf))
        case ("pnf", form) => 
          parseForm(form)
            .foreach(f => printForm(f.pnf))
        case ("mts", form) => 
          parseForm(form)
            .foreach(f => printForm(f))
        case ("mgu", forms) => 
          val fs = forms.split(";", 2)
            .flatMap(s => parseForm(s.trim))
          if (fs.size == 2) {
            val mgu = Unifier.mgu(fs(0), fs(1))
            mgu match {
              case Some(set) => println("{"+set.mkString(", ")+"}")
              case _ => println("could not unify")
            }
          } else {
            println("invalid number of predicates!")
          }
        case _ => 
          println("invalid command!")
      }

      println()
    }
  }
}
