package com.maxadamski.illogical

object Main {
  def printForm(x: Form): Unit = println(TextFormatter.formatted(x))

  def printForm(x: String): Unit = Parser.parse(x) match {
    case Some(form) => 
      printForm(form)
      printForm(form.cnf)
    case None => 
      println("invalid form!")
  }

  def main(args: Array[String]) = {
    while (true) {
      print("illogical> ")
      val line = readLine()
      printForm(line)
      println()
    }
  }
}
