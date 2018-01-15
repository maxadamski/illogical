package com.maxadamski.illogical

import org.scalatest._

abstract class UnitSpec extends FunSpec with Matchers {
  def shouldParse(formula: String, result: Form, comment: String = "") {
    it(s"should parse `${formula}`" + comment) {
      Parser.parse(formula) shouldEqual Some(result)
    }
  }

  val (x, y, z) = (Var("x"), Var("y"), Var("z"))
  val (p, q, r) = (Pred("p", List(x)), Pred("q", List(x)), Pred("r", List(x)))
}
