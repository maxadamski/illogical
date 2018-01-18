package com.maxadamski.illogical

import org.scalatest._

abstract class UnitSpec extends FunSpec with Matchers {
  def shouldParse(formula: String, result: Form, comment: String = "") {
    it(s"should parse `$formula`" + comment) {
      Parser.parse(formula) shouldEqual Some(result)
    }
  }

  def shouldEqual(message: String, in: String, out: String, transform: Form => Form): Unit = {
    it(message) {
      transform(Parser.parse(in).get) shouldEqual Parser.parse(out).get
    }
  }

  def itShouldPNF(in: String, out: String): Unit = {
    shouldEqual(s"should pnf $in", in, out, _.pnf)
  }

  def itShouldSkolemize(in: String, out: Form): Unit = {
    it(s"should skolemize $in") {
      Skolemizer.skolemized(out) shouldEqual out
    }
  }

  def itShouldMGU(pString: String, qString: String, mgu: Set[Sub]): Unit = {
    val (p, q) = (Parser.parse(pString).get, Parser.parse(qString).get)
    it(s"should mgu $pString and $qString") {
      Unifier.mgu(p, q) shouldEqual Some(mgu)
    }
  }

  def itShouldNotMGU(pString: String, qString: String): Unit = {
    val (p, q) = (Parser.parse(pString).get, Parser.parse(qString).get)
    it(s"should not mgu $pString and $qString") {
      Unifier.mgu(p, q) shouldEqual None
    }
  }


  val (x, y, z) = (Var("x"), Var("y"), Var("z"))
  val (p, q, r) = (Pred("p", List(x)), Pred("q", List(x)), Pred("r", List(x)))
}
