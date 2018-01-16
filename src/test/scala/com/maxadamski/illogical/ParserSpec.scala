package com.maxadamski.illogical

import org.scalatest._

class ParserSpec extends UnitSpec {
  describe("Parser") {

    describe("given valid form") {


      describe("given atom") {

        it("should parse atom with constant") {
          Parser.parse("p(a)") shouldEqual Some(Pred("p", List(Con("a"))))
        }

        it("should parse atom with multiple constants") {
          Parser.parse("p(a, b)") shouldEqual Some(Pred("p", List(Con("a"), Con("b"))))
        }

        it("should parse atom with variable") {
          Parser.parse("p(x)") shouldEqual Some(Pred("p", List(Var("x"))))
        }

        it("should parse atom with multiple variables") {
          Parser.parse("p(x, y)") shouldEqual Some(Pred("p", List(Var("x"), Var("y"))))
        }

        it("should parse atom with function") {
          Parser.parse("p(f(x))") shouldEqual Some(Pred("p", List(Func("f", List(Var("x"))))))
        }

        it("should parse atom with nested function") {
          val h = Func("h", List(Var("x")))
          val g = Func("g", List(h))
          val f = Func("f", List(g))
          Parser.parse("p(f(g(h(x))))") shouldEqual Some(Pred("p", List(f)))
        }

        it("should parse atom with complex arguments") {
          Parser.parse("p(x, f(x, f(x, y), y), a, f(f(f(z))))") shouldEqual
            Some(Pred("p", List(Var("x"), Func("f", List(Var("x"), Func("f", List(Var("x"), Var("y"))), Var("y"))), Con("a"), 
              Func("f", List(Func("f", List(Func("f", List(Var("z"))))))))))
        }

      }

      describe("given negated atom") {
      
        List("-", "~", "!", "NOT").foreach { symbol =>

          val result = Not(Pred("p", List(Var("x"))))
          shouldParse(s"${symbol}p(x)", result)
          shouldParse(s"${symbol} p(x)", result)

        }

      }

      describe("given quantified atom") {
          List("A", "ALL", "FA", "FALL", "FORA", "FORALL").foreach { symbol =>
            val result = Qu(FORALL, Var("x"), Pred("p", List(Var("x"))))
            shouldParse(s"${symbol}x p(x)", result)
          }

          List("E", "EX", "EXIST", "EXISTS").foreach { symbol =>
            val result = Qu(EXISTS, Var("x"), Pred("p", List(Var("x"))))
            shouldParse(s"${symbol}x p(x)", result)
          }

          shouldParse("Ax Ay Az p(x, y, z)", Qu(FORALL, x, Qu(FORALL, y, Qu(FORALL, z, Pred("p", List(x, y, z))))))

          shouldParse("Ax !Ay p(x, y)", Qu(FORALL, x, Not(Qu(FORALL, y, Pred("p", List(x, y))))))

          shouldParse("Ax (!Ay p(x, y))", Qu(FORALL, x, Not(Qu(FORALL, y, Pred("p", List(x, y))))))

          shouldParse("Ax !(Ay p(x, y))", Qu(FORALL, x, Not(Qu(FORALL, y, Pred("p", List(x, y))))))

          shouldParse("!Ax Ay p(x, y)", Not(Qu(FORALL, x, Qu(FORALL, y, Pred("p", List(x, y))))))

          shouldParse("!(Ax Ay p(x, y))", Not(Qu(FORALL, x, Qu(FORALL, y, Pred("p", List(x, y))))))

          shouldParse("Ax !p(x, y)", Qu(FORALL, x, Not(Pred("p", List(x, y)))))
      }


      describe("given binary operation") {
        shouldParse("p(x) | q(x)", Op(p, OR, q))

        shouldParse("!p(x) | q(x)", Op(Not(p), OR, q))

        shouldParse("p(x) | !q(x)", Op(p, OR, Not(q)))

        shouldParse("(p(x) | q(x))", Op(p, OR, q))

        shouldParse("!(p(x) | q(x))", Not(Op(p, OR, q)))

        shouldParse("((p(x) | q(x)))", Op(p, OR, q))

        shouldParse("p(x) | q(x) | r(x)", Op(Op(p, OR, q), OR, r))

        shouldParse("(p(x) | q(x)) | r(x)", Op(Op(p, OR, q), OR, r))

        shouldParse("!(p(x) | q(x)) | r(x)", Op(Not(Op(p, OR, q)), OR, r))

        shouldParse("p(x) | !(q(x) | r(x))", Op(p, OR, Not(Op(q, OR, r))))

        shouldParse("p(x) | Ax q(x)", Op(p, OR, Qu(FORALL, x, q)))

        shouldParse("Ax p(x) | q(x)", Op(Qu(FORALL, x, p), OR, q))

        shouldParse("Ax (p(x) | q(x))", Qu(FORALL, x, Op(p, OR, q)))

        shouldParse("Ax !(p(x) | q(x))", Qu(FORALL, x, Not(Op(p, OR, q))))
      }


      describe("given more advanced formulas") {
        shouldParse("ExAy(Ez p(f(x, y, z)) -> Ez(!q(z) & p(x)))", Qu(EXISTS, x, Qu(FORALL, y, Op(
          Qu(EXISTS, z, 
            Pred("p", List(Func("f", List(x, y, z))))
          ),
          IMP,
          Qu(EXISTS, z, 
            Op(Not(Pred("q", List(z))), AND, Pred("p", List(x)))
          )
        ))))

        shouldParse("Ev.jAx(Ev.ry p.au(f(x, y, z)) | Ez(-p.qa(z) & p(v.a)))", 
          Qu(EXISTS, Var("v.j"), 
            Qu(FORALL, x, 
              Op(
                Qu(EXISTS, Var("v.ry"), Pred("p.au", List(Func("f", List(x, y, z))))),
                OR,
                Qu(EXISTS, z, 
                  Op(
                    Not(Pred("p.qa", List(z))), 
                    AND, 
                    Pred("p", List(Var("v.a")))
                  )
                )
              )
            )
          )
        )

        shouldParse("Ax(Ey p(y) | Ez(!q(z) & p(x)))", Qu(FORALL, x, Op(
          Qu(EXISTS, y, Pred("p", List(y))),
          OR,
          Qu(EXISTS, z, Op(Not(Pred("q", List(z))), AND, Pred("p", List(x))))
        )))

        shouldParse("p(y) | Ez(-q(z) & p(x))", Op(
          Pred("p", List(y)), 
          OR, 
          Qu(EXISTS, z, Op(
            Not(Pred("q", List(z))), 
            AND, 
            Pred("p", List(x))
          ))
        ))
      }

    }
  }
}
