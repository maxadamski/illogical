package com.maxadamski.illogical

import org.scalatest._

class FormSpec extends UnitSpec {
  describe("Form") {

    describe("simplification") {

      describe("student questions") {

        itShouldSimplify("Ax[p(x) -> Ey q(y)]", 
          "Ax[!p(x) | Ey q(y)]")

        itShouldSimplify("AxAy{Ex p(x) & Ev[q(x,v) -> Ev q(y, v)]}", 
          "AxAy{Ex p(x) & Ev[!q(x,v) | Ev q(y, v)]}")

        itShouldSimplify("Ex[!Ey p(y) -> Ez(q(z) -> r(x))]", 
          "Ex{ Ey p(y) | Ez[!q(z) | r(x)]}")

        itShouldSimplify("{Ax[p(x) -> p(x,x)]} -> p(f(x), @a)", 
          "Ex[p(x) & !p(x,x)] | p(f(x),@a)")

        itShouldSimplify("Ay{[Ax(p(x) -> Ex q(x))] -> [Ax p(x,y) -> Ax q(x)]}", 
          "Ay{[Ex(p(x) & Ax !q(x))] | [Ex !p(x,y) | Ax q(x)]}")

        itShouldSimplify("(Ex p(x, @a)) XOR (AxEy q(x, y))", 
          "[Ex p(x,@a) | AxEy q(x,y)] & [Ax !p(x,@a) | ExAy !q(x,y)]")

      }

    }

    describe("prenex normal form") {

      describe("trivial cases") {

        itShouldPNF("AxAxAxAx p(x)", 
          "AxAx'Ax''Ax''' p(x''')")

        itShouldPNF("Ax[AxAx p(x) | ExEx q(x)]", 
          "AxAx'Ax''Ex'''Ex''''[p(x'') | q(x'''')]")

        itShouldPNF("!Ax !q(x)",
          "Ex q(x)")

        itShouldPNF("Ex ![!p(x) | Ax !q(x)]",
          "ExEx' [p(x) & q(x')]")
      }

      describe("student questions") {

        itShouldPNF("Ax[p(x) -> Ey q(y)]", 
          "AxEy[!p(x) | q(y)]")

        itShouldPNF("AxAy{Ex p(x) & Ev[q(x,v) -> Ev q(y, v)]}", 
          "AxAyEx'EvEv'{p(x') & [!q(x,v) | q(y, v')]}")

        itShouldPNF("Ex[!Ey p(y) -> Ez(q(z) -> r(x))]", 
          "ExEyEz[p(y) | (!q(z) | r(x))]")

        itShouldPNF("{Ax[p(x) -> p(x,x)]} -> p(f(x), @a)", 
          "Ex{ p(f(x),@a) | [p(x) & !p(x,x)] }")

        itShouldPNF("Ay{[Ax(p(x) -> Ex q(x))] -> [Ax p(x,y) -> Ax q(x)]}", 
          "AyExAx'Ex''Ax'''{[p(x) & !q(x')] | [!p(x'', y) | q(x''')]}")

        itShouldPNF("(Ex p(x, @a)) XOR (AxEy q(x, y))", 
          "ExAx'EyAx''Ex'''Ay'{[p(x,@a) | q(x',y)] & [!p(x'',@a) | !q(x''',y')]}")

      }

    }

    describe("conjuncitive normal form") {

      describe("student questions") {

        itShouldCNF("Ax[p(x) -> Ey q(y)]", 
          "AxEy[!p(x) | q(y)]")

        itShouldCNF("AxAy{Ex p(x) & Ev[q(x,v) -> Ev q(y, v)]}", 
          "AxAyEx'EvEv'{p(x') & [!q(x,v) | q(y, v')]}")

        itShouldCNF("Ex[!Ey p(y) -> Ez(q(z) -> r(x))]", 
          "ExEyEz[p(y) | (!q(z) | r(x))]")

        itShouldCNF("{Ax[p(x) -> p(x,x)]} -> p(f(x), @a)", 
          "Ex{ [ p(f(x),@a) | p(x) ] & [ p(f(x),@a) | !p(x,x) ] }")

        itShouldCNF("Ay{[Ax(p(x) -> Ex q(x))] -> [Ax p(x,y) -> Ax q(x)]}", 
          "AyExAx'Ex''Ax'''{{p(x) | [!p(x'', y) | q(x''')]} & {!q(x') | [!p(x'', y) | q(x''')]}}")

        itShouldCNF("(Ex p(x, @a)) XOR (AxEy q(x, y))", 
          "ExAx'EyAx''Ex'''Ay'{[p(x,@a) | q(x',y)] & [!p(x'',@a) | !q(x''',y')]}")

      }
      
    }

    describe("skolemization") {

      describe("trivial cases") {

        itShouldSkolemize("p(x)", 
          "p(x)")

        itShouldSkolemize("AxAxAxAx p(x)", 
          "AxAx'Ax''Ax''' p(x''')")

        itShouldSkolemize("ExExExEx p(x)", 
          "p(@k4)")

        itShouldSkolemize("AxExAxEx p(x)", 
          "AxAx'' p(s2(x, x''))")

        itShouldSkolemize("p(x) -> Ey q(y)", 
          "!p(x) | q(@k1)")
      }

      describe("student questions") {

        itShouldSkolemize("Ax[p(x) -> Ey q(y)]", 
          "Ax[!p(x) | q(s1(x))]")

        itShouldSkolemize("AxAy{Ex p(x) & Ev[q(x,v) -> Ev q(y, v)]}", 
          "AxAy{p(s1(x,y)) & [!q(x,s2(x,y)) | q(y,s3(x,y))]}")

        itShouldSkolemize("Ex[!Ey p(y) -> Ez(q(z) -> r(x))]", 
          "p(@k2) | (!q(@k3) | r(@k1))")

        itShouldSkolemize("{Ax[p(x) -> p(x,x)]} -> p(f(x), @a)", 
          "[p(f(@k1),@a) | p(@k1)] & [p(f(@k1),@a) | !p(@k1,@k1)]")

        itShouldSkolemize("Ex{Ax[p(x) -> p(x,x)] -> p(f(x), @a)}", 
          "[p(f(@k1),@a) | p(@k2)] & [p(f(@k1),@a) | !p(@k2,@k2)]")

        itShouldSkolemize("Ay{[Ax(p(x) -> Ex q(x))] -> [Ax p(x,y) -> Ax q(x)]}", 
          "AyAx'Ax'''({[!p(s2(y,x'), y) | q(x''')] | p(s1(y))} & {[!p(s2(y,x'), y) | q(x''')] | !q(x')})")

        itShouldSkolemize("(Ex p(x, @a)) XOR (AxEy q(x, y))", 
          "Ax'Ax''Ay'{[p(@k1,@a) | q(x',s1(x'))] & [!p(x'',@a) | !q(s2(x',x''),y')]}")

      }
    }


    describe("most general unifier") {

      describe("student questions") {

        itShouldMGU("p(@a, x, f(g(y)))", "p(y, f(z), f(z))", 
          Set(Sub(Var("x"), Func("f", List(Func("g", List(Con("a")))))), Sub(Var("y"), Con("a")), Sub(Var("z"), Func("g", List(Con("a"))))))

        itShouldMGU("p(@a, x, f(g(y)))", "p(z, h(z,u), f(u))", 
          Set(Sub(Var("z"), Con("a")), Sub(Var("u"), Func("g", List(Var("y")))), Sub(Var("x"), Func("h", List(Con("a"), Func("g", List(Var("y"))))))))

        itShouldNotMGU("p(x,g(f(@a)),f(x))", "p(f(@a),y,y)")

        itShouldNotMGU("p(x,g(f(@a)),f(x))", "p(f(y),z,y)")

        itShouldNotMGU("p(x,f(x))", "p(f(y),y)")

      }

    }

    def itShouldGetClauses(form: String, expected: Set[String]) {
      it(s"should get clauses '$form'") {
        val f = Parser.parse(form)
        val e = expected.map(Parser.parse)
        e.foreach(clause => clause should not equal None)
        f should not equal None
        f.get.clauses shouldEqual e.flatten
      }
    }

    describe("extracting clauses") {

      itShouldGetClauses("p(x)", Set("p(x)"))

      itShouldGetClauses("p(x) | q(x)", Set("p(x) | q(x)"))

      itShouldGetClauses("p(x) | !q(x) | r(x)", Set("p(x) | !q(x) | r(x)"))

      itShouldGetClauses("p(x) & q(x) & r(x)", Set("p(x)", "q(x)", "r(x)"))

      itShouldGetClauses("(p(x) | q(x)) & !p(x)", Set("p(x) | q(x)", "!p(x)"))

      itShouldGetClauses("(p(x) | q(x)) & (p(x) | r(x))", Set("p(x) | q(x)", "p(x) | r(x)"))

      // maybe this should return an empty set?
      itShouldGetClauses("Ax(p(x) | q(x)) & !p(x)", Set("!p(x)"))
    }


    describe("resolution") {
      
      describe("student questions") {

      }

    }

  }

}
