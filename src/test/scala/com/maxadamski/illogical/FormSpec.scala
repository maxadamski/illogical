package com.maxadamski.illogical

import org.scalatest._

class FormSpec extends UnitSpec {
  describe("Form") {


    describe("pnf") {

      itShouldPNF("Ax[p(x) -> Ey q(y)]", "AxEy[!p(x) | q(y)]")

      itShouldPNF("AxAy[Ex p(x) & Ev(q(x, v) -> Ev q(y, v))]", "AxAyEx'EvEv'[p(x') & (!q(x,v) | q(y, v'))]")
      itShouldPNF("Ex[!Ey p(y) -> Ez(q(z) -> r(x))]", "ExEyEz[p(y) | (!q(z) | r(x))]")
      //itShouldPNF("[Ax(p(x) -> p(x, x))] -> p(f(x), a)", "Ex[ ( p(f(x),a) | p(x) ) & ( p(f(x),a) |  !p(x,x) ) ]")
      //itShouldPNF("Ay[(Ax(p(x) -> Ex q(x))) -> (Ax p(x, y) -> Ax q(x))]", "AyExEx'Ex''Ax'''[ ((!p(x, y) | q(x)) | p(x)) & ((!p(x, y) | q(x)) | !q(x))]")
      //itShouldPNF("(Ex p(x, a)) XOR (AxEy q(x, y))", "?err(x)")
      itShouldPNF("AxAxAxAx p(x)", "AxAx'Ax''Ax''' p(x''')")
      itShouldPNF("Ax[AxAx p(x) | ExEx q(x)]", "AxAx'Ax''Ex'''Ex''''[p(x'') | q(x'''')]")
    }

    describe("skolemization") {
      //itShouldSkolemize("Ax[p(x) -> Ey q(y)]", Qu(FORALL, Var("x"), Op(Not(Pred("p", List(Var("x")))), OR, Pred("q", List(Func("s2", List(Var("x"))))))))
      //itShouldSkolemize("p(x) -> Ey q(y)", Op(Not(Pred("p", List(Var("x")))), OR, Pred("q", List(Con("k")))))
    }

    describe("mgu") {
      itShouldMGU("p(a, x, f(g(y)))", "p(y, f(z), f(z))", Set(Sub(Var("x"), Func("f", List(Func("g", List(Con("a")))))), Sub(Var("y"), Con("a")), Sub(Var("z"), Func("g", List(Con("a"))))))
      itShouldMGU("p(a, x, f(g(y)))", "p(z, h(z,u), f(u))", Set(Sub(Var("z"), Con("a")), Sub(Var("u"), Func("g", List(Var("y")))), Sub(Var("x"), Func("h", List(Con("a"), Func("g", List(Var("y"))))))))
      itShouldNotMGU("p(x,g(f(a)),f(x))", "p(f(a),y,y)")
      itShouldNotMGU("p(x,g(f(a)),f(x))", "p(f(y),z,y)")
      itShouldNotMGU("p(x,f(x))", "p(f(y),y)")
    }

      // 4.
      //
      // p(a, x, f(g(y))); p(y, f(z), f(z))
      // p(x, g(f(a)), f(x)); p(f(a), y, y)
      // p(x, g(f(a)), f(x)); p(f(y), z, y)
      // p(a, x, f(g(y))); p(z, h(z, u), f(u))
      // p(x, f(x)); p(f(y), y)
      //


      //println(TextFormatter.formatted(Parser.parse("(p(x) & q(x)) | r(x)").get.conlit))


  }
}
