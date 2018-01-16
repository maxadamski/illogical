package com.maxadamski.illogical

import org.scalatest._

class FormSpec extends UnitSpec {
  describe("Form") {

    it("") {

      // 4.
      //
      // p(a, x, f(g(y))); p(y, f(z), f(z))
      // p(x, g(f(a)), f(x)); p(f(a), y, y)
      // p(x, g(f(a)), f(x)); p(f(y), z, y)
      // p(a, x, f(g(y))); p(z, h(z, u), f(u))
      // p(x, f(x)); p(f(y), y)
      //
      // Ax (p(x) -> Ey q(y))
      // AxAy(Ex p(x) & Ev(q(x, v) -> Ev q(y, v)))
      // Ex(!Ey p(y) -> Ez(q(z) -> r(x)))
      // [Ax(p(x) -> p(x, x))] -> p(f(x), a)
      // Ay[(Ax(p(x) -> Ex q(x))) -> (Ax p(x, y) -> Ax q(x))]
      // (Ex p(x, a)) XOR (AxEy q(x, y))


      //println(TextFormatter.formatted(Parser.parse("(p(x) & q(x)) | r(x)").get.conlit))

    }

  }
}
