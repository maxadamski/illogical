package com.maxadamski.illogical

trait LogicLaws {

  val converse: Form => Option[Form] = { 
    case Op(a, token, b) => Some(Op(b, token, a))
    case _ => None
  }

  val inverse: Form => Option[Form] = {
    case Op(a, token, b) => Some(Op(Not(a), token, Not(b)))
    case _ => None
  }

  val expand_not_quantifier: Form => Option[Form] = {
    case Not(Qu(FORALL, v, p)) => Some(Qu(EXISTS, v, Not(p)))
    case Not(Qu(EXISTS, v, p)) => Some(Qu(FORALL, v, Not(p)))
    case _ => None
  }

  val expand_de_morgan: Form => Option[Form] = {
    case Not(Op(a, AND, b)) => Some(Op(Not(a), OR, Not(b)))
    case Not(Op(a, OR, b)) => Some(Op(Not(a), AND, Not(b)))
    case _ => None
  }

  val expand_not_not: Form => Option[Form] = {
    case Not(Not(a)) => Some(a)
    case _ => None
  }

  val expand_imp: Form => Option[Form] = { 
    case Op(a, IMP, b) => Some(Op(Not(a), OR, b))
    case _ => None
  }

  val expand_eqv: Form => Option[Form] = { 
    case Op(a, EQV, b) => Some(Not(Op(a, XOR, b)))
    case _ => None
  }

  val expand_nand: Form => Option[Form] = { 
    case Op(a, NAND,b) => Some(Not(Op(a, AND, b)))
    case _ => None
  }

  val expand_nor: Form => Option[Form] = { 
    case Op(a, NOR, b) => Some(Not(Op(a, OR, b)))
    case _ => None
  }

  val expand_xor: Form => Option[Form] = { 
    case Op(a, XOR, b) => Some(Op(Op(a, OR, b), AND, Op(a, NAND, b)))
    case _ => None
  }

}
