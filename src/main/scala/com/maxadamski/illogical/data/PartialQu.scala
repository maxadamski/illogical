package com.maxadamski.illogical

case class PartialQu(token: QuToken, variable: Var) {

  def complete(form: Form): Form = 
    Qu(token, variable, form)

  def isExistential =
    token.isExistential

  def isUniversal =
    token.isUniversal

}

