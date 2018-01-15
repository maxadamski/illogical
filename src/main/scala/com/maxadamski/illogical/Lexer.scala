package com.maxadamski.illogical

object Lexer {
  val seps = raw"[\{\}\[\]\(\)\,\ ]"
  val ops = raw"[A-Z\!\&\|\<\>\-]"

  def mergeTokens(symbol: Symbol)(list: List[Token], t: Token) = (list, t) match {
    case (head :+ Token(`symbol`, a), Token(`symbol`, b)) => 
      list.updated(list.length - 1, Token(symbol, a ++ b))
    case _ => 
      list :+ t
  }

  def tokenFromString(s: String) =
    if (s.matches(seps)) Token('sep, s)
    else if (s.matches(ops)) Token('op, s)
    else Token('id, s)

  def tokens(equation: String) = equation
    .map(_.toString).map(tokenFromString)
    .foldLeft(List[Token]())(mergeTokens('id))
    .foldLeft(List[Token]())(mergeTokens('op))
    .filter(_ != Token('sep, " "))

  def repr(tokens: List[Token]) = tokens
    .map(t => t.value)
    .mkString("")
}

