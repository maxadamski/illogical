package com.maxadamski.illogical

object Lexer {
  val seps = raw"[\{\}\[\]\(\)\,\ ]"
  val ops = raw"[A-Z\!\&\|\<\>\-\~]"

  def mergeTokens(symbol: Symbol)(list: List[ParserToken], t: ParserToken) = (list, t) match {
    case (head :+ ParserToken(`symbol`, a), ParserToken(`symbol`, b)) => 
      list.updated(list.length - 1, ParserToken(symbol, a ++ b))
    case _ => 
      list :+ t
  }

  def tokenFromString(s: String) =
    if (s.matches(seps)) ParserToken('sep, s)
    else if (s.matches(ops)) ParserToken('op, s)
    else ParserToken('id, s)

  def tokens(equation: String) = equation
    .map(_.toString).map(tokenFromString)
    .foldLeft(List[ParserToken]())(mergeTokens('id))
    .foldLeft(List[ParserToken]())(mergeTokens('op))
    .filter(_ != ParserToken('sep, " "))

  def repr(tokens: List[ParserToken]) = tokens
    .map(t => t.value)
    .mkString("")
}

