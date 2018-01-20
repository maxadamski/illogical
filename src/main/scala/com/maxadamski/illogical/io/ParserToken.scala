package com.maxadamski.illogical

case class ParserToken(symbol: Symbol, value: String) {
  def matches(regex: String) = value.matches(regex)

  val notMatcher = raw"NOT|\~|\-|\!"
  val andMatcher = raw"AND|\&\&?"
  val orMatcher  = raw"OR|\|\|?"
  val existsMatcher = raw"E(X|XIST|XISTS)?"
  val forallMatcher = raw"(F|FOR)?A(LL)?"

  def isSep = symbol == 'sep
  def isOp  = symbol == 'op
  def isId  = symbol == 'id

  def isLeftBrace  = isSep && matches(raw"[\(\[\{]")
  def isRightBrace = isSep && matches(raw"[\)\]\}]")
  def isArgSep     = isSep && matches(raw"\,")

  def isEXISTS = isOp && matches(existsMatcher)
  def isFORALL = isOp && matches(forallMatcher)
  def isNOT    = isOp && matches(notMatcher)
  def isAND    = isOp && matches(andMatcher)
  def isOR     = isOp && matches(orMatcher)
  def isEQV    = isOp && matches(raw"EQV|IFF|\=\=\=|\<\>|\<\-\>")
  def isIMP    = isOp && matches(raw"IMP|THEN|\-\>|\>")
  def isNAND   = isOp && matches(s"(N|${notMatcher})(${andMatcher})")
  def isNOR    = isOp && matches(s"(N|${notMatcher})(${orMatcher})")
  def isXOR    = isOp && matches(s"X(${orMatcher})")

  def isCon  = isId && matches(raw"^@[a-z]+[0-9]*'*")
  val isVar  = isId && matches(raw"^[a-z]+[0-9]*'*")
  val isFunc = isId && matches(raw"^[a-z]+[0-9]*'*")
  val isPred = isId && matches(raw"^[a-z]+[0-9]*'*")

  def isNOTEXISTS: Boolean = isOp && matches(s"(${notMatcher})(${existsMatcher})")
  def isNOTFORALL: Boolean = isOp && matches(s"(${notMatcher})(${forallMatcher})")

  def opToken: Option[OpToken] = 
    if (isAND) Some(AND)
    else if (isOR) Some(OR)
    else if (isNAND) Some(NAND)
    else if (isNOR) Some(NOR)
    else if (isXOR) Some(XOR)
    else if (isIMP) Some(IMP)
    else if (isEQV) Some(EQV)
    else None

  def quToken: Option[QuToken] =
    if (isFORALL) Some(FORALL)
    else if (isEXISTS) Some(EXISTS)
    else None

}

