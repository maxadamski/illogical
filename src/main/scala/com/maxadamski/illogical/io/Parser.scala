package com.maxadamski.illogical

object Parser {

  // Convenience initializers

  def makeFunc(name: String, args: List[Term]): Option[Func] = 
    if (!args.isEmpty) Some(Func(name, args)) else None

  def makePred(name: String, args: List[Term]): Option[Pred] = 
    if (!args.isEmpty) Some(Pred(name, args)) else None

  def makeQu(t: Option[QuToken], v: Option[Var], p: Option[Form]): Option[Qu] =
    for { t <- t; v <- v; p <- p } yield Qu(t, v, p)

  def makeOp(p: Option[Form], t: Option[OpToken], q: Option[Form]): Option[Op] =
    for { p <- p; t <- t; q <- q } yield Op(p, t, q)

  def makeNot(p: Option[Form]): Option[Not] =
    for { p <- p } yield Not(p)

  // Node parsers

  def isBalanced(ts: List[ParserToken]) = 
    ts.count(_.isLeftBrace) == ts.count(_.isRightBrace)

  def unbraced(ts: List[ParserToken]): Option[List[ParserToken]] = ts match {
    case lbrace +: tokens :+ rbrace if lbrace.isLeftBrace && rbrace.isRightBrace && isBalanced(tokens) => 
      Some(tokens)
    case _ => None
  }

  def getTerm(ts: List[ParserToken], in: Symbol): Option[Term] = unbraced(ts) match {
    case Some(x) =>
      //println("unbracing term")
      getFunc(ts, in) orElse getVar(ts, in) orElse getCon(ts, in) orElse getTerm(x, in)
    case None =>
      //println("get term")
      getFunc(ts, in) orElse getVar(ts, in) orElse getCon(ts, in)
  }

  def getForm(ts: List[ParserToken]): Option[Form] = unbraced(ts) match {
    case Some(x) =>
      //println("unbracing form")
      getOp(ts) orElse getNot(ts) orElse getQu(ts) orElse getPred(ts, 'form) orElse getForm(x)
    case None =>
      //println("get form")
      //println(Lexer.repr(ts))
      getOp(ts) orElse getNot(ts) orElse getQu(ts) orElse getPred(ts, 'form)
  }

  // Term parsers

  def getCon(ts: List[ParserToken], in: Symbol): Option[Con] = ts match {
    case name :: Nil if name.isCon => Some(Con(name.value.drop(1)))
    case _ => None
  }

  def getVar(ts: List[ParserToken], in: Symbol): Option[Var] = ts match {
    case name :: Nil if (List('qu, 'pred, 'func) contains in) && name.isVar => Some(Var(name.value))
    case _ => None
  }

  def getFunc(ts: List[ParserToken], in: Symbol): Option[Term] = ts match {
    case name +: lbrace +: tokens :+ rbrace if (List('pred, 'func) contains in) && lbrace.isLeftBrace && rbrace.isRightBrace => 
      //println("get func")
      makeFunc(name.value, getArgs(tokens, 'func))
    case _ => 
      //println("could not get func")
      None
  }

  // Form parsers

  def getPred(ts: List[ParserToken], in: Symbol): Option[Form] = ts match {
    case name +: lbrace +: tokens :+ rbrace if name.isPred && lbrace.isLeftBrace && rbrace.isRightBrace => 
      //println("get pred")
      makePred(name.value, getArgs(tokens, 'pred))
    case _ => 
      //println("could not get pred")
      None
  }

  def getNot(ts: List[ParserToken]): Option[Form] = ts match {
    case head +: lbrace +: tokens :+ rbrace if head.isNOT && lbrace.isLeftBrace && rbrace.isRightBrace =>
      makeNot(getForm(tokens))
    case head :: tail if head.isNOT => 
      //println("get not")
      makeNot(getForm(tail))
    // UGLY HACK: negation and quantifier operator is lexed as one token
    case head :: tail if head.isNOTEXISTS => 
      makeNot(getQu(ParserToken('op, "E") +: tail))
    case head :: tail if head.isNOTFORALL => 
      makeNot(getQu(ParserToken('op, "A") +: tail))
    case _ =>
      //println("could not get not")
      None
  }

  def getQu(ts: List[ParserToken]): Option[Form] = ts match {
    case t +: v +: tokens if t.isOp && v.isVar =>
      //println("get qu")
      makeQu(t.quToken, getVar(List(v), 'qu), getForm(tokens))
    case _ =>
      //println("could not get qu")
      None
  }

  def nestedArgs(ts: List[ParserToken]): (List[ParserToken], List[ParserToken]) = {
    // TODO: clean up this mess...
    val (name, lbrace, tokens) = (ts(0), ts(1), ts.drop(2))
    var level = -1
    var i = -1

    do {
      i += 1
      val token = tokens(i)
      if (token.isLeftBrace) level -= 1
      if (token.isRightBrace) level += 1
    } while (level != 0 && i < tokens.length)

    if (level == 0) {
      val inside = name +: lbrace +: tokens.slice(0, i + 1)
      val slice = tokens.slice(i + 2, tokens.length)
      (inside, slice)
      
    } else {
      (List(), List())
    }
  }

  def getArgs(token: ParserToken, in: Symbol): List[Term] = 
    List(getTerm(List(token), in)).flatten

  def getArgs(ts: List[ParserToken], in: Symbol): List[Term] = ts match {
    case name :: Nil => 
      //println("last arg")
      //println(Lexer.repr(ts))
      getArgs(name, in)
    case name +: argsep +: tokens if name.isId && argsep.isArgSep => 
      //println("next arg")
      //println(Lexer.repr(ts))
      getArgs(name, in) ++ getArgs(tokens, in)
    case name +: lbrace +: tokens if name.isId && lbrace.isLeftBrace => 
      val (current, next) = nestedArgs(ts)
      //println("big arg")
      //println(Lexer.repr(current))
      List(getTerm(current, in)).flatten ++ getArgs(next, in)
    case _ => List()
  }

  def isOpToken(t: ParserToken) = t.opToken match { case Some(_) => true; case _ => false }


  def opSplitIndex(ts: List[ParserToken]): Int = {
    var split = -1
    var level = 0
    var i = ts.length - 1

    while (split == -1 && i >= 0) {
        val token = ts(i)
        if (token.isLeftBrace) level -= 1
        if (token.isRightBrace) level += 1
        if (level == 0 && isOpToken(token)) split = i
        i -= 1
    }

    split
  }

  def getOp(ts: List[ParserToken]): Option[Form] = {
    val i = opSplitIndex(ts)
    if (i == -1) return None
    var (p, q) = ts.splitAt(i)
    q = q.drop(1)
    //println(s"get op ${Lexer.repr(p)} ${ts(i).opToken} ${Lexer.repr(q)}")
    makeOp(getForm(p), ts(i).opToken, getForm(q))
  }

  def parse(equation: String): Option[Form] = {
    var tokens = Lexer.tokens(equation)
    getForm(tokens)
  }
}

