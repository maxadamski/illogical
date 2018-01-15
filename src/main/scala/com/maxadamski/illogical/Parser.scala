package com.maxadamski.illogical

object Parser {

  // Convenience initializers

  val makeFunc: (String, List[Term]) => Option[Func] = {
    case (name, args) if !args.isEmpty => Some(Func(name, args))
    case _ => None
  }

  val makePred: (String, List[Term]) => Option[Pred] = {
    case (name, args) if !args.isEmpty => Some(Pred(name, args))
    case _ => None
  }

  val makeQu: (Option[QuToken], Option[Var], Option[Form]) => Option[Qu] = {
    case (Some(t), Some(v), Some(p)) => Some(Qu(t, v, p))
    case _ => None
  }

  val makeOp: (Option[Form], Option[OpToken], Option[Form]) => Option[Op] = {
    case (Some(p), Some(t), Some(q)) => Some(Op(p, t, q))
    case _ => None
  }

  val makeNot: Option[Form] => Option[Not] = {
    case (Some(p)) => Some(Not(p))
    case _ => None
  }

  // Node parsers

  def isBalanced(ts: List[Token]) = ts.count(_.isLeftBrace) == ts.count(_.isRightBrace)

  def unbraced(ts: List[Token]): Option[List[Token]] = ts match {
    case lbrace +: tokens :+ rbrace if lbrace.isLeftBrace && rbrace.isRightBrace && isBalanced(tokens) => 
      Some(tokens)
    case _ => None
  }

  def getTerm(ts: List[Token]): Option[Term] = unbraced(ts) match {
    case Some(x) =>
      //println("unbracing term")
      getFunc(ts) orElse getVar(ts) orElse getCon(ts) orElse getTerm(x)
    case None =>
      //println("get term")
      getFunc(ts) orElse getVar(ts) orElse getCon(ts)
  }

  def getForm(ts: List[Token]): Option[Form] = unbraced(ts) match {
    case Some(x) =>
      //println("unbracing form")
      getOp(ts) orElse getQu(ts) orElse getNot(ts) orElse getPred(ts) orElse getForm(x)
    case None =>
      //println("get form")
      //println(Lexer.repr(ts))
      getOp(ts) orElse getQu(ts) orElse getNot(ts) orElse getPred(ts)
  }

  // Term parsers

  def getCon(ts: List[Token]): Option[Con] = ts match {
    case name :: Nil if name.isCon => Some(Con(name.value))
    case _ => None
  }

  def getVar(ts: List[Token]): Option[Var] = ts match {
    case name :: Nil if name.isVar => Some(Var(name.value))
    case _ => None
  }

  def getFunc(ts: List[Token]): Option[Term] = ts match {
    case name +: lbrace +: tokens :+ rbrace if name.isFunc && lbrace.isLeftBrace && rbrace.isRightBrace => 
      //println("get func")
      makeFunc(name.value, getArgs(tokens))
    case _ => 
      //println("could not get func")
      None
  }

  // Form parsers

  def getPred(ts: List[Token]): Option[Form] = ts match {
    case name +: lbrace +: tokens :+ rbrace if name.isPred && lbrace.isLeftBrace && rbrace.isRightBrace => 
      //println("get pred")
      makePred(name.value, getArgs(tokens))
    case _ => 
      //println("could not get pred")
      None
  }

  def getNot(ts: List[Token]): Option[Form] = ts match {
    case head +: lbrace +: tokens :+ rbrace if head.isNOT && lbrace.isLeftBrace && rbrace.isRightBrace =>
      makeNot(getForm(tokens))
    case head :: tail if head.isNOT => 
      //println("get not")
      makeNot(getQu(tail) orElse getPred(tail))
    case _ =>
      //println("could not get not")
      None
  }

  def getQu(ts: List[Token]): Option[Form] = ts match {
    case t +: v +: tokens if t.isOp && v.isVar =>
      //println("get qu")
      makeQu(t.quToken, getVar(List(v)), getForm(tokens))
    case _ =>
      //println("could not get qu")
      None
  }

  def nestedArgs(ts: List[Token]): (List[Token], List[Token]) = {
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

  def getArgs(token: Token): List[Term] = 
    List(getTerm(List(token))).flatten

  def getArgs(ts: List[Token]): List[Term] = ts match {
    case name :: Nil => 
      //println("last arg")
      //println(Lexer.repr(ts))
      getArgs(name)
    case name +: argsep +: tokens if name.isId && argsep.isArgSep => 
      //println("next arg")
      //println(Lexer.repr(ts))
      getArgs(name) ++ getArgs(tokens)
    case name +: lbrace +: tokens if name.isId && lbrace.isLeftBrace => 
      val (current, next) = nestedArgs(ts)
      //println("big arg")
      //println(Lexer.repr(current))
      List(getTerm(current)).flatten ++ getArgs(next)
    case _ => List()
  }

  def isOpToken(t: Token) = t.opToken match { case Some(_) => true; case _ => false }


  def opSplitIndex(ts: List[Token]): Int = {
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

  def getOp(ts: List[Token]): Option[Form] = {
    val i = opSplitIndex(ts)
    if (i == -1) return None
    var (p, q) = ts.splitAt(i)
    q = q.drop(1)
    //println(s"get op ${Lexer.repr(p)} ${ts(i).opToken} ${Lexer.repr(q)}")
    makeOp(getForm(p), ts(i).opToken, getForm(q))
  }

  def parse(equation: String): Option[Form] = {
    var tokens = Lexer.tokens(equation)
    //println("start parsing " + equation)
    //println(Lexer.repr(tokens))
    getForm(tokens)
  }
}

