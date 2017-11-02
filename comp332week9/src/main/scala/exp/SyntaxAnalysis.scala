/**
 * Expression language parser.
 *
 * Copyright 2009-2010, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for the expression language.
 */
class SyntaxAnalysis (positions : Positions) extends Parsers (positions) {

    import ExpTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[ExpProgram] =
        phrase (program)

    lazy val program : PackratParser[ExpProgram] =
        (statement+) ^^ ExpProgram

    lazy val statement : PackratParser[Statement] =
        ("set" ~> idnuse) ~ ("=" ~> expression) ^^
            { case i ~ e => SetStmt (IdnExp (i), e) } |
        ("if" ~> "(" ~> expression <~ ")") ~ ("{" ~> (statement+) <~ "}") ^^
            { case e ~ ss => IfStmt (e, ss) } |
        "decl" ~> idndef ~ (":" ~> tipe) ^^
            { case i ~ t => VarDecl (i, t) } |
        ("const" ~> idndef) ~ (":" ~> tipe) ~ ("=" ~> expression) ^^
            { case i ~ t ~ e => ConstDecl (i, t, e) } |
        expression ^^ ExpStmt

    lazy val tipe : Parser[Type] =
    	"integer" ^^^ IntType () |
        "string" ^^^ StrType ()

    lazy val expression : PackratParser[Expression] =
        expression ~ ("+" ~> term) ^^ { case e ~ t => PlusExp (e, t) } |
        expression ~ ("-" ~> term) ^^ { case e ~ t => MinusExp (e, t) } |
        term

    lazy val term : PackratParser[Expression] =
        term ~ ("*" ~> factor) ^^ { case t ~ f => StarExp (t, f) } |
        term ~ ("/" ~> factor) ^^ { case t ~ f => SlashExp (t, f) } |
        factor

    lazy val factor : PackratParser[Expression] =
        integer ^^ (s => IntExp (s.toInt)) |
        string ^^ StrExp |
        idnuse ^^ IdnExp

    lazy val integer : PackratParser[String] =
        regex ("[0-9]+".r)

    lazy val string : Parser[String] =
    	regex ("\"[^\"]*\"".r)

    lazy val idndef : PackratParser[IdnDef] =
        ident ^^ IdnDef

    lazy val idnuse : PackratParser[IdnUse] =
        ident ^^ IdnUse

    lazy val ident : PackratParser[String] =
        regex ("[a-zA-Z]+".r)

}
