/**
 * Expression language semantics analysis test support.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.bitbucket.inkytonik.kiama.util.ParseTests

/**
 * Support code for semantic test suites.
 */
class SemanticTests extends ParseTests {

    import ExpTree._
    import org.bitbucket.inkytonik.kiama.parsing.{Error, Failure, Success}
    import org.bitbucket.inkytonik.kiama.util.StringSource
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages

    val parsers = new SyntaxAnalysis (positions)
    import parsers._

    /**
     * Parse some test input and, if the parse succeeds with no input left,
     * return the program tree. If the parse fails, fail the test.
     */
    def parseProgram (str : String) : ExpProgram =
        parseAll (parser, StringSource (str)) match {
            case Success (r, in) =>
                if (!in.atEnd) fail ("input remaining at " + in.pos)
                r
            case f : Error =>
                fail ("parse error: " + f.message)
            case f : Failure =>
                fail ("parse failure: " + f.message)
        }

    /**
     * Parse some test input and run the name analyser over the resulting
     * tree (if the parse succeeds).
     */
    def semanticTest (str : String) : Messages = {
        val prog = parseProgram (str)
        val tree = new ExpTree (prog)
        val analysis = new SemanticAnalysis (tree)
        val messages = analysis.errors (prog)
        // println (messages)
        messages
    }

    /**
     * Assert that a message was produced at a given position.
     */
    def assertMessage (messages : Messages, index : Int, line : Int, column : Int, msg : String) {
        val m = messages (index)
        m.label shouldBe msg
        positions.getStart (m.value) match {
            case Some (posn) =>
                posn.line shouldBe line
                posn.column shouldBe column
            case _ =>
                fail ("no position for message value")
        }
    }

}
