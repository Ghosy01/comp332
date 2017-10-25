/**
 * Expression language tests.
 *
 * Copyright 2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

    import ExpTree._

    val parsers = new SyntaxAnalysis (positions)
    import parsers._

    test ("parsing digits as a factor gives the correct tree") {
        factor ("1203") should parseTo[Expression] (IntExp (1203))
    }

    test ("parsing a non-integer as an integer gives an error") {
        integer ("hello") should failParseAt (1, 1, "string matching regex '[0-9]+' expected but 'h' found")
    }

    test ("parsing a simple expression produces the correct tree") {
        expression ("2 + 4") should parseTo[Expression] (PlusExp (IntExp (2), IntExp (4)))
    }

    test ("parsing an expression with associative operators produces the correct tree") {
        expression ("2 + 4 * 6") should parseTo[Expression] (PlusExp (IntExp (2), StarExp (IntExp (4), IntExp (6))))
    }
	
	test ("making sure minus + plus operators work"){
		expression ("2-3") should parseTo[Expression] (MinusExp (IntExp(2),IntExp(3)))
	
	}

    // FIXME: MORE TESTS SHOULD GO HERE

}
