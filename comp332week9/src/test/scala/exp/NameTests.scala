/**
 * Expression language name analysis tests.
 *
 * Copyright 2012-2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the name analysis works correctly.  I.e., we run
 * the checking process over some input and make sure that the expected
 * errors were produced (and only them).
 */
@RunWith(classOf[JUnitRunner])
class NameAnalysisTests extends SemanticTests {

    test ("can't have two variables with same name") {
        val messages =
            semanticTest ("""
                |decl v : integer
                |decl v : integer
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 6, "v is declared more than once")
    }

    test ("can't have a constant and a variable with same name") {
        val messages =
            semanticTest ("""
                |const c : integer = 10
                |decl c : integer
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 6, "c is declared more than once")
    }

    test ("can't set a constant") {
        val messages =
            semanticTest ("""
                |const c : integer = 10
                |set c = 11
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 5, "c is not variable, so cannot be set")
    }

    test ("can set a variable") {
        val messages = semanticTest ("""
                |decl v : integer
                |set v = 11
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("constant initialisation expressions must be constant") {
        val messages = semanticTest ("""
                |decl v : integer
                |const c : integer = v + 1
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 21, "initialising expression is not constant")
    }

}
