/**
 * Expression language type analysis tests.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
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
class TypeTests extends SemanticTests {

    // If statements

    test ("can use an integer expression in an if condition") {
        val messages =
            semanticTest ("""
                |decl a : integer
                |if (a + 1) { set a = 2 - 4}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("can't use a string expression in an if condition") {
        val messages =
            semanticTest ("""
                |decl a : string
                |if (a) { set a = "foo" }
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 5, "type error: expected integer got string")
    }

    // Set statements

    test ("can assign an integer to an integer variable") {
        val messages =
            semanticTest ("""
                |decl a : integer
                |set a = 8 / 4
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("can't assign an integer to a string variable") {
        val messages =
            semanticTest ("""
                |decl a : string
                |set a = 3 * 2
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 9, "type error: expected string got integer")
    }

    test ("can't assign a string to an integer variable") {
        val messages =
            semanticTest ("""
                |decl a : integer
                |set a = "foo"
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 9, "type error: expected integer got string")
    }

    // Constant declarations

    test ("a constant integer can be assigned an integer expression") {
        val messages =
            semanticTest ("""
                |const a : integer = 3 + 4 * 5
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("a constant string can be assigned a string expression") {
        val messages =
            semanticTest ("""
                |const a : string = "hello"
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("a constant integer can't be assigned a string expression") {
        val messages =
            semanticTest ("""
                |const a : integer = "hello"
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 2, 21, "type error: expected integer got string")
    }

    test ("a constant string can't be assigned an integer expression") {
        val messages =
            semanticTest ("""
                |const a : string = 3 + 4 * 5
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 2, 20, "type error: expected string got integer")
    }

}
