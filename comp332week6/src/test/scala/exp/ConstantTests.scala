/**
 * Expression language constant tests.
 *
 * Copyright 2011-2017, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.bitbucket.inkytonik.kiama.util.Tests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the constant semantic analysis works correctly.  I.e.,
 * it correctly detects constant and non-constant expressions.  For constant
 * ones, it correctly calculates the value.
 */
@RunWith(classOf[JUnitRunner])
class ConstantTests extends Tests {

    import ExpTree._

    def makeSemanticAnalysis (exp : Expression) : SemanticAnalysis = {
        val tree = new ExpTree (ExpProgram (Vector (ExpStmt (exp))))
        new SemanticAnalysis (tree)
    }

    test ("a single integer expression is constant with the correct value") {
        val e = IntExp (10)
        val analysis = makeSemanticAnalysis (e)
        analysis.isconst (e) shouldBe true
        analysis.expvalue (e) shouldBe 10
    }

    test ("a single identifier is not constant") {
        val e = IdnExp (IdnUse ("total"))
        val analysis = makeSemanticAnalysis (e)
        analysis.isconst (e) shouldBe false
    }

    test ("an expression involving only integers is constant with the correct value") {
        val e = PlusExp (StarExp (IntExp (3), IntExp (4)), IntExp (5))
        val analysis = makeSemanticAnalysis (e)
        analysis.isconst (e) shouldBe true
        analysis.expvalue (e) shouldBe 17
    }

    test ("a non-trivial expression involving a variable is not constant") {
        val e = MinusExp (IdnExp (IdnUse ("a")), IntExp (0))
        val analysis = makeSemanticAnalysis (e)
        analysis.isconst (e) shouldBe false
    }

    test ("a complex expresison involving multiple variables is not constant") {
        val e = StarExp (PlusExp (IntExp (1), IdnExp (IdnUse ("a"))),
                         MinusExp (IdnExp (IdnUse ("b")), IntExp (3)))
        val analysis = makeSemanticAnalysis (e)
        analysis.isconst (e) shouldBe false
    }

    // another non-trivial test with operators that haven't been used before
    test ("an expression with other operators is constant with the correct value") {
        val e = SlashExp (IntExp (10), IntExp (2))
        val analysis = makeSemanticAnalysis (e)
        analysis.isconst (e) shouldBe true
        analysis.expvalue (e) shouldBe 5
    }

}
