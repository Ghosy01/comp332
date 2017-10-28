/**
 * Expression language constant tests.
 *
 * Copyright 2011, Anthony Sloane, Macquarie University, All rights reserved.
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
        val e = IdnExp ("total")
        val analysis = makeSemanticAnalysis (e)
        analysis.isconst (e) shouldBe false
    }

    test ("an expression involving only integers is constant with the correct value") {
        val e = PlusExp (StarExp (IntExp (3), IntExp (4)), IntExp (5))
        val analysis = makeSemanticAnalysis (e)
        analysis.isconst (e) shouldBe true
        analysis.expvalue (e) shouldBe 17
    }

    // FIXME: more tests needed here...

}
