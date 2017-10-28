/**
 * Semantic analysis for the Expression language.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import ExpTree.ExpTree
import org.bitbucket.inkytonik.kiama.attribution.Attribution

class SemanticAnalysis (tree : ExpTree) extends Attribution {

    import ExpTree._

    /**
     * Is an expression constant?
     */
    val isconst : Expression => Boolean =
        attr {
            case _ => true
        }

    /**
     * What is the value of an expression?  Only needs to be valid if the
     * expression is constant (see isconst above).
     */
    val expvalue : Expression => Int =
        attr {
            case _ => 0
        }

}
