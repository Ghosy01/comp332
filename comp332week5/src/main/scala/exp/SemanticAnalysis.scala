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
            case IdnExp (i)      => false
            case IntExp (n)      => true
            case MinusExp (l, r) => isconst (l) && isconst (r)
            case PlusExp (l, r)  => isconst (l) && isconst (r)
            case StarExp (l, r)  => isconst (l) && isconst (r)
            case SlashExp (l, r) => isconst (l) && isconst (r)
			}

    /**
     * What is the value of an expression?  Only needs to be valid if the
     * expression is constant (see isconst above).
     */
    val expvalue : Expression => Int =
        attr {
              case IdnExp (i)      => 0   // undefined
            case IntExp (n)      => n
            case MinusExp (l, r) => expvalue (l) - expvalue (r)
            case PlusExp (l, r)  => expvalue (l) + expvalue (r)
            case StarExp (l, r)  => expvalue (l) * expvalue (r)
            case SlashExp (l, r) => expvalue (l) / expvalue (r)
        }

}
