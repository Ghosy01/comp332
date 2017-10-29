/**
 * Semantic analysis for the Expression language.
 *
 * Copyright 2009-2017, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import ExpTree.ExpTree
import org.bitbucket.inkytonik.kiama.attribution.Attribution

class SemanticAnalysis (tree : ExpTree) extends Attribution {
    
    import ExpTree._
    import SymbolTable._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.collectall
    import org.bitbucket.inkytonik.kiama.util.{Entity, MultipleEntity, UnknownEntity}
    import org.bitbucket.inkytonik.kiama.util.Messaging.message

    /**
     * Collect the semantic error messages for a given tree.
     */
    val errors =
        attr (collectall {
            case d @ IdnDef (i) if entity (d) == MultipleEntity () =>
                message (d, i + " is declared more than once")

            case u @ IdnUse (i) if entity (u) == UnknownEntity () =>
                message (u, i + " is not declared")
        })

    /**
     * The environment containing all bindings visible at a particular
     * node in the tree, not including any that are defined at that node.
     */
    val envin : ExpNode => Environment =
        attr {

            // If we are at the program node (root of tree) then the
            // environment in is an empty root environment.
            case p : ExpProgram =>
                rootenv ()

            // If we are at a statement that is not the first in a
            // statement sequence then the environment in is the same
            // as the environment out of the previous statement.
            case tree.prev.pair (s : Statement, p) =>
                env (p)

            // Otherwise, the environment in is our parent's environment in.
            case tree.parent.pair (n, p) =>
                envin (p)

        }

    /**
     * The environment containing all bindings visible "after" a
     * particular node in the tree.  I.e., it's the environment at the
     * node plus any new bindings introduced by the node.
     */
    val env : ExpNode => Environment =
        attr {
            case VarDecl (n @ IdnDef (i)) =>
                define (envin (n), i, entity (n))
            case n =>
                envin (n)
        }

    /**
     * The program entity referred to by an identifier definition or use.  In
     * the case of a definition it's the thing being defined, so define it to
     * be a reference to a new entity that represents that thing.  If it's
     * already defined, return an entity that indicates a multiple definition.
     * In the case of a use, it's the thing defined elsewhere that is being
     * referred to here, so look it up in the environment, using an unknown
     * entity if the environment does not already contain a binding.
     */
    val entity : IdnNode => Entity =
        attr {
            case n @ IdnDef (i) =>
                if (isDefinedInEnv (envin (n), i))
                    MultipleEntity ()
                else
                    n match {
                        case tree.parent (p : VarDecl) =>
                            Variable ()
                        case _ =>
                            UnknownEntity ()
                    }
            case n @ IdnUse (i) =>
                lookup (env (n), i, UnknownEntity ())
        }

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
