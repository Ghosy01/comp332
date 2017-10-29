/**
 * Symbol table for the expression language.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.bitbucket.inkytonik.kiama.util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating expression language symbol information.
 */
object SymbolTable extends Environments {

    // import ExpTree._
    import org.bitbucket.inkytonik.kiama.util.Entity

    /**
     * A variable entity.
     */
    case class Variable () extends Entity

}
