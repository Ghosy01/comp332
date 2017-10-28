/**
 * Expression language implementation main program.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

/**
 * Syntax analyse the expression language program in the file given as the
 * first command-line argument and process the source tree to describe
 * whether expression statements contain constant statements or not.
 */
object Main {

    import ExpTree._
    import java.io.FileNotFoundException
    // import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, layout}
    import org.bitbucket.inkytonik.kiama.parsing.Success
    import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions}

    def main (args : Array[String]) {

        args.size match {

            // If there is exactly one command-line argument
            case 1 =>
                try {
                    // Create a source for the argument file name
                    val source = new FileSource (args (0))

                    // Create a syntax analysis module
                    val positions = new Positions
                    val parsers = new SyntaxAnalysis (positions)

                    // Parse the file
                    parsers.parse (parsers.parser, source) match {

                        // If it worked, we get a source tree
                        case Success (sourcetree, _) =>
                            // Pretty print the source tree
                            // println (layout (any (sourcetree)))

                            // Process the source tree
                            process (sourcetree)

                        // Parsing failed, so report it
                        case f =>
                            println (f)

                    }
                } catch {
                    case e : FileNotFoundException =>
                        println (e.getMessage)
                }

            // Complain otherwise
            case _ =>
                println ("usage: run file.exp")

        }

    }

    /**
     * Process the source tree by finding all expression statements and
     * printing whether they are constant or not.  If one is constant, also
     * print its value.
     */
    def process (program : ExpProgram) {

        val tree = new ExpTree (program)
        val analysis = new SemanticAnalysis (tree)
        import analysis._

        for (s <- program.stmts)
            s match {
                case ExpStmt (e) =>
                    print (e + " is ")
                    if (!(isconst (e))) print ("not ")
                    print ("constant")
                    if (isconst (e))
                        print (" with value " + expvalue (e))
                    println
                case _ =>
                    // Do nothing
            }
    }

}
