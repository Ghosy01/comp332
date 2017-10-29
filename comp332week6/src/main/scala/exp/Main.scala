/**
 * Expression language implementation main program.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

/**
 * Syntax analyse the expression language program in the file given as the
 * first command-line argument and process the source tree to perform naming
 * semantic analysis checks.
 */
object Main {

    import ExpTree._
    import java.io.FileNotFoundException
    // import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, layout}
    import org.bitbucket.inkytonik.kiama.parsing.Success
    import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, PositionStore}
    import org.bitbucket.inkytonik.kiama.util.Messaging

    def main (args : Array[String]) {

        args.size match {

            // If there is exactly one command-line argument
            case 1 =>
                try {
                    // Create a reader for the argument file name
                    val source = new FileSource (args (0))

                    // Create a syntax analysis module
                    val posns = new Positions
                    val parsers = new SyntaxAnalysis (posns)

                    // Create a messaging module for semantic analysis
                    val messaging = new Messaging with PositionStore {
                                       override val positions = posns
                                    }

                    // Parse the file
                    parsers.parse (parsers.parser, source) match {

                        // If it worked, we get a source tree
                        case Success (sourcetree, _) =>
                            // Pretty print the source tree
                            // println (layout (any (sourcetree)))

                            // Process the source tree
                            process (sourcetree, messaging)

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
     * Process the source tree by analysing it to check for semantic
     * errors.  If any messages are produced, print them.
     */
    def process (program : ExpProgram, messaging : Messaging) {
        val tree = new ExpTree (program)
        val analysis = new SemanticAnalysis (tree)
        import analysis._

        val messages = errors (program)
        if (messages.length > 0)
            messaging.report (messages)
    }

}
