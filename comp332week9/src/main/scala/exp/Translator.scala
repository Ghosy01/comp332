/**
 * Expression language to Stack Machine translator.
 *
 * Copyright 2014, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

/**
 * Translator from Expression language source programs to Stack Machine target programs.
 */
object Translator {

    import ExpTree._
    import StackTree._
    import scala.collection.mutable.ListBuffer

    /**
     * Return a Stack Machine program that implements the given Expression language
     * program which came from the given source file.
     */
    def translate (sourcetree : ExpProgram) : StackProgram = {

        // An instruction buffer for translating statements and expressions into
        val instrBuffer = new ListBuffer[StackInstr] ()

        /**
         * Generate an instruction by appending it to the instruction buffer.
         */
        def gen (instr : StackInstr) {
            instrBuffer.append (instr)
        }

        /**
         * Translate an expression language program by translating each of its
         * statements and returning the sequence of resulting instructions.
         */
        def translateProgram (program : ExpProgram) : StackProgram = {
            instrBuffer.clear ()
            program.stmts.map (translateStmt)
            gen (Halt ())
            instrBuffer.result ()
        }

        /**
         * Append the translation of a statement to the instruction buffer.
         */
        def translateStmt (stmt : Statement) {
            stmt match {

                case ConstDecl (IdnDef (i), _, exp) =>
                    // FIXME: TODO next week

                case ExpStmt (exp) =>
                    translateExp (exp)
                    gen (Print())

                case IfStmt (cond, stmts) =>
                    // FIXME: TODO next week

                case SetStmt (IdnExp (IdnUse (i)), exp) =>
                    // FIXME: TODO

                case _ : VarDecl =>
                    // No code generated for variable declarations

            }
        }

        /**
         * Append the translation of an expression to the instruction buffer.
         */
        def translateExp (exp : Expression) {
            exp match {

                case IdnExp (IdnUse (i)) =>
                    // FIXME: TODO

                case IntExp (i) =>
                    // FIXME: TODO

                case MinusExp (left, right) =>
                    // FIXME: TODO

                case PlusExp (left, right) =>
                    // FIXME: TODO

                case SlashExp (left, right) =>
                    // FIXME: TODO

                case StarExp (left, right) =>
                    // FIXME: TODO

                case _ =>
                    // Other expressions are not translated

            }
        }

        // Translate the program and return the resulting code
        translateProgram (sourcetree)

    }

}
