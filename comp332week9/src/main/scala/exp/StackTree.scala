/**
 * Expression language implementation stack code representation.
 *
 * Copyright 2014, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

/**
 * Module containing tree structures for representing stack code programs.
 * See the Week 9 mixed class handout for details of the meanings of instructions.
 */
object StackTree {

    /**
     * A stack machine program is a sequence of instructions.
     */
    type StackProgram = List[StackInstr]

    /**
     * Base class for stack machine instructions.
     */
    sealed abstract class StackInstr

    /**
     * Pop two integer values off the stack, add them and push the result back on the stack.
     */
    case class Add () extends StackInstr

    /**
     * Pop an integer value and then a String off the stack and assign the value to the String.
     */
    case class Assign () extends StackInstr

    /**
     * Pop two integer values off the stack, divide them and push the result back on the stack.
     */
    case class Div () extends StackInstr

    /**
     * Pop the top integer value off the stack and conditonally jump to the instruction
     * labelled by a given label if the popped value is zero.
     */
    case class GoFalse (lab : String) extends StackInstr

    /**
     * Unconditionally jump to the instruction labelled by a given label.
     */
    case class Goto (lab : String) extends StackInstr

    /**
     * Pop the top integer value off the stack and conditonally jump to the instruction
     * labelled by a given label if the popped value is non-zero.
     */
    case class GoTrue (lab : String) extends StackInstr

    /**
     * Halt the machine.
     */
    case class Halt () extends StackInstr

    /**
     * A special instruction to mark the position of a label in the code. These
     * will be output symbolically by the compiler but then replaced with absolute
     * addresses as the program executes.
     */
    case class Label (lab : String) extends StackInstr

    /**
     * Push a named memory location onto the stack.
     */
    case class LValue (loc : String) extends StackInstr

    /**
     * Pop two integer values off the stack, multiply them and push the result back on the stack.
     */
    case class Mul () extends StackInstr

    /**
     * Pop an integer value off the stack and discard it.
     */
    case class Pop () extends StackInstr

    /**
     * Pop the top integer value off the stack and print it.
     */
    case class Print () extends StackInstr

    /**
     * Push an integer value on the stack.
     */
    case class Push (v : Int) extends StackInstr

    /**
     * Push the value stored in a named memory location onto the stack.
     */
    case class RValue (loc : String) extends StackInstr

    /**
     * Pop two integer values off the stack, subtract them and push the result back on the stack.
     */
    case class Sub () extends StackInstr


}
