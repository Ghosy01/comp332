/**
 * Stack machine implementation.
 *
 * Copyright 2014, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

/**
 * Module that interprets stack machine code programs.
 */
object StackMachine {

    import StackTree._

    /**
     * Execute a stack machine program.
     */
    def execute (program : StackProgram) {

        import scala.collection.mutable.HashMap

        // Convert instruction list to array for random access
        val instrs = program.toArray

        /**
         * Program counter, starts at zero.
         */
        var pc = 0

        /**
         * Operand stack elements.
         */
        sealed abstract class Element

        /**
         * Integer operand stack element.
         */
        case class IntElem (i : Int) extends Element

        /**
         * Location operand stack element.
         */
        case class LocElem (s : String) extends Element

        /**
         * The operand stack, starts empty.
         */
        var stack = List[Element] ()

        /**
         * Make a map from label names to instruction addresses that they correspond to.
         */
        def makeLabelMap () : Map[String,Int] = {
            val map = new HashMap[String,Int] ()
            var loc = 0
            while (loc < program.length) {
                program (loc) match {
                    case Label (lab) =>
                        map.update (lab, loc)
                    case _ =>
                        // Do nothing
                }
                loc = loc + 1
            }
            map.toMap
        }

        /**
         * Label map that gives the instruction address for a given named label.
         */
        val labelmap = makeLabelMap ()

        /**
         * Get an integer operand off the operand stack, raising an error if the
         * stack is empty or if the top of stack is not an integer.
         */
        def popint () : Int =
            if (stack.isEmpty)
                sys.error ("Stack Machine Error: attempt to get int from empty stack")
            else {
                stack.head match {
                    case IntElem (i) =>
                        stack = stack.tail
                        i
                    case LocElem (_) =>
                        sys.error ("Stack Machine Error: popint found string stack element")
                }
            }

        /**
         * Get a location operand off the operand stack, raising an error if the
         * stack is empty or if the top of stack is not a location.
         */
        def poploc () : String =
            if (stack.isEmpty)
                sys.error ("Stack Machine Error: attempt to get location from empty stack")
            else {
                stack.head match {
                    case IntElem (i) =>
                        sys.error ("Stack Machine Error: poploc found integer stack element")
                    case LocElem (s) =>
                        stack = stack.tail
                        s
                }
            }

        /**
         * Push an integer element onto the operand stack.
         */
        def pushint (i : Int) {
            stack = IntElem (i) :: stack
        }

        /**
         * Push a location element onto the operand stack.
         */
        def pushloc (s : String) {
            stack = LocElem (s) :: stack
        }

        /**
         * Memory: a map from variable location names to values.
         */
        val memory = new HashMap[String,Int] ()

        /**
         * Store a value in a memory location.
         */
        def store (loc : String, op : Int) {
            memory.update (loc, op)
        }

        /**
         * Load a value from a memory location.
         */
        def load (loc : String) : Int =
            if (memory contains loc)
                memory (loc)
            else
                sys.error (s"Stack Machine Error: attempt to read from undefined memory location $loc")

        /**
         * Execute a single stack machine instruction at the given program counter
         * location. Returns the new program counter from which the next instruction
         * should be executed.
         */
        def executeInstruction (pc : Int, instr : StackInstr) : Int = {

            // Optional tracing
            // println (s"Stack: $stack")
            // println (s"PC $pc: $instr")
            // println ()

            instr match {

                case Add () =>
                    val op2 = popint ()
                    val op1 = popint ()
                    pushint (op1 + op2)
                    pc + 1

                case Assign () =>
                    val op = popint ()
                    val loc = poploc ()
                    store (loc, op)
                    pc + 1

                case Label (lab) =>
                    pc + 1

                case Div () =>
                    val op2 = popint ()
                    val op1 = popint ()
                    pushint (op1 / op2)
                    pc + 1

                case GoFalse (lab) =>
                    if (labelmap contains lab) {
                        val op = popint ()
                        if (op == 0)
                            labelmap (lab)
                        else
                            pc + 1
                    } else
                        sys.error (s"Stack Machine Error: GoFalse uses unknown label $lab")

                case Goto (lab) =>
                    if (labelmap contains lab)
                        labelmap (lab)
                    else
                        sys.error (s"Stack Machine Error: Goto uses unknown label $lab")

                case GoTrue (lab) =>
                    if (labelmap contains lab) {
                        val op = popint ()
                        if (op == 0)
                            pc + 1
                        else
                            labelmap (lab)
                    } else
                        sys.error (s"Stack Machine Error: GoTrue uses unknown label $lab")

                case Halt () =>
                    0 // Unused

                case LValue (loc) =>
                    pushloc (loc)
                    pc + 1

                case Mul () =>
                    val op2 = popint ()
                    val op1 = popint ()
                    pushint (op1 * op2)
                    pc + 1

                case Pop () =>
                    popint ()
                    pc + 1

                case Print () =>
                    val op = popint ()
                    println (op)
                    pc + 1

                case Push (v) =>
                    pushint (v)
                    pc + 1

                case RValue (loc) =>
                    val op = load (loc)
                    pushint (op)
                    pc + 1

                case Sub () =>
                    val op2 = popint ()
                    val op1 = popint ()
                    pushint (op1 - op2)
                    pc + 1

            }

        }

        // Instruction fetch-decode-execute loop
        while (true) {

            // Check that the pc is in range
            if ((pc < 0) || (pc >= instrs.length))
                sys.error (s"program counter out of range $pc with ${instrs.length} instructions")

            // Fetch the current instruction
            val instr = instrs (pc)

            // Stop the execution if we have a Halt instruction
            if (instr == Halt ())
                return

            // Otherwise, execute the instruction and update the pc
            pc = executeInstruction (pc, instr)

        }

    }

}
