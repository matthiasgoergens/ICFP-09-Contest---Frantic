package de.hronopik.icfp2009.model;

import static java.util.logging.Level.FINE;
import java.util.logging.Logger;

/**
 * A D-Type instruction.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.2 p.3"
 */
public final class DInstruction extends Instruction {

    private static final Logger logger = Logger.getLogger("de.hronopik.icfp2009.vm.InstructionTrace");

    private final DOp op;
    private final int r1;
    private final int r2;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public DInstruction(int address, DOp op, int r1, int r2) {
        super(address);
        this.op = op;
        this.r1 = r1;
        this.r2 = r2;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------


    public DOp getOp() {
        return op;
    }

    public int getR1() {
        return r1;
    }

    public int getR2() {
        return r2;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public Result execute(int stepIndex, ROM memory, InputPorts inputPorts) {

        // Log into the instruction trace
        if (logger.isLoggable(FINE)) {
            logger.fine(stepIndex + "," + address + "," + toString() + "," + op.toSemanticsString(r1, r2, memory));
        }

        return op.execute(r1, r2, memory);
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        return op + "(" + r1 + ", " + r2 + ")";
    }
}

