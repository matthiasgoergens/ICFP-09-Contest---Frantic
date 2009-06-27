package de.hronopik.icfp2009.vm;

import org.jetbrains.annotations.NotNull;

import java.util.Map;
import static java.util.logging.Level.FINE;
import java.util.logging.Logger;

/**
 * A D-Type instruction.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.2 p.3"
 */
final class DInstruction extends Instruction {

    private static final Logger logger = Logger.getLogger("de.hronopik.icfp2009.vm.InstructionTrace");

    @NotNull
    private final DOp op;
    private final int r1;
    private final int r2;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    DInstruction(int address, @NotNull DOp op, int r1, int r2) {
        super(address);
        this.op = op;
        this.r1 = r1;
        this.r2 = r2;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    @NotNull
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

    boolean execute(int stepIndex, boolean status, double[] values, @NotNull Map<Integer, Double> inputs,
                    @NotNull Map<Integer, Double> outputs) {

        switch (op) {
            case Add:
                values[address] = values[r1] + values[r2];
                break;
            case Sub:
                values[address] = values[r1] - values[r2];
                break;
            case Mult:
                values[address] = values[r1] * values[r2];
                break;
            case Div:
                values[address] = values[r2] == 0 ? 0 : values[r1] / values[r2];
                break;
            case Output:
                outputs.put(r1, values[r2]);
                break;
            case Phi:
                values[address] = status ? values[r1] : values[r2];
                break;
        }

        // Log into the instruction trace
        if (logger.isLoggable(FINE)) {
            logger.fine(stepIndex + "," + address + "," + toString() + "," +
                    op.toSemanticsString(address, r1, r2, values, status));
        }

        return status;
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        return op + "(" + r1 + ", " + r2 + ")";
    }
}

