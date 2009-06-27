package de.hronopik.icfp2009.model;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import static java.util.logging.Level.FINE;
import java.util.logging.Logger;

/**
 * A S-Type instruction.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.3 p.3"
 */
public final class SInstruction<P extends Parameter> extends Instruction {

    private static final Logger logger = Logger.getLogger("de.hronopik.icfp2009.vm.InstructionTrace");

    @NotNull
    private final SOp op;

    @Nullable
    private final P param;

    private final int r1;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public SInstruction(int address, @NotNull SOp op, @Nullable P param, int r1) {
        super(address);
        this.op = op;
        this.param = param;
        this.r1 = r1;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    @NotNull
    public SOp getOp() {
        return op;
    }

    @Nullable
    public P getParam() {
        return param;
    }

    public int getR1() {
        return r1;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public boolean execute(int stepIndex, boolean status, @NotNull double[] values,
                           @NotNull Map<Integer, Double> inputs, @NotNull Map<Integer, Double> outputs) {

        switch (op) {
            case Noop:
                break;
            case Cmpz:
                status = ((CompParam) param).isCompZero(values[r1]);
                break;
            case Sqrt:
                double value = values[r1];
                if (value < 0) {
                    // TODO: implement absolute value; see page 4
                    throw new ArithmeticException("negative sqrt");
                }
                values[address] = Math.sqrt(value);
                break;
            case Copy:
                values[address] = values[r1];
                break;
            case Input:
                Double inputValue = inputs.get(r1);

                // Not existing inputs are assumed to be zero
                values[address] = inputValue == null ? 0 : inputValue;

                break;
            default:
                throw new IllegalArgumentException("Unknown operation: " + op);
        }

        // Log into the instruction trace
        if (logger.isLoggable(FINE)) {
            logger.fine(stepIndex + "," + address + "," + toString() + "," +
                    op.toSemanticsString(address, param, r1, values, inputs));
        }

        return status;
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        return op + "(" + (param != null ? param + ", " : "") + r1 + ")";
    }
}