package de.hronopik.icfp2009.model;

import org.jetbrains.annotations.Nullable;

import java.util.Map;
import static java.util.logging.Level.FINE;
import java.util.logging.Logger;

import de.hronopik.icfp2009.util.Maybe;
import static de.hronopik.icfp2009.util.Maybe.nothing;

/**
 * A S-Type instruction.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.3 p.3"
 */
public final class SInstruction<P extends Maybe<MP>, MP extends Parameter> extends Instruction {

    private static final Logger logger = Logger.getLogger("de.hronopik.icfp2009.vm.InstructionTrace");


    private final SOpI<P, MP> op;

    private final P param;

    private final int r1;

    //---------------------------------------------------------------------------------------------
    // Constructors
    //---------------------------------------------------------------------------------------------

    public static <P extends Maybe<MP>, MP extends Parameter> SInstruction<P, MP> newInstance(int address,
                                                                                              SOpI<P, MP> op, P param,
                                                                                              int r1) {
        return new SInstruction<P, MP>(address, op, param, r1);
    }

    public SInstruction(int address, SOpI<P, MP> op, P param, int r1) {
        super(address);
        this.op = op;
        this.param = param;
        this.r1 = r1;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public SOpI<P, MP> getOp() {
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

    public Result execute(int stepIndex, boolean status, ROM memory, InputPorts inputPorts,
                          Map<Integer, Double> outputs) {

        // Log into the instruction trace
        if (logger.isLoggable(FINE)) {
            logger.fine(stepIndex + "," + address + "," + toString() + "," +
                    op.toSemanticsString(param, r1, memory, inputPorts));
        }

        return op.execute(param, r1, memory, status, inputPorts);
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        return op + "(" + (param != null ? param + ", " : "") + r1 + ")";
    }

    //---------------------------------------------------------------------------------------------
    // SResult
    //---------------------------------------------------------------------------------------------

    abstract static class SResult implements Result {

        public Maybe<OutputPorts.Assignment> getOutputAssignment() {
            return nothing();
        }
    }
}