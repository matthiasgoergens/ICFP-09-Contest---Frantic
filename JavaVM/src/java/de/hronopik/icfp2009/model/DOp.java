package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Maybe;
import static de.hronopik.icfp2009.util.Maybe.nothing;
import static de.hronopik.icfp2009.util.Maybe.just;
import de.hronopik.icfp2009.vm.Output;

/**
 * Operation for D-Type instructions.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.2 p.3"
 */
public enum DOp implements Op {

    // Do not change the order of the operations!

    Add {

        public MemoryResult execute(final int r1, final int r2, final ROM memory, boolean status) {
            return new MemoryResult() {
                public Maybe<Double> getMemoryValue() {
                    return just(memory.getValue(r1) + memory.getValue(r2));
                }
            };
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "<- " + memory.getValue(r1) + " + " + memory.getValue(r2);
        }
    },

    Sub {

        public MemoryResult execute(final int r1, final int r2, final ROM memory, boolean status) {
            return new MemoryResult() {
                public Maybe<Double> getMemoryValue() {
                    return just(memory.getValue(r1) - memory.getValue(r2));
                }
            };
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "<- " + memory.getValue(r1) + " - " + memory.getValue(r2);
        }
    },

    Mult {

        public MemoryResult execute(final int r1, final int r2, final ROM memory, boolean status) {
            return new MemoryResult() {
                public Maybe<Double> getMemoryValue() {
                    return just(memory.getValue(r1) * memory.getValue(r2));
                }
            };
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "<- " + memory.getValue(r1) + " * " + memory.getValue(r2);
        }
    },

    Div {

        public MemoryResult execute(final int r1, final int r2, final ROM memory, boolean status) {
            return new MemoryResult() {
                public Maybe<Double> getMemoryValue() {
                    return just(memory.getValue(r2) == 0 ? 0 : memory.getValue(r1) / memory.getValue(r2));
                }
            };
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "<- " + memory.getValue(r1) + " / " + memory.getValue(r2);
        }
    },

    Output {

        public OutputResult execute(final int r1, final int r2, final ROM memory, boolean status) {
            return new OutputResult(){
                public Maybe<Output> getOutput() {
                    return just(new Output(r1, memory.getValue(r2)));
                }
            };
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "outport[" + r1 + "] <- " + memory.getValue(r2);
        }
    },

    Phi {

        public MemoryResult execute(final int r1, final int r2, final ROM memory, final boolean status) {
            return new MemoryResult() {
                public Maybe<Double> getMemoryValue() {
                    return just(status ? memory.getValue(r1) : memory.getValue(r2));
                }
            };
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "<- " + (status ? memory.getValue(r1) : memory.getValue(r2));
        }
    };

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public abstract DInstruction.DResult execute(int r1, int r2, ROM memory, boolean status);

    public abstract String toSemanticsString(int r1, int r2, ROM memory, boolean status);

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    /**
     * Returns the operation according to the given opcode.
     *
     * @param opcode the opcode of the operation to return
     * @return the operation
     */
    public static DOp fromOpcode(int opcode) {
        return values()[opcode - 1];
    }

    //---------------------------------------------------------------------------------------------
    // MemoryResult
    //---------------------------------------------------------------------------------------------

    private static abstract class MemoryResult extends DInstruction.DResult {

        public Maybe<Output> getOutput() {
            return nothing();
        }
    }

    //---------------------------------------------------------------------------------------------
    // MemoryResult
    //---------------------------------------------------------------------------------------------

    private static abstract class OutputResult extends DInstruction.DResult {

        public Maybe<Double> getMemoryValue() {
            return nothing();
        }
    }
}
