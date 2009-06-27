package de.hronopik.icfp2009.vm;

import org.jetbrains.annotations.NotNull;

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

        @NotNull
        public String toSemanticsString(int rd, int r1, int r2, double[] values, boolean status) {
            return "mem[" + rd + "] <- " + values[r1] + " + " + values[r2];
        }
    },

    Sub {

        @NotNull
        public String toSemanticsString(int rd, int r1, int r2, double[] values, boolean status) {
            return "mem[" + rd + "] <- " + values[r1] + " - " + values[r2];
        }
    },

    Mult {

        @NotNull
        public String toSemanticsString(int rd, int r1, int r2, double[] values, boolean status) {
            return "mem[" + rd + "] <- " + values[r1] + " * " + values[r2];
        }
    },

    Div {

        @NotNull
        public String toSemanticsString(int rd, int r1, int r2, double[] values, boolean status) {
            return "mem[" + rd + "] <- " + values[r1] + " / " + values[r2];
        }
    },

    Output {

        @NotNull
        public String toSemanticsString(int rd, int r1, int r2, double[] values, boolean status) {
            return "outport[" + r1 + "] <- " + values[r2];
        }
    },

    Phi {

        @NotNull
        public String toSemanticsString(int rd, int r1, int r2, double[] values, boolean status) {
            return "mem[" + rd + "] <- " + (status ? values[r1] : values[r2]);
        }
    };

    @NotNull
    public abstract String toSemanticsString(int rd, int r1, int r2, double[] values, boolean status);

    /**
     * Returns the operation according to the given opcode.
     *
     * @param opcode the opcode of the operation to return
     * @return the operation
     */
    public static DOp fromOpcode(int opcode) {
        return values()[opcode - 1];
    }
}
