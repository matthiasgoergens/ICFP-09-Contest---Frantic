package de.hronopik.icfp2009.model;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Map;

/**
 * Operation for S-Type instructions.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.3 p.4"
 */
public enum SOp implements Op {

    // Do not change the order of the operations!

    Noop {

        @NotNull
        public String toSemanticsString(int rd, @Nullable Parameter param, int r1, @NotNull double[] values,
                                        @NotNull Map<Integer, Double> inputs) {
            return "mem[" + rd + "] <- " + "mem[" + rd + "]";
        }
    },

    Cmpz {

        @NotNull
        public String toSemanticsString(int rd, @Nullable Parameter param, int r1, @NotNull double[] values,
                                        @NotNull Map<Integer, Double> inputs) {
            return "status <- " + values[r1] + " " + param + " 0.0";
        }
    },

    Sqrt {

        @NotNull
        public String toSemanticsString(int rd, @Nullable Parameter param, int r1, @NotNull double[] values,
                                        @NotNull Map<Integer, Double> inputs) {
            return "mem[" + rd + "] <- |sqrt(" + values[r1] + ")|";
        }
    },

    Copy {

        @NotNull
        public String toSemanticsString(int rd, @Nullable Parameter param, int r1, @NotNull double[] values,
                                        @NotNull Map<Integer, Double> inputs) {
            return "mem[" + rd + "] <- " + values[r1];
        }
    },

    Input {

        @NotNull
        public String toSemanticsString(int rd, @Nullable Parameter param, int r1, @NotNull double[] values,
                                        @NotNull Map<Integer, Double> inputs) {
            Double inputValue = inputs.get(r1);

            // Not existing inputs are assumed to be zero
            return "mem[" + rd + "] <- " + (inputValue == null ? "0.0" : inputValue);
        }
    };

    @NotNull
    public abstract String toSemanticsString(int rd, @Nullable Parameter param, int r1, @NotNull double[] values,
                                             @NotNull Map<Integer, Double> inputs);

    /**
     * Returns the operation according to the given opcode.
     *
     * @param opcode the opcode of the operation to return
     * @return the operation
     */
    public static SOp fromOpcode(int opcode) {
        return values()[opcode];
    }
}
