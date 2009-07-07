package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Map;
import de.hronopik.icfp2009.util.Pair;
import static de.hronopik.icfp2009.util.Pairs.newPair;
import de.hronopik.icfp2009.vm.Memory;

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

        public Pair<Memory, Map<Integer, Double>> execute(final int r1, final int r2, final Memory memory, Map<Integer, Double> output) {
            return newPair(memory.setValue(memory.getValue(r1) + memory.getValue(r2)), output);
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "<- " + memory.getValue(r1) + " + " + memory.getValue(r2);
        }
    },

    Sub {

        public Pair<Memory, Map<Integer, Double>> execute(final int r1, final int r2, final Memory memory, Map<Integer, Double> output) {
            return newPair(memory.setValue(memory.getValue(r1) - memory.getValue(r2)), output);
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "<- " + memory.getValue(r1) + " - " + memory.getValue(r2);
        }
    },

    Mult {

        public Pair<Memory, Map<Integer, Double>> execute(final int r1, final int r2, final Memory memory, Map<Integer, Double> output) {
            return newPair(memory.setValue(memory.getValue(r1) * memory.getValue(r2)), output);
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "<- " + memory.getValue(r1) + " * " + memory.getValue(r2);
        }
    },

    Div {

        public Pair<Memory, Map<Integer, Double>> execute(final int r1, final int r2, final Memory memory, Map<Integer, Double> output) {
            final double divisor = memory.getValue(r2);
            return newPair(memory.setValue(divisor == 0 ? 0 : memory.getValue(r1) / divisor), output);
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "<- " + memory.getValue(r1) + " / " + memory.getValue(r2);
        }
    },

    Output {

        public Pair<Memory, Map<Integer, Double>> execute(final int r1, final int r2, final Memory memory, Map<Integer, Double> output) {
            return newPair(memory, output.put(r1, memory.getValue(r2)));
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "outport[" + r1 + "] <- " + memory.getValue(r2);
        }
    },

    Phi {

        public Pair<Memory, Map<Integer, Double>> execute(final int r1, final int r2, final Memory memory, Map<Integer, Double> output) {
            return newPair(memory.setValue(memory.isStatus() ? memory.getValue(r1) : memory.getValue(r2)), output);
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "<- " + (memory.isStatus() ? memory.getValue(r1) : memory.getValue(r2));
        }
    };

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public abstract Pair<Memory, Map<Integer, Double>> execute(int r1, int r2, Memory memory, Map<Integer, Double> output);

    public abstract String toSemanticsString(int r1, int r2, ROM memory);

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
}
