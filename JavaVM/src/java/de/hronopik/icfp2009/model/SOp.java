package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Maybe;
import de.hronopik.icfp2009.vm.Memory;

/**
 * Operation for S-Type instructions.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.3 p.4"
 */
public class SOp {

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Noop = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public Memory execute(Maybe.Nothing<Parameter> param, int r1, Memory memory, Map<Integer, Double> input) {
            return memory.copy();
        }

        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, Memory memory, Map<Integer, Double> input) {
            return "Noop";
        }

        @Override
        public String toString() {
            return "Noop";
        }
    };

    public static final SOpI<Maybe.Just<CompParam>, CompParam> Cmpz = new SOpI<Maybe.Just<CompParam>, CompParam>() {

        public Memory execute(Maybe.Just<CompParam> param, int r1, Memory memory, Map<Integer, Double> input) {
            return memory.setStatus(param.getValue().isCompZero(memory.getValue(r1)));
        }


        public String toSemanticsString(Maybe.Just<CompParam> param, int r1, Memory memory, Map<Integer, Double> input) {
            return "status <- " + memory.getValue(r1) + " " + param.getValue() + " 0.0";
        }

        @Override
        public String toString() {
            return "Cmpz";
        }
    };

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Sqrt = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public Memory execute(Maybe.Nothing<Parameter> param, int r1, Memory memory, Map<Integer, Double> input) {
            double value = memory.getValue(r1);
            if (value < 0) {
                // sqrt is undefined for negative values; see page 4
                throw new ArithmeticException("negative sqrt");
            }
            return memory.setValue(Math.sqrt(value));
        }

        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, Memory memory, Map<Integer, Double> input) {
            return "<- |sqrt(" + memory.getValue(r1) + ")|";
        }

        @Override
        public String toString() {
            return "Sqrt";
        }
    };

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Copy = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public Memory execute(Maybe.Nothing<Parameter> param, int r1, Memory memory, Map<Integer, Double> input) {
            return memory.setValue(memory.getValue(r1));
        }

        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, Memory memory, Map<Integer, Double> input) {
            return "<- " + memory.getValue(r1);
        }

        @Override
        public String toString() {
            return "Copy";
        }
    };

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Input = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public Memory execute(Maybe.Nothing<Parameter> param, int r1, Memory memory, Map<Integer, Double> input) {
            return memory.setValue(input.getValue(r1));
        }

        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, Memory memory, Map<Integer, Double> input) {
            return "<- " + input.getValue(r1);
        }

        @Override
        public String toString() {
            return "Input";
        }
    };

    private static final SOpI<?, ?>[] VALUES = new SOpI<?, ?>[]{Noop, Cmpz, Sqrt, Copy, Input};

    /**
     * Returns the operation according to the given opcode.
     *
     * @param opcode the opcode of the operation to return
     * @return the operation
     */
    public static SOpI<?, ?> fromOpcode(int opcode) {
        return VALUES[opcode];
    }
}
