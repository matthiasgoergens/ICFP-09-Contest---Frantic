package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Maybe;

/**
 * Operation for S-Type instructions.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.3 p.4"
 */
public class SOp {

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Noop = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public Instruction.Result execute(Maybe.Nothing<Parameter> param, int r1, final ROM memory,
                                          boolean status, InputPorts inputPorts) {
            return Instruction.noopResult();
        }


        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, ROM memory,
                                        InputPorts inputPorts) {
            return "Noop";
        }

        @Override
        public String toString() {
            return "Noop";
        }
    };

    public static final SOpI<Maybe.Just<CompParam>, CompParam> Cmpz = new SOpI<Maybe.Just<CompParam>, CompParam>() {

        public Instruction.Result execute(final Maybe.Just<CompParam> param, final int r1,
                                          final ROM memory, final boolean status,
                                          InputPorts inputPorts) {
            return Instruction.statusResult(param.just().isCompZero(memory.getValue(r1)));
        }


        public String toSemanticsString(Maybe.Just<CompParam> param, int r1, ROM memory,
                                        InputPorts inputPorts) {
            return "status <- " + memory.getValue(r1) + " " + param.just() + " 0.0";
        }

        @Override
        public String toString() {
            return "Cmpz";
        }
    };

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Sqrt = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public Instruction.Result execute(Maybe.Nothing<Parameter> param, final int r1, final ROM memory,
                                          boolean status, InputPorts inputPorts) {
            double value = memory.getValue(r1);
            if (value < 0) {
                // sqrt is undefined for negative values; see page 4
                throw new ArithmeticException("negative sqrt");
            }
            return Instruction.memoryResult(Math.sqrt(value));
        }

        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, ROM memory,
                                        InputPorts inputPorts) {
            return "<- |sqrt(" + memory.getValue(r1) + ")|";
        }

        @Override
        public String toString() {
            return "Sqrt";
        }
    };

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Copy = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public Instruction.Result execute(Maybe.Nothing<Parameter> param, final int r1, final ROM memory,
                                          boolean status, InputPorts inputPorts) {
            return Instruction.memoryResult(memory.getValue(r1));
        }

        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, ROM memory,
                                        InputPorts inputPorts) {
            return "<- " + memory.getValue(r1);
        }

        @Override
        public String toString() {
            return "Copy";
        }
    };

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Input = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public Instruction.Result execute(Maybe.Nothing<Parameter> param, final int r1, ROM memory,
                                          boolean status, final InputPorts inputPorts) {
            return Instruction.memoryResult(inputPorts.getValue(r1));
        }

        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, ROM memory, InputPorts inputPorts) {
            return "<- " + inputPorts.getValue(r1);
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
