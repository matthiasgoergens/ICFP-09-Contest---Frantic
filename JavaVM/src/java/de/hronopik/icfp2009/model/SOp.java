package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Maybe;
import static de.hronopik.icfp2009.util.Maybe.just;
import static de.hronopik.icfp2009.util.Maybe.nothing;

/**
 * Operation for S-Type instructions.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.3 p.4"
 */
public class SOp {

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Noop = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        private final MemoryResult RESULT = new MemoryResult() {
            public Maybe<Double> getMemoryValue() {
                return nothing();
            }
        };

        public SInstruction.SResult execute(Maybe.Nothing<Parameter> param, int r1, final ROM memory,
                                            boolean status, InputPorts inputPorts) {
            return RESULT;
        }


        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, ROM memory,
                                        InputPorts inputPorts) {
            return "Noop";
        }
    };

    public static final SOpI<Maybe.Just<CompParam>, CompParam> Cmpz = new SOpI<Maybe.Just<CompParam>, CompParam>() {

        public SInstruction.SResult execute(final Maybe.Just<CompParam> param, final int r1,
                                            final ROM memory, final boolean status,
                                            InputPorts inputPorts) {
            return new StatusResult() {

                public Maybe<Boolean> getStatus() {
                    return just(param.just().isCompZero(memory.getValue(r1)));
                }
            };
        }


        public String toSemanticsString(Maybe.Just<CompParam> param, int r1, ROM memory,
                                        InputPorts inputPorts) {
            return "status <- " + memory.getValue(r1) + " " + param + " 0.0";
        }
    };

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Sqrt = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public SInstruction.SResult execute(Maybe.Nothing<Parameter> param, final int r1, final ROM memory,
                                            boolean status, InputPorts inputPorts) {
            return new MemoryResult() {

                public Maybe<Double> getMemoryValue() {
                    double value = memory.getValue(r1);
                    if (value < 0) {
                        // TODO: implement absolute value; see page 4
                        throw new ArithmeticException("negative sqrt");
                    }
                    return just(Math.sqrt(value));
                }
            };
        }

        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, ROM memory,
                                        InputPorts inputPorts) {
            return "<- |sqrt(" + memory.getValue(r1) + ")|";
        }
    };

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Copy = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public SInstruction.SResult execute(Maybe.Nothing<Parameter> param, final int r1, final ROM memory,
                                            boolean status, InputPorts inputPorts) {
            return new MemoryResult() {

                public Maybe<Double> getMemoryValue() {
                    return just(memory.getValue(r1));
                }
            };
        }

        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, ROM memory,
                                        InputPorts inputPorts) {
            return "<- " + memory.getValue(r1);
        }
    };

    public static final SOpI<Maybe.Nothing<Parameter>, Parameter> Input = new SOpI<Maybe.Nothing<Parameter>, Parameter>() {

        public SInstruction.SResult execute(Maybe.Nothing<Parameter> param, final int r1, ROM memory,
                                            boolean status, final InputPorts inputPorts) {
            return new MemoryResult() {

                public Maybe<Double> getMemoryValue() {
                    return just(inputPorts.getValue(r1));
                }
            };
        }


        public String toSemanticsString(Maybe.Nothing<Parameter> param, int r1, ROM memory, InputPorts inputPorts) {
            return "<- " + inputPorts.getValue(r1);
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

    //---------------------------------------------------------------------------------------------
    // MemoryResult
    //---------------------------------------------------------------------------------------------

    private static abstract class MemoryResult extends SInstruction.SResult {

        public Maybe<Boolean> getStatus() {
            return nothing();
        }
    }

    //---------------------------------------------------------------------------------------------
    // StatusResult
    //---------------------------------------------------------------------------------------------

    private static abstract class StatusResult extends SInstruction.SResult {

        public Maybe<Double> getMemoryValue() {
            return nothing();
        }
    }
}
