package de.hronopik.icfp2009.model;

/**
 * A 32-bit instruction.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
public abstract class Instruction {

    final int address;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    Instruction(int address) {
        this.address = address;
    }

    /**
     * Executes the instruction.
     *
     * @param stepIndex  the index of the current simulation step
     * @param memory
     * @param inputPorts
     * @return the result of the instruction execution
     */
    public abstract Result execute(int stepIndex, ROM memory, InputPorts inputPorts);

    //---------------------------------------------------------------------------------------------
    // Result
    //---------------------------------------------------------------------------------------------

    public static Result.MemoryResult memoryResult(double value) {
        return new Result.MemoryResult(value);
    }

    public static Result.OutputResult outputResult(Output output) {
        return new Result.OutputResult(output);
    }

    public static Result.StatusResult statusResult(boolean value) {
        return new Result.StatusResult(value);
    }

    private final static Result.NoopResult NOOP_RESULT = new Result.NoopResult();

    public static Result.NoopResult noopResult() {
        return NOOP_RESULT;
    }

    /**
     * A result of an instruction.
     */
    public abstract static class Result {

        private Result() {
        }

        public abstract <T> T cont(ResultC<T> continuation);

        public static final class MemoryResult extends Result {

            private final double value;

            private MemoryResult(double value) {
                this.value = value;
            }

            public double getValue() {
                return value;
            }

            public <T> T cont(ResultC<T> continuation) {
                return continuation.memoryResult(value);
            }

            @Override
            public String toString() {
                return "MemoryResult(" + value + ")";
            }
        }

        public static final class OutputResult extends Result {

            private final Output output;

            private OutputResult(Output output) {
                this.output = output;
            }

            public Output getOutput() {
                return output;
            }

            public <T> T cont(ResultC<T> continuation) {
                return continuation.outputResult(output);
            }

            @Override
            public String toString() {
                return "OutputResult(" + output + ")";
            }
        }

        public static final class StatusResult extends Result {

            private final boolean value;

            private StatusResult(boolean value) {
                this.value = value;
            }

            public boolean getValue() {
                return value;
            }

            public <T> T cont(ResultC<T> continuation) {
                return continuation.statusResult(value);
            }

            @Override
            public String toString() {
                return "StatusResult(" + value + ")";
            }
        }

        public static final class NoopResult extends Result {

            private NoopResult() {
            }

            public <T> T cont(ResultC<T> continuation) {
                return continuation.noopResult();
            }

            @Override
            public String toString() {
                return "NoopResult()";
            }
        }
    }

    public interface ResultC<T> {

        T memoryResult(double value);

        T outputResult(Output output);

        T statusResult(boolean value);

        T noopResult();
    }
}
