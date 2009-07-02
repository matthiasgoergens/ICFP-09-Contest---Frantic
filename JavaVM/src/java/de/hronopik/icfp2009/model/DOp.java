package de.hronopik.icfp2009.model;

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

        public Instruction.Result.MemoryResult execute(final int r1, final int r2, final ROM memory, boolean status) {
            return Instruction.memoryResult(memory.getValue(r1) + memory.getValue(r2));
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "<- " + memory.getValue(r1) + " + " + memory.getValue(r2);
        }
    },

    Sub {

        public Instruction.Result.MemoryResult execute(final int r1, final int r2, final ROM memory, boolean status) {
            return Instruction.memoryResult(memory.getValue(r1) - memory.getValue(r2));
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "<- " + memory.getValue(r1) + " - " + memory.getValue(r2);
        }
    },

    Mult {

        public Instruction.Result.MemoryResult execute(final int r1, final int r2, final ROM memory, boolean status) {
            return Instruction.memoryResult(memory.getValue(r1) * memory.getValue(r2));
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "<- " + memory.getValue(r1) + " * " + memory.getValue(r2);
        }
    },

    Div {

        public Instruction.Result.MemoryResult execute(final int r1, final int r2, final ROM memory, boolean status) {
            return Instruction.memoryResult(memory.getValue(r2) == 0 ? 0 : memory.getValue(r1) / memory.getValue(r2));
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "<- " + memory.getValue(r1) + " / " + memory.getValue(r2);
        }
    },

    Output {

        public Instruction.Result.OutputResult execute(final int r1, final int r2, final ROM memory, boolean status) {
            return Instruction.outputResult(new Output(r1, memory.getValue(r2)));
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "outport[" + r1 + "] <- " + memory.getValue(r2);
        }
    },

    Phi {

        public Instruction.Result.MemoryResult execute(final int r1, final int r2, final ROM memory, final boolean status) {
            return Instruction.memoryResult(status ? memory.getValue(r1) : memory.getValue(r2));
        }

        public String toSemanticsString(int r1, int r2, ROM memory, boolean status) {
            return "<- " + (status ? memory.getValue(r1) : memory.getValue(r2));
        }
    };

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public abstract Instruction.Result execute(int r1, int r2, ROM memory, boolean status);

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
}
