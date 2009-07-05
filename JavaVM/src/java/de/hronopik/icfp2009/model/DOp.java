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

        public Instruction.Result.MemoryResult execute(final int r1, final int r2, final ROM memory
        ) {
            return Instruction.memoryResult(memory.getValue(r1) + memory.getValue(r2));
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "<- " + memory.getValue(r1) + " + " + memory.getValue(r2);
        }
    },

    Sub {

        public Instruction.Result.MemoryResult execute(final int r1, final int r2, final ROM memory
        ) {
            return Instruction.memoryResult(memory.getValue(r1) - memory.getValue(r2));
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "<- " + memory.getValue(r1) + " - " + memory.getValue(r2);
        }
    },

    Mult {

        public Instruction.Result.MemoryResult execute(final int r1, final int r2, final ROM memory
        ) {
            return Instruction.memoryResult(memory.getValue(r1) * memory.getValue(r2));
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "<- " + memory.getValue(r1) + " * " + memory.getValue(r2);
        }
    },

    Div {

        public Instruction.Result.MemoryResult execute(final int r1, final int r2, final ROM memory
        ) {
            return Instruction.memoryResult(memory.getValue(r2) == 0 ? 0 : memory.getValue(r1) / memory.getValue(r2));
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "<- " + memory.getValue(r1) + " / " + memory.getValue(r2);
        }
    },

    Output {

        public Instruction.Result.OutputResult execute(final int r1, final int r2, final ROM memory
        ) {
            return Instruction.outputResult(new Output(r1, memory.getValue(r2)));
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "outport[" + r1 + "] <- " + memory.getValue(r2);
        }
    },

    Phi {

        public Instruction.Result.MemoryResult execute(final int r1, final int r2, final ROM memory
        ) {
            return Instruction.memoryResult(memory.isStatus() ? memory.getValue(r1) : memory.getValue(r2));
        }

        public String toSemanticsString(int r1, int r2, ROM memory) {
            return "<- " + (memory.isStatus() ? memory.getValue(r1) : memory.getValue(r2));
        }
    };

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public abstract Instruction.Result execute(int r1, int r2, ROM memory);

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
