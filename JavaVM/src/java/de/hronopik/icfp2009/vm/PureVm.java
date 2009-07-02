package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.io.OrbitBinaryFrame;
import de.hronopik.icfp2009.model.InputPorts;
import de.hronopik.icfp2009.model.Instruction;
import de.hronopik.icfp2009.model.Output;
import de.hronopik.icfp2009.model.ROM;
import de.hronopik.icfp2009.util.*;
import static de.hronopik.icfp2009.util.List.nil;
import static de.hronopik.icfp2009.util.Pairs.newPair;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class PureVm {

    private final List<Instruction> instructions;

    private final Memory memory;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    /**
     * Creates a new VM directly from the binary frames.
     *
     * @param frames the frames which are read from a orbit binary
     */
    public PureVm(java.util.List<OrbitBinaryFrame> frames) {
        List<Instruction> instructions = nil();
        List<Double> memory = nil();

        for (OrbitBinaryFrame frame : frames) {
            instructions = new LinkedList<Instruction>(frame.getInstruction(), instructions);
            memory = new LinkedList<Double>(frame.getValue(), memory);
        }

        this.instructions = instructions.reverse();
        this.memory = new Memory(memory.reverse(), false);
    }

    public PureVm(List<Instruction> instructions, Memory memory) {
        this.instructions = instructions;
        this.memory = memory.rewind();
    }

    //---------------------------------------------------------------------------------------------
    // Stepping
    //---------------------------------------------------------------------------------------------

    public Pair<PureVm, Map<Integer, Double>> step(final InputPorts input) {

        // Starting with our memory and an empty map of outputs
        final Pair<Memory, Map<Integer, Double>> start = newPair(memory, ListMap.<Integer, Double>emptyMap());

        // Folding over the instructions getting a new memory and the outputs
        final Pair<Memory, Map<Integer, Double>> memOut = instructions.foldLeft(start, new MicroStep(input));

        // Returning a new VM and the output
        return newPair(new PureVm(instructions, memOut.getFst()), memOut.getSnd());
    }

    private static class MicroStep implements Function2<Pair<Memory, Map<Integer, Double>>, Instruction, Pair<Memory, Map<Integer, Double>>> {

        private final InputPorts input;

        private MicroStep(InputPorts input) {
            this.input = input;
        }

        public Pair<Memory, Map<Integer, Double>> apply(Pair<Memory, Map<Integer, Double>> state, Instruction instruction) {
            final Memory memory = state.getFst();
            final Map<Integer, Double> output = state.getSnd();

            return instruction.execute(0, memory.isStatus(), memory, input).cont(
                    new Instruction.ResultC<Pair<Memory, Map<Integer, Double>>>() {

                        public Pair<Memory, Map<Integer, Double>> memoryResult(double value) {
                            return newPair(memory.setValue(value), output);
                        }

                        public Pair<Memory, Map<Integer, Double>> outputResult(Output value) {
                            return newPair(memory.copy(), output.add(value));
                        }

                        public Pair<Memory, Map<Integer, Double>> statusResult(boolean value) {
                            return newPair(memory.setStatus(value), output);
                        }

                        public Pair<Memory, Map<Integer, Double>> noopResult() {
                            return newPair(memory.copy(), output);
                        }
                    }
            );
        }
    }

    /**
     * This class represents the memory of the VM.
     */
    private static class Memory implements ROM {

        private final Array<Double> oldMemory;

        private final List<Double> newMemory;

        private final boolean status;

        private final int insertPos;

        private Memory(Collection<Double> values, boolean status) {
            this.oldMemory = new ReadOnlyArray<Double>(values);
            this.newMemory = nil();
            this.status = status;
            this.insertPos = 0;
        }

        private Memory(Array<Double> oldMemory, List<Double> newMemory, boolean status, int insertPos) {
            this.oldMemory = oldMemory;
            this.newMemory = newMemory;
            this.status = status;
            this.insertPos = insertPos;
        }

        //---------------------------------------------------------------------------------------------
        //
        //---------------------------------------------------------------------------------------------

        public double getValue(final int address) {
            return (address >= insertPos ? oldMemory.get(address) : newMemory.drop(address).head()).maybe(
                    Continuations.<Double>fail("Illegal memory access at address " + address + ". " +
                            "insertPos = " + insertPos + ", old memory size = " + oldMemory.size() +
                            ", new memory size = " + newMemory.size())
            );
        }

        public boolean isStatus() {
            return status;
        }

        /**
         * Sets the given value at the insert position.
         *
         * @param value the value to set
         * @return a new memory instance
         */
        private Memory setValue(double value) {
            return new Memory(oldMemory, new LinkedList<Double>(value, newMemory), status, insertPos + 1);
        }

        private Memory setStatus(boolean value) {
            return new Memory(oldMemory, new LinkedList<Double>(getValue(insertPos), newMemory), value, insertPos + 1);
        }

        private Memory copy() {
            return new Memory(oldMemory, new LinkedList<Double>(getValue(insertPos), newMemory), status, insertPos + 1);
        }

        public Memory rewind() {
            return new Memory(newMemory.reverse(), status);
        }
    }
}