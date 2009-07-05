package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.io.OrbitBinaryFrame;
import de.hronopik.icfp2009.model.InputPorts;
import de.hronopik.icfp2009.model.Instruction;
import de.hronopik.icfp2009.model.Output;
import de.hronopik.icfp2009.util.*;
import static de.hronopik.icfp2009.util.List.nil;
import static de.hronopik.icfp2009.util.Pairs.newPair;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class PureVm {

    private final List<Instruction> instructions;

    private final AvlMemory memory;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    /**
     * Creates a new VM directly from the binary frames.
     *
     * @param frames the frames which are read from a orbit binary
     */
    public PureVm(java.util.List<OrbitBinaryFrame> frames) {

        if (frames.isEmpty()) {
            throw new IllegalArgumentException("empty frames");
        }

        List<Instruction> instructions = nil();
        List<Double> memory = nil();

        for (OrbitBinaryFrame frame : frames) {
            instructions = new LinkedList<Instruction>(frame.getInstruction(), instructions);
            memory = new LinkedList<Double>(frame.getValue(), memory);
        }

        this.instructions = instructions.reverse();
        this.memory = new AvlMemory((List.Element<Double>) memory.reverse(), false);
    }

    public PureVm(List<Instruction> instructions, AvlMemory memory) {
        this.instructions = instructions;
        this.memory = memory;
    }

    //---------------------------------------------------------------------------------------------
    // Stepping
    //---------------------------------------------------------------------------------------------

    public Pair<PureVm, Map<Integer, Double>> step(final InputPorts input) {

        // Starting with our memory and an empty map of outputs
        final Pair<AvlMemory, Map<Integer, Double>> start = newPair(memory, ListMap.<Integer, Double>emptyMap());

        // Folding over the instructions getting a new memory and the outputs
        final Pair<AvlMemory, Map<Integer, Double>> memOut = instructions.foldLeft(start, new MicroStep(input));

        // Returning a new VM and the output
        return newPair(new PureVm(instructions, memOut.getFst()), memOut.getSnd());
    }

    private static class MicroStep implements
            Function2<Pair<AvlMemory, Map<Integer, Double>>, Instruction, Pair<AvlMemory, Map<Integer, Double>>> {

        private final InputPorts input;

        private MicroStep(InputPorts input) {
            this.input = input;
        }

        public Pair<AvlMemory, Map<Integer, Double>> apply(Pair<AvlMemory, Map<Integer, Double>> state,
                                                        Instruction instruction) {
            final AvlMemory memory = state.getFst();
            final Map<Integer, Double> output = state.getSnd();

            return instruction.execute(0, memory, input).cont(
                    new Instruction.ResultC<Pair<AvlMemory, Map<Integer, Double>>>() {

                        public Pair<AvlMemory, Map<Integer, Double>> memoryResult(double value) {
                            return newPair(memory.setValue(value), output);
                        }

                        public Pair<AvlMemory, Map<Integer, Double>> outputResult(Output value) {
                            return newPair(memory.copy(), output.add(value));
                        }

                        public Pair<AvlMemory, Map<Integer, Double>> statusResult(boolean value) {
                            return newPair(memory.setStatus(value), output);
                        }

                        public Pair<AvlMemory, Map<Integer, Double>> noopResult() {
                            return newPair(memory.copy(), output);
                        }
                    }
            );
        }
    }

}