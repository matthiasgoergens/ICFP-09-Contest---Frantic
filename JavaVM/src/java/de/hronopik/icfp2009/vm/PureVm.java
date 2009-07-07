package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.io.OrbitBinaryFrame;
import de.hronopik.icfp2009.model.Instruction;
import de.hronopik.icfp2009.util.*;
import static de.hronopik.icfp2009.util.Pairs.newPair;
import static de.hronopik.icfp2009.util.List.nil;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class PureVm implements Vm<PureVm> {

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
        this.memory = new IntAvlMemory((List.Element<Double>) memory, false);
    }

    public PureVm(List<Instruction> instructions, Memory memory) {
        this.instructions = instructions;
        this.memory = memory;
    }

    //---------------------------------------------------------------------------------------------
    // Stepping
    //---------------------------------------------------------------------------------------------

    public int getStepIndex() {
        return 0;
    }

    public Pair<PureVm, Map<Integer, Double>> step() {
        return step(AvlTree.<Integer, Double>empty());
    }

    public Pair<PureVm, Map<Integer, Double>> step(final Map<Integer, Double> input) {

        // Starting with our memory and an empty map of outputs
        final Pair<Memory, Map<Integer, Double>> start = Pairs.<Memory, Map<Integer, Double>>newPair(memory, AvlTree.<Integer, Double>empty());

        // Folding over the instructions getting a new memory and the outputs
        final Pair<Memory, Map<Integer, Double>> memOut = instructions.foldLeft(start, new MicroStep(input));

        // Returning a new VM and the output
        return newPair(new PureVm(instructions, memOut.getFst()), memOut.getSnd());
    }

    private static class MicroStep implements Function2<Pair<Memory, Map<Integer, Double>>, Instruction, Pair<Memory, Map<Integer, Double>>> {

        private final Map<Integer, Double> input;

        private MicroStep(Map<Integer, Double> input) {
            this.input = input;
        }

        public Pair<Memory, Map<Integer, Double>> apply(Pair<Memory, Map<Integer, Double>> state, Instruction instruction) {
            return instruction.execute(0, state.getFst(), input, state.getSnd());
        }
    }
}