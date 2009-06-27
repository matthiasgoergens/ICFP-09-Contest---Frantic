package de.hronopik.icfp2009.vm;

import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
abstract class AbstractVm {

    @NotNull
    final Instruction[] instructions;

    @NotNull
    final double[] values;

    /**
     * The status register.
     */
    boolean status = false;

    private int stepIndex = 0;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    AbstractVm(@NotNull List<Frame> frames) {
        this.instructions = new Instruction[frames.size()];
        this.values = new double[frames.size()];
        int i = 0;
        for (Frame frame : frames) {
            instructions[i] = frame.getInstruction();
            values[i] = frame.getValue();
            i++;
        }
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    @NotNull
    public Map<Integer, Double> step(@NotNull Map<Integer, Double> inputs) {
        Map<Integer, Double> outputs = new HashMap<Integer, Double>();

        for (Instruction instruction : instructions) {
            status = instruction.execute(stepIndex, status, values, inputs, outputs);
        }

        stepIndex++;

        return outputs;
    }
}
