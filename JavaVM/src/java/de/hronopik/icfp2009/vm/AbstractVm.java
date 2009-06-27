package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.io.Frame;
import de.hronopik.icfp2009.model.Instruction;
import org.jetbrains.annotations.NotNull;

import static java.util.Arrays.copyOf;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A VM.
 * <p/>
 * A VM isn't threadsafe!
 *
 * @author Alexander Kiel
 * @version $Id$
 */
abstract class AbstractVm {

    @NotNull
    final Instruction[] instructions;

    @NotNull
    double[] values;

    @NotNull
    final Map<String, Snapshoot> snapshoots = new HashMap<String, Snapshoot>();

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

    public int getStepIndex() {
        return stepIndex;
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

    /**
     * Creates a new snapshoot with the given name.
     *
     * @param snapshootName the name of the new snapshoot
     */
    public void createSnapshoot(@NotNull String snapshootName) {
        snapshoots.put(snapshootName, new Snapshoot(values, status, stepIndex));
    }

    /**
     * Resets the state of the VM to the state at the time of the snapshoot with the given name.
     *
     * @param snapshootName the name of the snapshoot to use
     * @throws IllegalArgumentException if there is no snapshoot with the given name
     */
    public void reset(@NotNull String snapshootName) {
        Snapshoot snapshoot = snapshoots.get(snapshootName);
        if (snapshoot == null) {
            throw new IllegalArgumentException("Unknown snapshoot with name \"" + snapshootName + "\".");
        }
        values = snapshoot.values;
        status = snapshoot.status;
        stepIndex = snapshoot.stepIndex;
    }

    //---------------------------------------------------------------------------------------------
    // Snapshoot
    //---------------------------------------------------------------------------------------------

    private static class Snapshoot {

        @NotNull
        private final double[] values;

        /**
         * The status register.
         */
        private final boolean status;

        private final int stepIndex;

        //---------------------------------------------------------------------------------------------
        // Constructor
        //---------------------------------------------------------------------------------------------

        private Snapshoot(@NotNull double[] values, boolean status, int stepIndex) {
            this.values = copyOf(values, values.length);
            this.status = status;
            this.stepIndex = stepIndex;
        }
    }
}
