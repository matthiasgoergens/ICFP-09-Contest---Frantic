package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.io.OrbitBinaryFrame;
import de.hronopik.icfp2009.model.InputPorts;
import de.hronopik.icfp2009.model.Instruction;
import de.hronopik.icfp2009.model.Output;
import de.hronopik.icfp2009.model.RAM;
import de.hronopik.icfp2009.util.*;

import static java.lang.System.arraycopy;
import static java.util.Arrays.copyOf;
import java.util.HashMap;

/**
 * A VM.
 * <p/>
 * A VM isn't threadsafe!
 *
 * @author Alexander Kiel
 * @version $Id$
 */
abstract class AbstractVm implements Vm {


    private final Instruction[] instructions;


    private final Memory memory;

    /**
     * The status register.
     */
    StatusRegister status = new StatusRegister(false);

    private int stepIndex = 0;


    final java.util.Map<String, Snapshoot> snapshoots = new HashMap<String, Snapshoot>();

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    AbstractVm(java.util.List<OrbitBinaryFrame> frames) {
        this.instructions = new Instruction[frames.size()];
        this.memory = new Memory(frames.size());
        int i = 0;
        for (OrbitBinaryFrame frame : frames) {
            instructions[i] = frame.getInstruction();
            memory.setValue(i, frame.getValue());
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

    public void setInput() {

    }

    /**
     * Performes a step with empty input.
     *
     * @return the output
     */

    public Map<Integer, Double> step() {
        return step(EmptyInputPorts.EMPTY_INPUT);
    }


    public Map<Integer, Double> step(java.util.Map<Integer, Double> inputs) {
        return step(new MapBackedInputPorts(inputs));
    }

    public Map<Integer, Double> step(InputPorts inputPorts) {
        Map<Integer, Double> outputs = new ListMap<Integer, Double>();

        for (int i = 0; i < instructions.length; i++) {
            Instruction.Result result = instructions[i].execute(stepIndex, memory, inputPorts);
            outputs = result.cont(new ResultContuniation(outputs, i));

            /*memory.setValue(i, result.getMemoryValue().maybe(Maybe.<Double>idC(), memory.getValueC(i)));
            status = new StatusRegister(result.getStatus().maybe(Maybe.<Boolean>idC(), status));
            outputs = result.getOutput().maybe(outputs.add());*/
        }

        outputs.get(0).maybe(CRASH_DETECTION);

        stepIndex++;

        return outputs;
    }

    private class ResultContuniation implements Instruction.ResultC<Map<Integer, Double>> {

        private final Map<Integer, Double> outputs;
        private final int destAddress;

        private ResultContuniation(Map<Integer, Double> outputs, int destAddress) {
            this.outputs = outputs;
            this.destAddress = destAddress;
        }

        public Map<Integer, Double> memoryResult(double value) {
            memory.setValue(destAddress, value);
            return outputs;
        }

        public Map<Integer, Double> outputResult(Output output) {
            return outputs.add(output);
        }

        public Map<Integer, Double> statusResult(boolean value) {
            status = new StatusRegister(value);
            return outputs;
        }

        public Map<Integer, Double> noopResult() {
            return outputs;
        }
    }

    private static final MaybeC<Double, Double> CRASH_DETECTION = new MaybeC<Double, Double>() {
        public Double c(Double r) {
            if (r == -1) {
                throw new RuntimeException("CRASHED");
            }
            return r;
        }

        public Double c() {
            throw new RuntimeException("Score output missing.");
        }
    };

    /**
     * Creates a new snapshoot with the given name.
     *
     * @param snapshootName the name of the new snapshoot
     */
    public void createSnapshoot(String snapshootName) {
        snapshoots.put(snapshootName, new Snapshoot(memory.values, status.isValue(), stepIndex));
    }

    /**
     * Resets the state of the VM to the state at the time of the snapshoot with the given name.
     *
     * @param snapshootName the name of the snapshoot to use
     * @throws IllegalArgumentException if there is no snapshoot with the given name
     */
    public void reset(String snapshootName) {
        Snapshoot snapshoot = snapshoots.get(snapshootName);
        if (snapshoot == null) {
            throw new IllegalArgumentException("Unknown snapshoot with name \"" + snapshootName + "\".");
        }
        /*
         * Patrick: ok this copyOf ensures that you can use a snapshoot twice. You are right, if you reset to a
         * snapshoot, let the VM run a step and try to reset again, it will fail without this copyOf. So better I had
         * removed snapshoots from the map after resetting.
         */
        memory.restoreSnapshoot(snapshoot);
        status = new StatusRegister(snapshoot.status);
        stepIndex = snapshoot.stepIndex;
    }

    //---------------------------------------------------------------------------------------------
    // Memory
    //---------------------------------------------------------------------------------------------

    private static class Memory implements RAM {

        private final double[] values;

        //---------------------------------------------------------------------------------------------
        // Constructor
        //---------------------------------------------------------------------------------------------

        private Memory(int size) {
            this.values = new double[size];
        }

        //---------------------------------------------------------------------------------------------
        //
        //---------------------------------------------------------------------------------------------

        public boolean isStatus() {
            return false;
        }

        public double getValue(int address) {
            if (address >= values.length) {
                throw new IndexOutOfBoundsException("Index: " + address + " Size: " + values.length);
            }
            return values[address];
        }

        public NothingC<Double> getValueC(final int address) {
            return new NothingC<Double>() {
                public Double c() {
                    return values[address];
                }
            };
        }

        public void setValue(int address, double value) {
            if (address >= values.length) {
                throw new IndexOutOfBoundsException("Index: " + address + " Size: " + values.length);
            }
            values[address] = value;
        }

        //---------------------------------------------------------------------------------------------
        //
        //---------------------------------------------------------------------------------------------

        private void restoreSnapshoot(Snapshoot snapshoot) {
            arraycopy(snapshoot.values, 0, values, 0, snapshoot.values.length);
        }
    }

    //---------------------------------------------------------------------------------------------
    // StatusRegister
    //---------------------------------------------------------------------------------------------

    private static class StatusRegister implements NothingC<Boolean> {

        private final boolean value;

        private StatusRegister(boolean value) {
            this.value = value;
        }

        public boolean isValue() {
            return value;
        }

        public Boolean c() {
            return value;
        }
    }

    //---------------------------------------------------------------------------------------------
    // InputPorts
    //---------------------------------------------------------------------------------------------

    private static class MapBackedInputPorts implements InputPorts {


        private final java.util.Map<Integer, Double> map;

        //---------------------------------------------------------------------------------------------
        // Constructor
        //---------------------------------------------------------------------------------------------

        private MapBackedInputPorts(java.util.Map<Integer, Double> map) {
            this.map = new HashMap<Integer, Double>(map);
        }

        //---------------------------------------------------------------------------------------------
        //
        //---------------------------------------------------------------------------------------------

        public double getValue(int address) {
            Double value = map.get(address);
            return value == null ? 0d : value;
        }
    }

    //---------------------------------------------------------------------------------------------
    // Snapshoot
    //---------------------------------------------------------------------------------------------

    private static class Snapshoot {


        private final double[] values;

        /**
         * The status register.
         */
        private final boolean status;

        private final int stepIndex;

        //---------------------------------------------------------------------------------------------
        // Constructor
        //---------------------------------------------------------------------------------------------

        private Snapshoot(double[] values, boolean status, int stepIndex) {
            /*TODO: Buy a present for patrick since it took him an hour to find this crap..
               AAHHHH. WTF?
               I added a copyOf here because when I create a vm and do a snapshot and it it is not copied
               then every change in the vm causes a change in these values here...
               After reloading the snap shot I get exactly the last state of the vm with status and
               stepIndex reseted to the snapshot-point.
               I really don't know why all your test run fine.
               Patrick
                */
            /*
             * Patrick: you did not add this copyOf here, you did add a copyOf at reset - please look into the history
             */
            this.values = copyOf(values, values.length);
            this.status = status;
            this.stepIndex = stepIndex;
        }
    }
}
