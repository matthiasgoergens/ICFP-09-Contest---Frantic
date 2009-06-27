package de.hronopik.icfp2009.vm;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class OrbitVm implements Runnable {

    private static final Logger logger = Logger.getLogger(OrbitVm.class.getName());
    private static final String STOP = ".";

    @NotNull
    private final LineNumberReader in;

    @NotNull
    private final Writer out;

    @NotNull
    private final Instruction[] instructions;

    @NotNull
    private final double[] values;

    /**
     * The instruction pointer.
     */
    private int pc = 0;

    /**
     * The status register.
     */
    private boolean status = false;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public OrbitVm(@NotNull List<Frame> frames, @NotNull Reader in, @NotNull Writer out) {
        this.instructions = new Instruction[frames.size()];
        this.values = new double[frames.size()];
        int i = 0;
        for (Frame frame : frames) {
            instructions[i] = frame.getInstruction();
            values[i] = frame.getValue();
            i++;
        }
        this.in = new LineNumberReader(in);
        this.out = out;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public void run() {

        /*int i = 0;
        for (double value : values) {
            System.err.println(i++ + ": " + String.valueOf(value));
        }*/

        boolean running = true;
        while (running) {
            try {
                running = step();
            } catch (IOException e) {
                System.err.println(e.getMessage());
                break;
            }
        }
    }

    /**
     * Executes one step in the simulation process.
     * <p/>
     * <ul><li>reads the inputs<li>executes all instructions<li>writes the outputs
     *
     * @return {@code true} if the simulation should continue
     * @throws IOException if an I/O error occurs
     */
    public boolean step() throws IOException {
        Map<Integer, Double> inputs = readInputs();

        Map<Integer, Double> outputs = step(inputs);

        writeOutputs(outputs);

        return outputs.get(0) == 0;
    }

    @NotNull
    private Map<Integer, Double> step(@NotNull Map<Integer, Double> inputs) {
        Map<Integer, Double> outputs = new HashMap<Integer, Double>();

        for (Instruction instruction : instructions) {
            status = instruction.execute(status, values, inputs, outputs);
        }

        return outputs;
    }

    //---------------------------------------------------------------------------------------------
    // Helper Methods
    //---------------------------------------------------------------------------------------------

    @NotNull
    private Map<Integer, Double> readInputs() throws IOException {
        Map<Integer, Double> inputs = new HashMap<Integer, Double>();
        String line;
        while ((line = in.readLine()) != null) {
            if (STOP.equals(line)) {
                break;
            }
            String[] parts = line.split(" ");

            int address;
            try {
                address = Integer.parseInt(parts[0]);
            } catch (NumberFormatException e) {
                throw new IOException("Unable to parse the address \"" + parts[0] + "\" in line " + in.getLineNumber() +
                        ".");
            }

            double value;
            try {
                value = Double.parseDouble(parts[1]);
            } catch (NumberFormatException e) {
                throw new IOException("Unable to parse the value \"" + parts[1] + "\" in line " + in.getLineNumber() +
                        ".");
            }

            if (inputs.put(address, value) != null) {
                throw new IOException("Double address " + address + " in line " + in.getLineNumber() + ".");
            }
        }
        return inputs;
    }

    private void writeOutputs(@NotNull Map<Integer, Double> outputs) throws IOException {
        for (Map.Entry<Integer, Double> entry : outputs.entrySet()) {
            out.write(String.valueOf(entry.getKey()));
            out.write(" ");
            out.write(String.valueOf(entry.getValue()));
            out.write("\n");
            out.flush();
        }
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    /**
     * Executes the orbit VM.
     *
     * @param args the first argument is the binary file to execute
     */
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Expect one argument, the binary to execute.");
            System.exit(1);
            return;
        }

        List<Frame> frames = readFrames(new File(args[0]));
        if (frames == null) {
            System.exit(1);
        }

        InputStreamReader in = new InputStreamReader(System.in);
        OutputStreamWriter out = new OutputStreamWriter(System.out);

        OrbitVm vm = new OrbitVm(frames, in, out);

        try {
            vm.run();
        } finally {
            try {
                in.close();
            } catch (IOException e) {
            }
            try {
                out.close();
            } catch (IOException e) {
            }
        }
    }

    @Nullable
    private static List<Frame> readFrames(File file) {
        FileInputStream fileIn;
        try {
            fileIn = new FileInputStream(file);
        } catch (FileNotFoundException e) {
            System.err.println("File " + file + " not found.");
            return null;
        }

        OrbitInputStream in = new OrbitInputStream(fileIn);
        List<Frame> frames = new ArrayList<Frame>();
        try {
            while (true) {
                frames.add(in.readFrame());
            }
        } catch (EOFException e) {
            return frames;
        } catch (IOException e) {
            System.err.println("File " + file + " not found.");
            return null;
        }
    }
}
