package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.io.Frame;
import de.hronopik.icfp2009.io.Frames;
import de.hronopik.icfp2009.util.Pair;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class StreamVm extends AbstractVm implements Runnable {

    private static final String STOP = ".";
    private static final char COMMENT_CHAR = '#';

    @NotNull
    private final LineNumberReader in;

    @NotNull
    private final Writer out;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public StreamVm(@NotNull List<Frame> frames, @NotNull Reader in, @NotNull Writer out) {
        super(frames);
        this.in = new LineNumberReader(in);
        this.out = out;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public void run() {
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
        Pair<Map<Integer, Double>, Boolean> readedInputs = readInputs();
        if (readedInputs.getB()) {
            return false;
        }

        Map<Integer, Double> outputs = step(readedInputs.getA());

        writeOutputs(outputs);

        return outputs.get(0) == 0;
    }

    //---------------------------------------------------------------------------------------------
    // Helper Methods
    //---------------------------------------------------------------------------------------------

    @NotNull
    private Pair<Map<Integer, Double>, Boolean> readInputs() throws IOException {
        Map<Integer, Double> inputs = new HashMap<Integer, Double>();
        boolean stopExecution = true;

        String line;
        while ((line = in.readLine()) != null) {

            // Finish one input
            if (STOP.equals(line)) {
                stopExecution = false;
                break;
            }

            // Skip Comments 
            if (line.charAt(0) == COMMENT_CHAR) {
                continue;
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
        return new Pair<Map<Integer, Double>, Boolean>(inputs, stopExecution);
    }

    private void writeOutputs(@NotNull Map<Integer, Double> outputs) throws IOException {
        for (Map.Entry<Integer, Double> entry : outputs.entrySet()) {
            out.write(String.valueOf(entry.getKey()));
            out.write(" ");
            out.write(String.valueOf(entry.getValue()));
            out.write("\n");
        }
        out.write(".\n");
        out.flush();
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

        StreamVm vm = new StreamVm(frames, in, out);

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
    private static List<Frame> readFrames(@NotNull File file) {
        try {
            return Frames.readFromFile(file);
        } catch (IOException e) {
            System.err.println("Problem while reading from file " + file + ". " + e.getMessage());
            return null;
        }
    }
}
