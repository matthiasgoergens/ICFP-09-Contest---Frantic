package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.io.Frames;
import de.hronopik.icfp2009.io.OrbitBinaryFrame;
import de.hronopik.icfp2009.io.VmReader;
import de.hronopik.icfp2009.io.VmWriter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.List;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class StreamVm extends AbstractVm implements Runnable {

    @NotNull
    private final VmReader in;

    @NotNull
    private final VmWriter out;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public StreamVm(@NotNull List<OrbitBinaryFrame> frames, @NotNull Reader in, @NotNull Writer out) {
        super(frames);
        this.in = new VmReader(in);
        this.out = new VmWriter(out);
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
        Map<Integer, Double> inputs = in.readInputs();

        Map<Integer, Double> outputs = step(inputs);

        out.writeOutputs(outputs);

        return outputs.get(0) == 0;
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

        List<OrbitBinaryFrame> frames = readFrames(new File(args[0]));
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
    private static List<OrbitBinaryFrame> readFrames(@NotNull File file) {
        try {
            return Frames.readFromFile(file);
        } catch (IOException e) {
            System.err.println("Problem while reading from file " + file + ". " + e.getMessage());
            return null;
        }
    }
}
