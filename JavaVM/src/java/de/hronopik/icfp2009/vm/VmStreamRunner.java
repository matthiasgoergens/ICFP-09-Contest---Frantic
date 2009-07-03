package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.io.Frames;
import de.hronopik.icfp2009.io.OrbitBinaryFrame;
import de.hronopik.icfp2009.io.VmReader;
import de.hronopik.icfp2009.io.VmWriter;
import de.hronopik.icfp2009.util.Map;
import de.hronopik.icfp2009.util.Continuations;
import de.hronopik.icfp2009.util.Pair;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.List;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class VmStreamRunner {

    private PureVm vm;

    private final VmReader in;

    private final VmWriter out;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public VmStreamRunner(List<OrbitBinaryFrame> frames, Reader in, Writer out) {
        this.vm = new PureVm(frames);
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
                running = ioStep();
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
     * @throws java.io.IOException if an I/O error occurs
     */
    public boolean ioStep() throws IOException {
        final Pair<PureVm,Map<Integer,Double>> state = vm.step(in.readInputs());

        vm = state.getFst();
        out.writeOutputs(state.getSnd());

        final Double score = state.getSnd().get(0).maybe(Continuations.<Double>fail("score missing"));
        if (score == -1) {
            throw new RuntimeException("CRASHED");
        }
        return score == 0;
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

        VmStreamRunner vm = new VmStreamRunner(frames, in, out);

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
    private static List<OrbitBinaryFrame> readFrames(File file) {
        try {
            return Frames.readFromFile(file);
        } catch (IOException e) {
            System.err.println("Problem while reading from file " + file + ". " + e.getMessage());
            return null;
        }
    }
}