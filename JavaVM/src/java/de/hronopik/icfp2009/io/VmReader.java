package de.hronopik.icfp2009.io;

import de.hronopik.icfp2009.util.AvlTree;
import de.hronopik.icfp2009.util.Map;

import java.io.*;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class VmReader extends FilterReader {

    private static final String STOP = ".";
    private static final char COMMENT_CHAR = '#';

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public VmReader(Reader in) {
        super(new LineNumberReader(in));
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------


    public Map<Integer, Double> readInputs() throws IOException {
        final LineNumberReader in = (LineNumberReader) this.in;

        Map<Integer, Double> inputs = AvlTree.empty();
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

            if (inputs.containsKey(address)) {
                throw new IOException("Double address " + address + " in line " + in.getLineNumber() + ".");
            }

            inputs = inputs.put(address, value);
        }

        if (stopExecution) {
            throw new EOFException("EOF");
        }

        return inputs;
    }
}
