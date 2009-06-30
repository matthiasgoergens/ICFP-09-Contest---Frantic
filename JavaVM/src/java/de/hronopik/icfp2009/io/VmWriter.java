package de.hronopik.icfp2009.io;

import org.jetbrains.annotations.NotNull;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class VmWriter extends FilterWriter {

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public VmWriter(Writer out) {
        super(out);
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public void writeOutputs(Map<Integer, Double> outputs) throws IOException {
        for (Map.Entry<Integer, Double> entry : outputs.entrySet()) {
            out.write(String.valueOf(entry.getKey()));
            out.write(" ");
            out.write(String.valueOf(entry.getValue()));
            out.write("\n");
        }
        out.write(".\n");
        out.flush();
    }
}
