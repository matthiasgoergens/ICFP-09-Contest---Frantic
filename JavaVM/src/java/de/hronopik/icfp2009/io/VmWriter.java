package de.hronopik.icfp2009.io;

import de.hronopik.icfp2009.util.Function2;
import static de.hronopik.icfp2009.util.LinkedList.fromCollection;
import static de.hronopik.icfp2009.util.Lists.sort;
import de.hronopik.icfp2009.util.Map;
import de.hronopik.icfp2009.util.Pair;
import de.hronopik.icfp2009.util.Pairs;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.Writer;

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
        out.write(sort(fromCollection(outputs), Pairs.<Integer>fstComparator()).foldLeft(new StringBuilder(),
                new Function2<StringBuilder, Pair<Integer, Double>, StringBuilder>() {
                    public StringBuilder apply(StringBuilder s, Pair<Integer, Double> mapping) {
                        return s.append(String.valueOf(mapping.getFst()))
                                .append(" ")
                                .append(String.valueOf(mapping.getSnd()))
                                .append("\n");
                    }
                }
        ).append(".\n").toString());
        out.flush();
    }
}
