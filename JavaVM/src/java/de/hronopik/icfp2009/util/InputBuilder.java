package de.hronopik.icfp2009.util;

import org.jetbrains.annotations.NotNull;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Helper class which build inputs in an easy way.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
public class InputBuilder {

    @NotNull
    private final Map<Integer, Double> input = new LinkedHashMap<Integer, Double>();

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public InputBuilder() {
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    @NotNull
    public InputBuilder add(int inputPort, double value) {
        input.put(inputPort, value);
        return this;
    }

    @NotNull
    public Map<Integer, Double> build() {
        return new LinkedHashMap<Integer, Double>(input);
    }
}
