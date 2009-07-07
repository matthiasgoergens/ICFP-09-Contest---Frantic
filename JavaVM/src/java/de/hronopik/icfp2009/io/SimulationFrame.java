package de.hronopik.icfp2009.io;

import org.jetbrains.annotations.NotNull;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public final class SimulationFrame {

    private final int timeStep;


    private final Map<Integer, Double> inputs;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    SimulationFrame(int timeStep, Map<Integer, Double> inputs) {
        this.timeStep = timeStep;
        this.inputs = new LinkedHashMap<Integer, Double>(inputs);
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public int getTimeStep() {
        return timeStep;
    }


    public Map<Integer, Double> getInputs() {
        return new LinkedHashMap<Integer, Double>(inputs);
    }
}