package de.hronopik.icfp2009.vm;

import org.jetbrains.annotations.NotNull;

import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Vm {

    int getStepIndex();


    Map<Integer, Double> step();


    Map<Integer, Double> step(Map<Integer, Double> inputs);
}
