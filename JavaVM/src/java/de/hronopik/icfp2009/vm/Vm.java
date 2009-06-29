package de.hronopik.icfp2009.vm;

import org.jetbrains.annotations.NotNull;

import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Vm {

    int getStepIndex();

    @NotNull
    Map<Integer, Double> step();

    @NotNull
    Map<Integer, Double> step(@NotNull Map<Integer, Double> inputs);
}
