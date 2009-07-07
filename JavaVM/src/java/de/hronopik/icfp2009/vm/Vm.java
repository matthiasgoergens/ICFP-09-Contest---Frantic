package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.util.Map;
import de.hronopik.icfp2009.util.Pair;


/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Vm<T extends Vm<T>> {

    int getStepIndex();

    Pair<T, Map<Integer, Double>> step();

    Pair<T, Map<Integer, Double>> step(Map<Integer, Double> input);
}
