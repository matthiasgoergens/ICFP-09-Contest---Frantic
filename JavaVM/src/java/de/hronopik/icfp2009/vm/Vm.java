package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.util.Map;
import de.hronopik.icfp2009.util.Pair;
import de.hronopik.icfp2009.model.InputPorts;


/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Vm {

    int getStepIndex();


    Map<Integer, Double> step();


    Map<Integer, Double> step(java.util.Map<Integer, Double> inputs);
}
