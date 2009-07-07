package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Maybe;
import de.hronopik.icfp2009.util.Map;
import de.hronopik.icfp2009.vm.Memory;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface SOpI<P extends Maybe<P1>, P1 extends Parameter> extends Op {

    public abstract Memory execute(P param, int r1, Memory memory, Map<Integer, Double> input);

    public abstract String toSemanticsString(P param, int r1, Memory memory, Map<Integer, Double> input);
}
