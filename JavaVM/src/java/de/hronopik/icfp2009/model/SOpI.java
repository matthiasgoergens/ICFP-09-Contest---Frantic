package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Maybe;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface SOpI<P extends Maybe<P1>, P1 extends Parameter> extends Op {

    public abstract Instruction.Result execute(P param, int r1, ROM memory,
                                               InputPorts inputPorts);


    public abstract String toSemanticsString(P param, int r1, ROM memory, InputPorts inputPorts);
}
