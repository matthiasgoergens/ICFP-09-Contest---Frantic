package de.hronopik.icfp2009.vm;

/**
 * Operation for D-Type instructions.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.2 p.3"
 */
public enum DOp implements Op {

    // Do not change the order of the operations!

    Add, Sub, Mult, Div, Output, Phi;

    /**
     * Returns the operation according to the given opcode.
     *
     * @param opcode the opcode of the operation to return
     * @return the operation
     */
    public static DOp fromOpcode(int opcode) {
        return values()[opcode - 1];
    }
}
