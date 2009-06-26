package de.hronopik.icfp2009.vm;

/**
 * Operation for S-Type instructions.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.3 p.4"
 */
public enum SOp implements Op {

    // Do not change the order of the operations!

    Noop, Cmpz, Sqrt, Copy, Input;

    /**
     * Returns the operation according to the given opcode.
     *
     * @param opcode the opcode of the operation to return
     * @return the operation
     */
    public static SOp fromOpcode(int opcode) {
        return values()[opcode];
    }
}
