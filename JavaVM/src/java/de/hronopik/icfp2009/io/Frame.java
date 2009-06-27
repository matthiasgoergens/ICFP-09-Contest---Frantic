package de.hronopik.icfp2009.io;

import de.hronopik.icfp2009.model.Instruction;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public final class Frame {

    private final Instruction instruction;
    private final double value;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    Frame(Instruction instruction, double value) {
        this.instruction = instruction;
        this.value = value;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public Instruction getInstruction() {
        return instruction;
    }

    public double getValue() {
        return value;
    }
}
