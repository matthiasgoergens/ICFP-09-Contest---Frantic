package de.hronopik.icfp2009.vm;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
final class Frame {

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
