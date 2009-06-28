package de.hronopik.icfp2009.io;

import de.hronopik.icfp2009.model.Instruction;
import org.jetbrains.annotations.NotNull;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public final class OrbitBinaryFrame {

    @NotNull
    private final Instruction instruction;
    private final double value;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    OrbitBinaryFrame(@NotNull Instruction instruction, double value) {
        this.instruction = instruction;
        this.value = value;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    @NotNull
    public Instruction getInstruction() {
        return instruction;
    }

    public double getValue() {
        return value;
    }
}
