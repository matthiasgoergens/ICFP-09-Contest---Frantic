package de.hronopik.icfp2009.model;

import org.jetbrains.annotations.NotNull;

import java.util.Map;

/**
 * A 32-bit instruction.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
public abstract class Instruction {

    final int address;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    Instruction(int address) {
        this.address = address;
    }

    /**
     * Executes the instruction.
     *
     * @param stepIndex the index of the current simulation step
     * @param status    the value of the status register
     * @param values    the RAM
     * @param inputs    the input register
     * @param outputs   the output register     @return the new value of the status register
     * @return the new status value
     */
    public abstract boolean execute(int stepIndex, boolean status, double[] values,
                                    @NotNull Map<Integer, Double> inputs, @NotNull Map<Integer, Double> outputs);
}
