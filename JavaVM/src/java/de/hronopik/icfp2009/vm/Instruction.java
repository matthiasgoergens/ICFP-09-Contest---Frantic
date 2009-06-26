package de.hronopik.icfp2009.vm;

import org.jetbrains.annotations.NotNull;

import java.util.Map;

/**
 * A 32-bit instruction.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
abstract class Instruction {

    final int address;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    Instruction(int address) {
        this.address = address;
    }

    public Instruction() {
    }

    /*
    * Executes the instruction.
    *
    * @param status the value of the status register
    * @param values the RAM
    * @return the new value of the status register
    */
    abstract boolean execute(boolean status, double[] values, @NotNull Map<Integer, Double> inputs,
                             @NotNull Map<Integer, Double> outputs);
}
