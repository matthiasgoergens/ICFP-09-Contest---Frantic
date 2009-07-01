package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Maybe;
import de.hronopik.icfp2009.model.Output;

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
     * @param status    the status register
     * @param memory
     * @param inputPorts
     * @return the result of the instruction execution
     */
    public abstract Result execute(int stepIndex, boolean status, ROM memory, InputPorts inputPorts);

    //---------------------------------------------------------------------------------------------
    // Result
    //---------------------------------------------------------------------------------------------

    /**
     * A result of an instruction.
     */
    public interface Result {

        Maybe<Double> getMemoryValue();

        Maybe<Output> getOutput();

        Maybe<Boolean> getStatus();
    }
}
