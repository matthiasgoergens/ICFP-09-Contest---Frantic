package de.hronopik.icfp2009.model;

import org.jetbrains.annotations.NotNull;

import java.util.Map;

import de.hronopik.icfp2009.util.Maybe;
import de.hronopik.icfp2009.vm.OutputPort;

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
     * @param outputs   the output register
     * @return the result of the instruction execution
     */
    public abstract Result execute(int stepIndex, boolean status, ROM memory,
                                   InputPorts inputPorts, Map<Integer, Double> outputs);

    //---------------------------------------------------------------------------------------------
    // Result
    //---------------------------------------------------------------------------------------------

    /**
     * A result of an instruction.
     */
    public interface Result {

        Maybe<Double> getMemoryValue();

        Maybe<OutputPort> getOutputAssignment();

        Maybe<Boolean> getStatus();
    }
}
