package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Map;
import de.hronopik.icfp2009.util.Pair;
import de.hronopik.icfp2009.vm.Memory;

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
     * @param memory    the memory of the VM
     * @param input     the input ports
     * @param output
     * @return the new memory and new output as result of the instruction execution
     */
    public abstract Pair<Memory, Map<Integer, Double>> execute(int stepIndex, Memory memory, Map<Integer, Double> input, Map<Integer, Double> output);
}
