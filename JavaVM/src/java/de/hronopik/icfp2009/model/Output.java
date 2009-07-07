package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Pair;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public final class Output extends Port {

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public Output(int address, double value) {
        super(address, value);
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        return "Output[address=" + address + ", value=" + value + "]";
    }
}
