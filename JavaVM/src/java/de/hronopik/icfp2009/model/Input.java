package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Pair;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public final class Input extends Port {

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public Input(int address, double value) {
        super(address, value);
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        return "Input[address=" + address + ", value=" + value + "]";
    }
}