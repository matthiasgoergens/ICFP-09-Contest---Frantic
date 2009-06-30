package de.hronopik.icfp2009.vm;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class OutputPort {

    private final int address;
    private final double value;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public OutputPort(int address, double value) {
        this.address = address;
        this.value = value;
    }

    //---------------------------------------------------------------------------------------------
    // Properties
    //---------------------------------------------------------------------------------------------

    public int getAddress() {
        return address;
    }

    public double getValue() {
        return value;
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        return "OutputPort[address=" + address + ", value=" + value + "]";
    }
}
