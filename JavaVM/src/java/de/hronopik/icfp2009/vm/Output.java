package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.util.Pair;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Output implements Pair<Integer, Double> {

    private final int address;
    private final double value;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public Output(int address, double value) {
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
    // Pair
    //---------------------------------------------------------------------------------------------

    public Integer getA() {
        return address;
    }

    public Double getB() {
        return value;
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        return "Output[address=" + address + ", value=" + value + "]";
    }
}
