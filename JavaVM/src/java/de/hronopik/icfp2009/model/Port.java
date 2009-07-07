package de.hronopik.icfp2009.model;

import de.hronopik.icfp2009.util.Pair;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
class Port implements Pair<Integer, Double> {

    final int address;
    final double value;

    Port(int address, double value) {
        this.address = address;
        this.value = value;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public int getAddress() {
        return address;
    }

    public double getValue() {
        return value;
    }

    //---------------------------------------------------------------------------------------------
    // Pair Implementation
    //---------------------------------------------------------------------------------------------

    public Integer getFst() {
        return address;
    }

    public Double getSnd() {
        return value;
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public final boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Output output = (Output) o;

        return address == output.address && Double.compare(output.value, value) == 0;
    }

    @Override
    public final int hashCode() {
        int result;
        long temp;
        result = address;
        temp = value != +0.0d ? Double.doubleToLongBits(value) : 0L;
        result = 31 * result + (int) (temp ^ (temp >>> 32));
        return result;
    }
}
