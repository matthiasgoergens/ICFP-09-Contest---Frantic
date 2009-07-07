package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.util.List;

/**
 * This class represents the memory of the VM.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
class ArrayMemory implements Memory {

    private final double[] array;

    private final boolean status;

    private final int insertPos;

    private final int size;

    /**
     * Creates a new memory.
     *
     * @param values a list of the values in the memory
     * @param status the value of the status register
     */
    ArrayMemory(List.Element<Double> values, boolean status) {
        this(toArray(values), status, 0, values.size());
    }

    private static double[] toArray(List.Element<Double> values) {
        final Double[] doubles = values.toArray(new Double[]{});
        final double[] result = new double[doubles.length];
        for (int i = 0; i < doubles.length; i++) {
            result[i] = doubles[i];
        }
        return result;
    }

    private ArrayMemory(double[] array, boolean status, int insertPos, int size) {
        this.array = array;
        this.status = status;
        this.insertPos = insertPos;
        this.size = size;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public double getValue(final int address) {
        if (address < 0 || address >= size) {
            throw new IndexOutOfBoundsException("address = " + address + "; size = " + size);
        }

        return array[address];
    }

    public boolean isStatus() {
        return status;
    }

    /**
     * Sets the given value at the insert position.
     *
     * @param value the value to set
     * @return a new memory instance
     */
    public ArrayMemory setValue(final double value) {
        double[] newArray = new double[array.length];
        System.arraycopy(array, 0, newArray, 0, array.length);
        newArray[insertPos] = value;
        return new ArrayMemory(newArray, status, advance(), size);
    }

    public ArrayMemory setStatus(boolean status) {
        return new ArrayMemory(array, status, advance(), size);
    }

    public ArrayMemory copy() {
        return new ArrayMemory(array, status, advance(), size);
    }

    private int advance() {
        final int nextPos = insertPos + 1;
        return nextPos == size ? 0 : nextPos;
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < size; i++) {
            if (i != 0) {
                sb.append(',');
            }
            sb.append(getValue(i));
        }
        return sb.append(']').toString();
    }
}