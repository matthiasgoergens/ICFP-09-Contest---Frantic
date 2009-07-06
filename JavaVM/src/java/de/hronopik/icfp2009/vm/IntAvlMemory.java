package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.util.*;

/**
 * This class represents the memory of the VM.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
class IntAvlMemory implements Memory {

    private final IntAvlTree<Double> tree;

    private final boolean status;

    private final int insertPos;

    private final int size;

    /**
     * Creates a new memory.
     *
     * @param values a list of the values in the memory
     * @param status the value of the status register
     */
    IntAvlMemory(List.Element<Double> values, boolean status) {
        this(IntAvlTree.fromList(values), status, 0, values.size());
    }

    private IntAvlMemory(IntAvlTree<Double> tree, boolean status, int insertPos, int size) {
        this.tree = tree;
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

        return ((Maybe.Just<Double>) tree.get(address)).getValue();
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
    public IntAvlMemory setValue(final double value) {
        return new IntAvlMemory(tree.put(insertPos, value), status, advance(), size);
    }

    public IntAvlMemory setStatus(boolean status) {
        return new IntAvlMemory(tree, status, advance(), size);
    }

    public IntAvlMemory copy() {
        return new IntAvlMemory(tree, status, advance(), size);
    }

    private int advance() {
        return insertPos < size - 1 ? insertPos + 1 : 0;
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