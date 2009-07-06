package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.util.*;

/**
 * This class represents the memory of the VM.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
class AvlMemory implements Memory {

    private final AvlTree<Integer, Double> tree;

    private final boolean status;

    private final int insertPos;

    private final int size;

    /**
     * Creates a new memory.
     *
     * @param values a list of the values in the memory
     * @param status the value of the status register
     */
    AvlMemory(List.Element<Double> values, boolean status) {
        this(AvlTree.fromList(values), status, 0, values.size());
    }

    private AvlMemory(AvlTree<Integer, Double> tree, boolean status, int insertPos, int size) {
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
    public AvlMemory setValue(final double value) {
        return new AvlMemory(tree.put(insertPos, value), status, advance(), size);
    }

    public AvlMemory setStatus(boolean status) {
        return new AvlMemory(tree, status, advance(), size);
    }

    public AvlMemory copy() {
        return new AvlMemory(tree, status, advance(), size);
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