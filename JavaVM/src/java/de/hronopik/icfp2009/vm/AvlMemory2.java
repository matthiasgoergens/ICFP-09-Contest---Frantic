package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.model.ROM;
import de.hronopik.icfp2009.util.*;

/**
 * This class represents the memory of the VM.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
class AvlMemory2 implements Memory {

    //private static long readTime = 0;
    //private static long writeTime = 0;
    //private static int step = 0;

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
    AvlMemory2(List.Element<Double> values, boolean status) {
        this(AvlTree.fromList(values), status, 0, values.size());
    }

    private AvlMemory2(AvlTree<Integer, Double> tree, boolean status, int insertPos, int size) {
        this.tree = tree;
        this.status = status;
        this.insertPos = insertPos;
        this.size = size;
        /*if (insertPos == 0) {
            step++;
            if (step == 60000) {
                System.err.println("readTime  = " + (readTime / 1000) + " ms");
                System.err.println("writeTime = " + (writeTime / 1000) + " ms");
            }
        }*/
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public double getValue(final int address) {
        if (address < 0 || address >= size) {
            throw new IndexOutOfBoundsException("address = " + address + "; size = " + size);
        }

        final double result;
        //long begin = System.nanoTime();
        result = ((Maybe.Just<Double>) tree.get(address)).getValue();
        //readTime += System.nanoTime() - begin;
        return result;
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
    public AvlMemory2 setValue(final double value) {
        return new AvlMemory2(tree.put(insertPos, value), status, advance(), size);
    }

    public AvlMemory2 setStatus(boolean status) {
        return new AvlMemory2(tree, status, advance(), size);
    }

    public AvlMemory2 copy() {
        return new AvlMemory2(tree, status, advance(), size);
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