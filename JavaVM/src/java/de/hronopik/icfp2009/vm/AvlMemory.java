package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.model.ROM;
import de.hronopik.icfp2009.util.*;

/**
 * This class represents the memory of the VM.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
class AvlMemory implements ROM {

    //private static long readTime = 0;
    //private static long writeTime = 0;
    //private static int step = 0;

    private final AvlTree<Integer, Double> left;

    private final AvlTree<Integer, Double> right;

    private final double value;

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
        this(AvlTree.fromList(List.<Double>nil()), AvlTree.fromList(values), values.head(), status, 0,
                values.size());
    }

    private AvlMemory(AvlTree<Integer, Double> left, AvlTree<Integer, Double> right, double value,
                      boolean status, int insertPos, int size) {
        this.left = left;
        this.right = right;
        this.status = status;
        this.value = value;
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
        if (address < insertPos) {
            result = ((Maybe.Just<Double>) left.get(address)).getValue();
        } else if (address > insertPos) {
            result = ((Maybe.Just<Double>) right.get(address)).getValue();
        } else {
            result = value;
        }
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
    AvlMemory setValue(final double value) {
        return advance(value, status);
    }

    AvlMemory setStatus(boolean status) {
        return advance(value, status);
    }

    AvlMemory copy() {
        return advance(value, status);
    }

    private AvlMemory advance(final double value, final boolean status) {
        final AvlMemory result;
        //long begin = System.nanoTime();
        if (insertPos < size - 1) {
            result = advance1(this.right, value, status);
        } else {
            result = advance2(value, status);
        }
        //writeTime += System.nanoTime() - begin;
        return result;
    }

    private AvlMemory advance1(AvlTree<Integer, Double> right, final double value, final boolean status) {
        return new AvlMemory(left.put(insertPos, value)
                , right
                , ((Maybe.Just<Double>) right.get(insertPos + 1)).getValue()
                , status
                , insertPos + 1
                , size
        );
    }

    private AvlMemory advance2(final double value, final boolean status) {
        final AvlTree.Node<Integer, Double> tree = left.put(insertPos, value);
        return new AvlMemory(AvlTree.<Integer, Double>empty()
                , tree
                , ((Maybe.Just<Double>) tree.get(0)).getValue()
                , status
                , 0
                , size
        );
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