package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.model.ROM;
import de.hronopik.icfp2009.util.LinkedList;
import de.hronopik.icfp2009.util.List;

/**
 * This class represents the memory of the VM.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
class ListMemory implements Memory {

    //private static long readTime = 0;
    //private static long writeTime = 0;
    //private static int step = 0;

    private final List<Double> left;

    private final List<Double> right;

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
    ListMemory(List.Element<Double> values, boolean status) {
        this(List.<Double>nil(), values.tail(), values.head(), status, 0, values.size());
    }

    private ListMemory(List<Double> left, List<Double> right, double value, boolean status, int insertPos, int size) {
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
            result = ((List.Element<Double>) left.drop(insertPos - address - 1)).head();
        } else if (address > insertPos) {
            result = ((List.Element<Double>) right.drop(address - 1 - insertPos)).head();
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
    public ListMemory setValue(final double value) {
        return advance(value, status);
    }

    public ListMemory setStatus(boolean status) {
        return advance(value, status);
    }

    public ListMemory copy() {
        return advance(value, status);
    }

    private ListMemory advance(final double value, final boolean status) {
        final ListMemory result;
        //long begin = System.nanoTime();
        if (right instanceof List.Element) {
            result = advance((List.Element<Double>) this.right, value, status);
        } else {
            result = advance((List.Nil<Double>) this.right, value, status);
        }
        //writeTime += System.nanoTime() - begin;
        return result;
    }

    private ListMemory advance(List.Element<Double> right, final double value, final boolean status) {
        return new ListMemory(new LinkedList<Double>(value, this.left)
                , right.tail()
                , right.head()
                , status
                , insertPos + 1
                , size
        );
    }

    private ListMemory advance(List.Nil<Double> right, final double value, final boolean status) {
        final LinkedList<Double> list = new LinkedList<Double>(value, this.left).reverse();
        return new ListMemory(right
                , list.tail()
                , list.head()
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
