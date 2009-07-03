package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.model.ROM;
import de.hronopik.icfp2009.util.LinkedList;
import de.hronopik.icfp2009.util.List;
import static de.hronopik.icfp2009.util.List.nil;
import de.hronopik.icfp2009.util.MaybeC;

/**
 * This class represents the memory of the VM.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
class Memory implements ROM {

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
    Memory(List.Element<Double> values, boolean status) {
        this.left = nil();
        this.right = values.tail().just();
        this.value = values.head().just();
        this.status = status;
        this.insertPos = 0;
        this.size = values.size();
    }

    private Memory(List<Double> left, List<Double> right, double value, boolean status, int insertPos, int size) {
        this.left = left;
        this.right = right;
        this.status = status;
        this.value = value;
        this.insertPos = insertPos;
        this.size = size;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public double getValue(final int address) {
        if (address < insertPos) {
            return left.drop(insertPos - address - 1).head().maybe(new FailContinuation(address));
        } else if (address > insertPos) {
            return right.drop(address - 1 - insertPos).head().maybe(new FailContinuation(address));
        } else {
            return value;
        }
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
    Memory setValue(final double value) {
        return advance(value, status);
    }

    Memory setStatus(boolean status) {
        return advance(value, status);
    }

    Memory copy() {
        return advance(value, status);
    }

    private Memory advance(final double value, final boolean status) {
        if (right instanceof List.Element) {
            return advance((List.Element<Double>) this.right, value, status);
        } else {
            return advance((List.Nil<Double>) this.right, value, status);
        }
    }

    private Memory advance(List.Element<Double> right, final double value, final boolean status) {
        return new Memory(new LinkedList<Double>(value, this.left)
                , right.tail().just()
                , right.head().just()
                , status
                , insertPos + 1
                , size
        );
    }

    private Memory advance(List.Nil<Double> right, final double value, final boolean status) {
        final LinkedList<Double> list = new LinkedList<Double>(value, this.left).reverse();
        return new Memory(right
                , list.tail().just()
                , list.head().just()
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

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    private class FailContinuation implements MaybeC<Double, Double> {

        private final int address;

        private FailContinuation(int address) {
            this.address = address;
        }

        public Double c(Double r) {
            return r;
        }

        public Double c() {
            throw new RuntimeException("Illegal memory access at address " + address +
                    ". insertPos  = " + insertPos +
                    ", left size  = " + left.size() +
                    ", right size = " + right.size()
            );
        }
    }
}
