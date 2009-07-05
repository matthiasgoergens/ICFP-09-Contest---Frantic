package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.Maybe.nothing;

import java.util.Arrays;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class ReadOnlyArray<E> implements Array<E> {

    private final Object[] values;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public ReadOnlyArray(Collection<? super E> collection) {
        values = new Object[collection.size()];

        Iterator<? super E> iterator = collection.iterator();
        Maybe<? super E> maybe;
        for (int i = 0; (maybe = iterator.current()).isJust(); i++) {
            values[i] = ((Maybe.Just<E>) maybe).getValue();
            iterator = iterator.next();
        }
    }

    private ReadOnlyArray(Object[] values) {
        this.values = values;
    }

    //---------------------------------------------------------------------------------------------
    // Array Implementation
    //---------------------------------------------------------------------------------------------

    public Maybe<E> get(int index) {
        if (index >= 0 && index < values.length) {
            //noinspection unchecked
            return Maybe.just((E) values[index]);
        } else {
            return nothing();
        }
    }

    //---------------------------------------------------------------------------------------------
    // Collection Implementation
    //---------------------------------------------------------------------------------------------

    @Complexity("O(1)")
    public boolean isEmpty() {
        return size() == 0;
    }

    @Complexity("O(1)")
    public int size() {
        return values.length;
    }

    @Complexity("O(n)")
    public boolean contains(E element) {
        for (Object value : values) {
            if (value.equals(element)) {
                return true;
            }
        }
        return false;
    }

    @Complexity("O(n)")
    public <T> Collection<T> map(Function1<E, T> mapper) {
        Object[] newValues = new Object[values.length];
        for (int i = 0; i < values.length; i++) {
            //noinspection unchecked
            newValues[i] = mapper.apply((E) values[i]);
        }
        return new ReadOnlyArray<T>(newValues);
    }

    @Complexity("O(n)")
    public Collection<E> filter(Function1<E, Boolean> p) {
        Object[] newValues = new Object[values.length];
        int i = 0;
        //noinspection unchecked
        for (E value : (E[]) values) {
            if (p.apply(value)) {
                newValues[i++] = value;
            }
        }
        return new ReadOnlyArray<E>(Arrays.copyOf(newValues, i));
    }

    //---------------------------------------------------------------------------------------------
    // Iterable Implementation
    //---------------------------------------------------------------------------------------------

    public Iterator<E> iterator() {
        return new ArrayIterator(0);
    }

    public <T> T foldLeft(T start, Function2<T, E, T> f) {
        //TODO: Implement
        return null;
    }

    public <T> T foldRight(T start, Function2<E, T, T> f) {
        //TODO: Implement
        return null;
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ReadOnlyArray that = (ReadOnlyArray) o;

        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        if (!Arrays.equals(values, that.values)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(values);
    }

    @Override
    public String toString() {
        return "ReadOnlyArray(" + Arrays.asList(values) + ")";
    }

    //---------------------------------------------------------------------------------------------
    // ArrayIterator
    //---------------------------------------------------------------------------------------------

    private class ArrayIterator implements Iterator<E> {

        private final int index;

        private ArrayIterator(int index) {
            this.index = index;
        }

        public Maybe<E> current() {
            return get(index);
        }

        public Iterator<E> next() {
            return new ArrayIterator(index + 1);
        }
    }
}
