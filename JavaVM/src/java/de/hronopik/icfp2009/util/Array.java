package de.hronopik.icfp2009.util;

/**
 * An indexed collection.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Array<E> extends Collection<E> {

    /**
     * Returns the value at the given index.
     * 
     * @param index the index of the value to return
     * @return the value at the given index
     */
    @Complexity("O(1)")
    Maybe<E> get(int index);
}
