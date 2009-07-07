package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Collection<E> extends Iterable<E> {

    boolean isEmpty();

    int size();

    boolean contains(E element);

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    <T> Collection<T> map(Function1<E, T> mapper);

    Collection<E> filter(Function1<E, Boolean> p);
}