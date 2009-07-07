package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Iterable<E> {

    Iterator<E> iterator();

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    <T> Iterable<T> map(Function1<E, T> mapper);

    Iterable<E> filter(Function1<E, Boolean> p);

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    <T> T foldLeft(T start, Function2<T, E, T> f);

    <T> T foldRight(T start, Function2<E, T, T> f);
}
