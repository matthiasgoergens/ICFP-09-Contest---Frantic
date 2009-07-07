package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Set<E> extends Collection<E> {

    Set<E> add(E element);

    MaybeC<? extends Set<E>, E> add();

    Set<E> remove(E element);

    MaybeC<? extends Set<E>, E> remove();

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    <T> Set<T> map(Function1<E, T> mapper);

    Set<E> filter(Function1<E, Boolean> p);
}