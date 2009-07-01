package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Collection<E>  {

    boolean isEmpty();

    int size();

    boolean contains(E element);
}