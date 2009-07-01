package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Iterator<E> {

    Maybe<E> current();

    Iterator<E> next();
}
