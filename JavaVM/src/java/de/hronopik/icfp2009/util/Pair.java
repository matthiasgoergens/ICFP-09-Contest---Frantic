package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Pair<A, B> {

    /**
     * Returns the first element of this pair.
     *
     * @return the first element of this pair
     */
    A getFst();

    /**
     * Returns the second element of this pair.
     *
     * @return the second element of this pair
     */
    B getSnd();
}