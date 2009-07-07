package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Function2<A, B, C> {

    C apply(A a, B b);
}