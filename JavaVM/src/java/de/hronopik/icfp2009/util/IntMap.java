package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface IntMap<V> extends Collection<V> {

    Maybe<V> get(int key);

    IntMap<V> put(int key, V value);

}