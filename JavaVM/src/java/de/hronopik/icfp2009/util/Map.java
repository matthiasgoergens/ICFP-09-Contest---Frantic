package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Map<K, V> extends Function1<K, Maybe<V>> {

    boolean isEmpty();

    int size();

    Maybe<V> get(K key);

    Map<K, V> add(Pair<K, V> mapping);

    <P extends Pair<K, V>> MaybeC<Map<K, V>, P> add();

    //---------------------------------------------------------------------------------------------
    // KeySet
    //---------------------------------------------------------------------------------------------

    Set<K> keySet();

    Set<Pair<K, V>> mappingSet();
}