package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Map<K, V> extends Set<Pair<K, V>>, Function1<K, Maybe<V>> {

    Maybe<V> get(K key);

    Map<K, V> put(K key, V value);

    Map<K, V> add(Pair<K, V> mapping);

    MaybeC<? extends Map<K, V>, Pair<K, V>> add();

    Map<K, V> remove(Pair<K, V> mapping);

    MaybeC<Map<K, V>, Pair<K, V>> remove();

    Map<K, V> removeKey(K key);

    MaybeC<Map<K, V>, K> removeKey();

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    <T> Set<T> map(Function1<Pair<K, V>, T> mapper);

    Map<K, V> filter(Function1<Pair<K, V>, Boolean> p);

    //---------------------------------------------------------------------------------------------
    // KeySet
    //---------------------------------------------------------------------------------------------

    //Set<K> keySet();
}