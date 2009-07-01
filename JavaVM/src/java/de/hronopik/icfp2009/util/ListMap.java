package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.List.nil;
import static de.hronopik.icfp2009.util.Maybe.just;
import static de.hronopik.icfp2009.util.Maybe.nothing;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class ListMap<K, V> implements Map<K, V> {

    private final List<Pair<K, V>> list;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public ListMap() {
        list = nil();
    }

    private ListMap(List<Pair<K, V>> list) {
        this.list = list;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public boolean isEmpty() {
        return list.isEmpty();
    }

    public int size() {
        return list.size();
    }

    public Maybe<V> get(K key) {
        return list.filter(new NodeSearcher<K, V>(key)).head().maybe(toValue());
    }

    public Map<K, V> add(Pair<K, V> mapping) {
        return new ListMap<K, V>(new TailElement<Pair<K, V>>(mapping, list.remove(mapping)));
    }

    public <P extends Pair<K, V>> MaybeC<Map<K, V>, P> add() {
        return new MaybeC<Map<K, V>, P>() {

            public Map<K, V> c(P r) {
                return add(r);
            }

            public Map<K, V> c() {
                return ListMap.this;
            }
        };
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public Set<K> keySet() {
        return list.map(new Function1<Pair<K, V>, K>() {
            public K apply(Pair<K, V> kvPair) {
                return kvPair.getA();
            }
        });
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public Maybe<V> apply(K k) {
        return get(k);
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    private MaybeC<Maybe<V>, Pair<K, V>> toValue() {
        return new MaybeC<Maybe<V>, Pair<K, V>>() {
            public Maybe<V> c(Pair<K, V> r) {
                return just(r.getB());
            }

            public Maybe<V> c() {
                return nothing();
            }
        };
    }

    //---------------------------------------------------------------------------------------------
    // NodeSearcher
    //---------------------------------------------------------------------------------------------

    private static class NodeSearcher<K, V> implements Function1<Pair<K, V>, Boolean> {

        private final K key;

        private NodeSearcher(K key) {
            this.key = key;
        }

        public Boolean apply(Pair<K, V> node) {
            return key.equals(node.getA());
        }
    }
}
