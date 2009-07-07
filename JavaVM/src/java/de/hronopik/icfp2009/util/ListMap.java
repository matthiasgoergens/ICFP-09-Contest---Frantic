package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.Maybe.just;
import static de.hronopik.icfp2009.util.Maybe.nothing;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class ListMap<K, V> implements Map<K, V> {

    private final Set<Pair<K, V>> set;

    //---------------------------------------------------------------------------------------------
    // Constructors
    //---------------------------------------------------------------------------------------------

    public static <K, V> Map<K, V> emptyMap() {
        return new ListMap<K,V>();
    }

    public ListMap() {
        set = new ListSet<Pair<K,V>>();
    }

    public ListMap(Pair<K, V> mapping) {
        set = new ListSet<Pair<K,V>>(mapping);
    }

    private ListMap(Set<Pair<K, V>> set) {
        this.set = set;
    }

    //---------------------------------------------------------------------------------------------
    // Iterable Implementation
    //---------------------------------------------------------------------------------------------

    public Iterator<Pair<K, V>> iterator() {
        return set.iterator();
    }

    public <T> Set<T> map(Function1<Pair<K, V>, T> mapper) {
        return set.map(mapper);
    }

    public Map<K, V> filter(Function1<Pair<K, V>, Boolean> p) {
        return new ListMap<K, V>(set.filter(p));
    }

    public <T> T foldLeft(T start, Function2<T, Pair<K, V>, T> f) {
        return set.foldLeft(start, f);
    }

    public <T> T foldRight(T start, Function2<Pair<K, V>, T, T> f) {
        return set.foldRight(start, f);
    }

    //---------------------------------------------------------------------------------------------
    // Collection Implementation
    //---------------------------------------------------------------------------------------------

    public boolean isEmpty() {
        return set.isEmpty();
    }

    public int size() {
        return set.size();
    }

    public boolean contains(Pair<K, V> element) {
        return set.contains(element);
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public Maybe<V> get(K key) {
        return set.filter(new NodeSearcher<K, V>(key)).iterator().current().maybe(toValue());
    }

    public Maybe<V> apply(K k) {
        return get(k);
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public ListMap<K, V> put(K key, V value) {
        return add(Pairs.newPair(key, value));
    }

    public ListMap<K, V> add(Pair<K, V> element) {
        return new ListMap<K, V>(set.add(element));
    }

    public MaybeC<ListMap<K, V>, Pair<K, V>> add() {
        return new MaybeC<ListMap<K, V>, Pair<K, V>>() {

            public ListMap<K, V> c(Pair<K, V> r) {
                return add(r);
            }

            public ListMap<K, V> c() {
                return ListMap.this;
            }
        };
    }

    public Map<K, V> remove(Pair<K, V> element) {
        return new ListMap<K, V>(set.remove(element));
    }

    public MaybeC<Map<K, V>, Pair<K, V>> remove() {
        return new MaybeC<Map<K, V>, Pair<K, V>>() {

            public Map<K, V> c(Pair<K, V> r) {
                return remove(r);
            }

            public Map<K, V> c() {
                return ListMap.this;
            }
        };
    }

    public Map<K, V> removeKey(final K key) {
        return filter(new Function1<Pair<K, V>, Boolean>() {
            public Boolean apply(Pair<K, V> mapping) {
                return !mapping.getFst().equals(key);
            }
        });
    }

    public MaybeC<Map<K, V>, K> removeKey() {
        return new MaybeC<Map<K, V>, K>() {

            public Map<K, V> c(K r) {
                return removeKey(r);
            }

            public Map<K, V> c() {
                return ListMap.this;
            }
        };
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    private MaybeC<Maybe<V>, Pair<K, V>> toValue() {
        return new MaybeC<Maybe<V>, Pair<K, V>>() {

            public Maybe<V> c(Pair<K, V> r) {
                return just(r.getSnd());
            }

            public Maybe<V> c() {
                return nothing();
            }
        };
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ListMap listMap = (ListMap) o;

        return set.equals(listMap.set);
    }

    @Override
    public int hashCode() {
        return set.hashCode();
    }

    @Override
    public String toString() {
        return set.toString();
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
            return key.equals(node.getFst());
        }
    }
}
