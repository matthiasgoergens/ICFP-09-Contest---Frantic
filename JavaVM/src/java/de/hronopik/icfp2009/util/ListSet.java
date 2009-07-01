package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.List.nil;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class ListSet<E> implements Set<E> {

    private final List<E> list;

    //---------------------------------------------------------------------------------------------
    // Constructors
    //---------------------------------------------------------------------------------------------

    public ListSet() {
        list = nil();
    }

    public ListSet(E element) {
        list = new LinkedList<E>(element, List.<E>nil());
    }

    private ListSet(List<E> list) {
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

    public boolean contains(E element) {
        return list.contains(element);
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public Set<E> add(E element) {
        return new ListSet<E>(new LinkedList<E>(element, list.remove(element)));
    }
    
    public MaybeC<Set<E>, E> add() {
        return new MaybeC<Set<E>, E>() {

            public Set<E> c(E r) {
                return add(r);
            }

            public Set<E> c() {
                return ListSet.this;
            }
        };
    }

    public Set<E> remove(E element) {
        return new ListSet<E>(list.remove(element));
    }

    public MaybeC<Set<E>, E> remove() {
        return new MaybeC<Set<E>, E>() {

            public Set<E> c(E r) {
                return remove(r);
            }

            public Set<E> c() {
                return ListSet.this;
            }
        };
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public Iterator<E> iterator() {
        return list.iterator();
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public <T> Set<T> map(Function1<E, T> mapper) {
        return new ListSet<T>(list.map(mapper));
    }

    public Set<E> filter(Function1<E, Boolean> p) {
        return new ListSet<E>(list.filter(p));
    }

    public <T> T foldLeft(T start, Function2<T, E, T> f) {
        return list.foldLeft(start, f);
    }

    public <T> T foldRight(T start, Function2<E, T, T> f) {
        return list.foldRight(start, f);
    }
}
