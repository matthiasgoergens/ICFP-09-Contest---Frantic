package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.Maybe.just;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class LinkedList<E> extends List.Element<E> {

    private final E head;

    private final List<E> tail;

    //---------------------------------------------------------------------------------------------
    // Constructors
    //---------------------------------------------------------------------------------------------

    public static <E> MaybeC<List<E>, E> newInstance() {
        return new MaybeC<List<E>, E>() {

            public List<E> c(E r) {
                return new LinkedList<E>(r, List.<E>nil());
            }

            public List<E> c() {
                return nil();
            }
        };
    }

    public static <E> LinkedList<E> newInstance3(E x0, E x1, E x2) {
        return new LinkedList<E>(x0, newInstance2(x1, x2));
    }

    public static <E> LinkedList<E> newInstance2(E x0, E x1) {
        return new LinkedList<E>(x0, newInstance1(x1));
    }

    public static <E> LinkedList<E> newInstance1(E x0) {
        return new LinkedList<E>(x0);
    }

    public static <E> List<E> fromCollection(Collection<E> collection) {
        return collection.foldRight(List.<E>nil(), new Function2<E, List<E>, List<E>>() {
            public List<E> apply(E head, List<E> tail) {
                return new LinkedList<E>(head, tail);
            }
        });
    }

    public LinkedList(E head) {
        this(head, List.<E>nil());
    }

    public LinkedList(E head, List<E> tail) {
        this.head = head;
        this.tail = tail;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public int size() {
        return 1 + tail.size();
    }

    public boolean contains(final E element) {
        return !filter(new Function1<E, Boolean>() {
            public Boolean apply(E e) {
                return element.equals(e);
            }
        }).isEmpty();
    }

    public Maybe.Just<E> head() {
        return just(head);
    }

    /**
     * Passes the head of this list to the given maybe continuation.
     *
     * @param continuation the maybe continuation to be invoked
     * @param <Q>          the type of the return value
     * @return the result of the maybe continuation
     */
    public <Q> Q headC(MaybeC<Q, ? super E> continuation) {
        return continuation.c(head);
    }

    public Maybe.Just<List<E>> tail() {
        return just(tail);
    }

    public List<E> take(int n) {
        return n > 0 ? new LinkedList<E>(head, tail.take(n - 1)) : List.<E>nil();
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public List<E> append(List<E> list) {
        return new LinkedList<E>(head, tail.append(list));
    }

    public List<E> remove(final E element) {
        return filter(new Function1<E, Boolean>() {
            public Boolean apply(E e) {
                return !element.equals(e);
            }
        });
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public <T> Element<T> map(Function1<E, T> mapper) {
        return new LinkedList<T>(mapper.apply(head), tail.map(mapper));
    }

    public List<E> filter(Function1<E, Boolean> p) {
        return p.apply(head) ? new LinkedList<E>(head, tail.filter(p)) : tail.filter(p);
    }
}
