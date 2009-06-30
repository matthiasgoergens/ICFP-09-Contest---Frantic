package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.Maybe.just;
import static de.hronopik.icfp2009.util.Maybe.nothing;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public abstract class List<E> {

    //---------------------------------------------------------------------------------------------
    // Constructors
    //---------------------------------------------------------------------------------------------

    /**
     * Returns an empty list.
     *
     * @param <E> the element type of the empty list
     * @return an empty list
     */
    public static <E> Nil<E> nil() {
        return new Nil<E>();
    }

    public static <E> Element<E> list(E element) {
        return nil().prepend(element);
    }

    private List() {
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public abstract boolean isEmpty();

    public abstract int size();

    public abstract Maybe<E> head();

    public abstract Maybe<List<E>> tail();

    public List<E> prepend(E element) {
        return new Element<E>(element, this);
    }

    public abstract List<E> append(List<E> list);

    /**
     * Partial application of append.
     *
     * @return
     * @see #append(List<E>)
     */
    public MaybeC<List<E>, List<E>> append() {
        return new MaybeC<List<E>, List<E>>() {

            public List<E> c(List<E> e) {
                return append(e);
            }

            public List<E> c() {
                return List.this;
            }
        };
    }

    //---------------------------------------------------------------------------------------------
    // Implementations
    //---------------------------------------------------------------------------------------------

    public static class Element<E> extends List<E> {

        private final E element;

        private final List<E> tail;

        private Element(E element, List<E> tail) {
            this.element = element;
            this.tail = tail;
        }

        public boolean isEmpty() {
            return false;
        }

        public int size() {
            return 1 + tail.size();
        }

        public Maybe<E> head() {
            return just(element);
        }

        public Maybe<List<E>> tail() {
            return just(tail);
        }

        public List<E> append(List<E> list) {
            return new Element<E>(element, tail.append(list));
        }
    }

    public static class Nil<E> extends List<E> {

        private Nil() {
        }

        public boolean isEmpty() {
            return true;
        }

        public int size() {
            return 0;
        }

        public Maybe<E> head() {
            return nothing();
        }

        public Maybe<List<E>> tail() {
            return nothing();
        }

        public List<E> append(List<E> list) {
            return list;
        }
    }
}
