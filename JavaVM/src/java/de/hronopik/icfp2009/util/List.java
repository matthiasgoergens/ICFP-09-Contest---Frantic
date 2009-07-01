package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.Maybe.nothing;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public abstract class List<E> implements Collection<E> {

    //---------------------------------------------------------------------------------------------
    // Constructors
    //---------------------------------------------------------------------------------------------

    private static final Nil NIL = new Nil();

    /**
     * Returns an empty list.
     *
     * @param <E> the element type of the empty list
     * @return an empty list
     */
    public static <E> Nil<E> nil() {
        return (Nil<E>) NIL;
    }

    /**
     * Private constructor to prevent instantiation.
     */
    private List() {
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public abstract Maybe<E> head();

    public abstract Maybe<List<E>> tail();

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

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

    public abstract List<E> remove(E element);

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public abstract <T> List<T> map(Function1<E, T> mappper);

    public abstract List<E> filter(Function1<E, Boolean> p);

    //---------------------------------------------------------------------------------------------
    // Implementations
    //---------------------------------------------------------------------------------------------

    public static abstract class Element<E> extends List<E> {

        public boolean isEmpty() {
            return false;
        }

        public int size() {
            return 1 + tail().just().size();
        }

        public abstract Maybe.Just<E> head();

        public abstract Maybe.Just<List<E>> tail();
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

        public boolean contains(E element) {
            return false;
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

        public List<E> remove(E element) {
            return this;
        }

        public <T> List<T> map(Function1<E, T> mappper) {
            return (List<T>) this;
        }

        public List<E> filter(Function1<E, Boolean> p) {
            return this;
        }
    }
}
