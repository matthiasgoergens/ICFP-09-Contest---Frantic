package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.Maybe.nothing;

/**
 * @author Alexander Kiel
 * @version $Id$
 * @see "http://www.haskell.org/haskellwiki/Non-empty_list"
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
    @SuppressWarnings({"unchecked"})
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

    public abstract List<E> take(int n);

    public abstract List<E> drop(int n);

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

    public abstract List<E> reverse();

    abstract List<E> rev(List<E> a);

    //---------------------------------------------------------------------------------------------
    // Implementations
    //---------------------------------------------------------------------------------------------

    public static abstract class Element<E> extends List<E> {

        public final boolean isEmpty() {
            return false;
        }

        public int size() {
            return 1 + tail().size();
        }

        public abstract E head();

        public abstract List<E> tail();

        public List<E> drop(int n) {
            return n > 0 ? tail().drop(n - 1) : this;
        }

        public Iterator<E> iterator() {
            return new Iterator<E>() {

                public Maybe<E> current() {
                    return Maybe.just(head());
                }

                public Iterator<E> next() {
                    return tail().iterator();
                }

                @Override
                public String toString() {
                    return "Iterator(" + head() + ", " + tail() + ")";
                }
            };
        }

        public abstract <T> Element<T> map(Function1<E, T> mappper);

        public <T> T foldLeft(T start, Function2<T, E, T> f) {
            /*
             * Normal recursive implementation:
             *
             * return tail().foldLeft(f.apply(start, head()), f);
             */

            // Iterative implementation
            T result = start;
            List<E> list = this;
            while (!list.isEmpty()) {
                Element<E> nonEmptyList = (Element<E>) list;
                result = f.apply(result, nonEmptyList.head());
                list = nonEmptyList.tail();
            }
            return result;
        }

        public <T> T foldRight(T start, Function2<E, T, T> f) {
            return f.apply(head(), tail().foldRight(start, f));
        }

        public abstract Element<E> reverse();

        //---------------------------------------------------------------------------------------------
        // Overridden Object Methods
        //---------------------------------------------------------------------------------------------

        @Override
        public final boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Element)) return false;

            Element that = (Element) o;

            return head().equals(that.head()) && tail().equals(that.tail());
        }

        @Override
        public final int hashCode() {
            int result = head().hashCode();
            result = 31 * result + tail().hashCode();
            return result;
        }

        @Override
        public String toString() {
            StringBuilder sb = foldLeft(new StringBuilder("["),
                    new Function2<StringBuilder, E, StringBuilder>() {
                        public StringBuilder apply(StringBuilder stringBuilder, E e) {
                            return stringBuilder.append(e).append(", ");
                        }
                    });
            // TODO: removing the last comma is not very functional
            return sb.delete(sb.length() - 2, sb.length()).append("]").toString();
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

        public boolean contains(E element) {
            return false;
        }

        public List<E> take(int n) {
            return this;
        }

        public List<E> drop(int n) {
            return this;
        }

        public Iterator<E> iterator() {
            return new Iterator<E>() {

                public Maybe<E> current() {
                    return nothing();
                }

                public Iterator<E> next() {
                    return this;
                }
            };
        }

        public List<E> append(List<E> list) {
            return list;
        }

        public List<E> remove(E element) {
            return this;
        }

        public <T> Nil<T> map(Function1<E, T> mappper) {
            return (Nil<T>) this;
        }

        public List<E> filter(Function1<E, Boolean> p) {
            return this;
        }

        public <T> T foldLeft(T start, Function2<T, E, T> f) {
            return start;
        }

        public <T> T foldRight(T start, Function2<E, T, T> f) {
            return start;
        }

        public Nil<E> reverse() {
            return this;
        }

        List<E> rev(List<E> a) {
            return a;
        }

        //---------------------------------------------------------------------------------------------
        // Overridden Object Methods
        //---------------------------------------------------------------------------------------------

        @Override
        public String toString() {
            return "[]";
        }
    }
}
