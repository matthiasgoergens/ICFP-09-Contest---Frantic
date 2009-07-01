package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public abstract class Maybe<T> {

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    private Maybe() {
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public abstract boolean isJust();

    public abstract boolean isNothing();

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public abstract <Q> Q maybe(MaybeC<Q, ? super T> mc);

    public abstract <Q> Q maybe(JustC<Q, ? super T> jc, NothingC<Q> nc);

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public static class Just<T> extends Maybe<T> {

        private final T value;

        public Just(T value) {
            this.value = value;
        }

        public boolean isJust() {
            return true;
        }

        public boolean isNothing() {
            return false;
        }

        public T just() {
            return value;
        }

        public <Q> Q maybe(MaybeC<Q, ? super T> mc) {
            return mc.c(value);
        }

        @Override
        public <Q> Q maybe(final JustC<Q, ? super T> jc, final NothingC<Q> nc) {
            return jc.c(value);
        }

        //---------------------------------------------------------------------------------------------
        //
        //---------------------------------------------------------------------------------------------

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Just just = (Just) o;

            return value.equals(just.value);
        }

        @Override
        public int hashCode() {
            return value.hashCode();
        }
    }

    public static abstract class Nothing<T> extends Maybe<T> {
        private Nothing() {
        }

        public boolean isJust() {
            return false;
        }

        public boolean isNothing() {
            return true;
        }
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public static <T> Just<T> just(final T t) {
        return new Just<T>(t);
    }

    public static <T> Nothing<T> nothing() {
        return new Nothing<T>() {

            public <Q> Q maybe(MaybeC<Q, ? super T> mc) {
                return mc.c();
            }

            @Override
            public <Q> Q maybe(final JustC<Q, ? super T> jc, final NothingC<Q> nc) {
                return nc.c();
            }
        };
    }

    //---------------------------------------------------------------------------------------------
    // Continuation Interfaces
    //---------------------------------------------------------------------------------------------

    public static <T> JustC<T, T> idC() {
        return new IdC<T>();
    }

    private static class IdC<T> implements JustC<T, T> {

        public T c(T value) {
            return value;
        }
    }

    /**
     * Returns the given default value.
     *
     * @param <T> the type of the default value
     */
    public static class DefaultC<T> implements NothingC<T> {

        private final T value;

        public DefaultC(T value) {
            this.value = value;
        }

        public T c() {
            return value;
        }
    }

}
