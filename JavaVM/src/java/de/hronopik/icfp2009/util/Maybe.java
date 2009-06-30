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

    public abstract <Q> Q maybe(MaybeC<Q, T> mc);

    public abstract <Q> Q maybe(JustC<Q, T> jc, NothingC<Q> nc);

    //---------------------------------------------------------------------------------------------
    // 
    //---------------------------------------------------------------------------------------------

    public static abstract class Nothing<T> extends Maybe<T> {
        private Nothing() {
        }
    }

    public static abstract class Just<T> extends Maybe<T> {
        private Just() {
        }

        public abstract T just();
    }

    //---------------------------------------------------------------------------------------------
    // 
    //---------------------------------------------------------------------------------------------

    public static <T> Maybe<T> just(final T t) {
        return new Just<T>() {

            @Override
            public T just() {
                return t;
            }

            public <Q> Q maybe(MaybeC<Q, T> mc) {
                return mc.c(t);
            }

            @Override
            public <Q> Q maybe(final JustC<Q, T> jc, final NothingC<Q> nc) {
                return jc.c(t);
            }
        };
    }

    public static <T> Maybe<T> nothing() {
        return new Nothing<T>() {

            public <Q> Q maybe(MaybeC<Q, T> mc) {
                return mc.c();
            }

            @Override
            public <Q> Q maybe(final JustC<Q, T> jc, final NothingC<Q> nc) {
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
