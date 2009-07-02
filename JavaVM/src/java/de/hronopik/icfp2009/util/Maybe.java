package de.hronopik.icfp2009.util;

/**
 * An abstract data type which encapsulates an optional value.
 * <p/>
 * A value of type {@code Maybe<T>} either contains a value of type {@code T} (represented as {@link Just
 * Just&lt;T>}), or it is empty (represented as {@link Nothing Nothing}). Using Maybe is a good way to deal
 * with errors or exceptional cases without returning {@code null} or throwing an exception.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @param <T> the type of the encapsulates optional value
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

    /**
     * Returns {@code true} iff this instance is of type {@link Just Just&lt;T>}.
     * <p/>
     * {@code a.isJust() == (a instanceof Maybe.Just)} for all {@code a}
     *
     * @return {@code true} iff this instance is of type {@link Just Just&lt;T>}
     */
    public abstract boolean isJust();

    /**
     * Returns {@code true} iff this instance is of type {@link Nothing Nothing}.
     * <p/>
     * {@code a.isNothing() == (a instanceof Maybe.Nothing)} for all {@code a}
     *
     * @return {@code true} iff this instance is of type {@link Nothing Nothing}
     */
    public abstract boolean isNothing();

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    /**
     * Passes the possible value of this maybe type to the given maybe continuation.
     *
     * @param continuation the maybe continuation to be invoked
     * @param <Q>          the type of the return value
     * @return the result of the maybe continuation
     */
    public abstract <Q> Q maybe(MaybeC<Q, ? super T> continuation);

    public abstract <Q> Q maybe(JustC<Q, ? super T> jc, NothingC<Q> nc);

    //---------------------------------------------------------------------------------------------
    // Just
    //---------------------------------------------------------------------------------------------

    public static final class Just<T> extends Maybe<T> {

        private final T value;

        private Just(T value) {
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
        // Overridden Object Methods
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

        @Override
        public String toString() {
            return "Just(" + value + ")";
        }
    }

    public static final class Nothing<T> extends Maybe<T> {

        private Nothing() {
        }

        public boolean isJust() {
            return false;
        }

        public boolean isNothing() {
            return true;
        }

        public <Q> Q maybe(MaybeC<Q, ? super T> mc) {
            return mc.c();
        }

        @Override
        public <Q> Q maybe(final JustC<Q, ? super T> jc, final NothingC<Q> nc) {
            return nc.c();
        }

        @Override
        public String toString() {
            return "Nothing";
        }
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public static <T> Just<T> just(final T value) {
        return new Just<T>(value);
    }

    private static final Nothing<Object> NOTHING = new Nothing<Object>();

    @SuppressWarnings({"unchecked"})
    public static <T> Nothing<T> nothing() {
        return (Nothing<T>) NOTHING;
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
