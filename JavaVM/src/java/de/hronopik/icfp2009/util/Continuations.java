package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Continuations {

    public static <T> MaybeC<T, T> fail(final String message) {
        return new FailContinuation<T>(message);
    }

    private static class FailContinuation<T> implements MaybeC<T, T> {

        private final String message;

        private FailContinuation(String message) {
            this.message = message;
        }

        public T c(T r) {
            return r;
        }

        public T c() {
            throw new RuntimeException(message);
        }
    }
}
