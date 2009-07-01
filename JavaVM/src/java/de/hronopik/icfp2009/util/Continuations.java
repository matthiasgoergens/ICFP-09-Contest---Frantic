package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Continuations {

    public static <T> MaybeC<T, T> fail(final String message) {
        return new MaybeC<T, T>() {

            public T c(T r) {
                return r;
            }

            public T c() {
                throw new RuntimeException(message);
            }
        };
    }
}
