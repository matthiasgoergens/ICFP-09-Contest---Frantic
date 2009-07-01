package de.hronopik.icfp2009.util;

import java.util.Comparator;
import java.io.Serializable;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Pairs {

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------
    
    public static <A, B> Pair<A, B> newPair(A a, B b) {
        return new PairImpl<A, B>(a, b);
    }
    
    //---------------------------------------------------------------------------------------------
    // Partial Getter
    //---------------------------------------------------------------------------------------------
    
    public static <A> Function1<Pair<A, ?>, A> fst() {
        return new Function1<Pair<A, ?>, A>() {
            public A apply(Pair<A, ?> pair) {
                return pair.getFst();
            }
        };
    }
    
    public static <B> Function1<Pair<?, B>, B> snd() {
        return new Function1<Pair<?, B>, B>() {
            public B apply(Pair<?, B> pair) {
                return pair.getSnd();
            }
        };
    }
    
    //---------------------------------------------------------------------------------------------
    // Comparators
    //---------------------------------------------------------------------------------------------

    public static <T extends Comparable<? super T>> Comparator<Pair<T, ?>> fstComparator() {
        return new FstComparator<T>();
    }
    
    public static <T extends Comparable<? super T>> Comparator<Pair<?, T>> sndComparator() {
        return new SndComparator<T>();
    }

    //---------------------------------------------------------------------------------------------
    // PairImpl
    //---------------------------------------------------------------------------------------------

    private static class PairImpl<A, B> implements Pair<A, B> {

        private final A fst;
        private final B snd;

        //---------------------------------------------------------------------------------------------
        // Constructor
        //---------------------------------------------------------------------------------------------

        private PairImpl(A fst, B snd) {
            this.fst = fst;
            this.snd = snd;
        }

        //---------------------------------------------------------------------------------------------
        //
        //---------------------------------------------------------------------------------------------

        public A getFst() {
            return fst;
        }

        public B getSnd() {
            return snd;
        }

        //---------------------------------------------------------------------------------------------
        // Overridden Object Methods
        //---------------------------------------------------------------------------------------------

        @Override
        public String toString() {
            return "(" + fst + ", " + snd + ")";
        }
    }
    
    //---------------------------------------------------------------------------------------------
    // Comparators
    //---------------------------------------------------------------------------------------------

    private static class FstComparator<T extends Comparable<? super T>> implements Comparator<Pair<T, ?>>, Serializable {

        public int compare(Pair<T, ?> pair1, Pair<T, ?> pair2) {
            return pair1.getFst().compareTo(pair2.getFst());
        }
    }
    
    private static class SndComparator<T extends Comparable<? super T>> implements Comparator<Pair<?, T>>, Serializable {

        public int compare(Pair<?, T> pair1, Pair<?, T> pair2) {
            return pair1.getSnd().compareTo(pair2.getSnd());
        }
    }
}
