package de.hronopik.icfp2009.util;

import java.util.Comparator;
import java.io.Serializable;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Lists {

    public static <E extends Comparable<? super E>> List<E> sort(List<E> list) {
        return sort(list, new ComparableComparator<E>());
    }

    public static <E> List<E> sort(List<E> list, Comparator<? super E> cmp) {
        return list.isEmpty() ? list : sort((List.Element<E>) list, cmp);
    }

    public static <E> List<E> sort(List.Element<E> list, final Comparator<? super E> cmp) {

        // This function returns the smallest element
        Function2<E, E, E> bubble = new Function2<E, E, E>() {
            public E apply(final E x, final E y) {
                return cmp.compare(x, y) < 0 ? x : y;
            }
        };

        E element = list.tail().just().foldRight(list.head().just(), bubble);
        return new LinkedList<E>(element, sort(list.remove(element), cmp));
    }

    //---------------------------------------------------------------------------------------------
    // Comparable Comparator
    //---------------------------------------------------------------------------------------------

    private static class ComparableComparator<T extends Comparable<? super T>> implements Comparator<T>, Serializable {

        public int compare(T o1, T o2) {
            return o1.compareTo(o2);
        }
    }
}
