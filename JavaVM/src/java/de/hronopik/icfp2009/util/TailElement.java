package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.Maybe.just;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class TailElement<E> extends List.Element<E> {

    private final E element;

    private final List<E> tail;

    //---------------------------------------------------------------------------------------------
    // Constructors
    //---------------------------------------------------------------------------------------------

    public static <E> MaybeC<List<E>, E> newInstance() {
        return new MaybeC<List<E>, E>() {

            public List<E> c(E r) {
                return new TailElement<E>(r, List.<E>nil());
            }

            public List<E> c() {
                return nil();
            }
        };
    }

    public TailElement(E element, List<E> tail) {
        this.element = element;
        this.tail = tail;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public int size() {
        return 1 + tail.size();
    }

    public boolean contains(final E element) {
        return !filter(new Function1<E, Boolean>() {
            public Boolean apply(E e) {
                return element.equals(e);
            }
        }).isEmpty();
    }

    public Maybe.Just<E> head() {
        return just(element);
    }

    public Maybe.Just<List<E>> tail() {
        return just(tail);
    }    

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public List<E> append(List<E> list) {
        return new TailElement<E>(element, tail.append(list));
    }

    public List<E> remove(final E element) {
        return filter(new Function1<E, Boolean>() {
            public Boolean apply(E e) {
                return !element.equals(e);
            }
        });
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public <T> List<T> map(Function1<E, T> mapper) {
        return new TailElement<T>(mapper.apply(element), tail.map(mapper));
    }

    public List<E> filter(Function1<E, Boolean> p) {
        return p.apply(element) ? new TailElement<E>(element, tail.filter(p)) : tail.filter(p);
    }
}
