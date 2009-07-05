package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.Maybe.just;
import static de.hronopik.icfp2009.util.Maybe.nothing;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public abstract class AvlTree<K extends Comparable<? super K>, V> implements Map<K, V> {

    //---------------------------------------------------------------------------------------------
    // Constructors
    //---------------------------------------------------------------------------------------------

    private static final Empty<Integer, Object> EMPTY = new Empty<Integer, Object>();

    /**
     * Creates an empty {@linkplain AvlTree}.
     *
     * @param <K> the type of the key
     * @param <V> the type of the value
     * @return an empty {@linkplain AvlTree}
     */
    @SuppressWarnings({"unchecked"})
    public static <K extends Comparable<? super K>, V> Empty<K, V> empty() {
        return new Empty<K, V>();
    }

    public static <K extends Comparable<? super K>, V> ZeroNode<K, V> singelton(K key, V value) {
        return AvlTree.<K, V>empty().put(key, value);
    }

    public static <V> AvlTree<Integer, V> fromList(List<V> list) {
        return list.foldLeft(new Empty<Integer, V>(), new Function2<AvlTree<Integer, V>, V, AvlTree<Integer, V>>() {
            public AvlTree<Integer, V> apply(AvlTree<Integer, V> tree, V value) {
                return tree.put(tree.size(), value);
            }
        });
    }

    /**
     * This private constructor prevents foreign instantiations.
     */
    private AvlTree() {
    }

    //---------------------------------------------------------------------------------------------
    // Map Implementation
    //---------------------------------------------------------------------------------------------

    public abstract Node<K, V> put(K key, V value);

    public Node<K, V> add(Pair<K, V> mapping) {
        return put(mapping.getFst(), mapping.getSnd());
    }

    public MaybeC<AvlTree<K, V>, Pair<K, V>> add() {
        return new MaybeC<AvlTree<K, V>, Pair<K, V>>() {

            public Node<K, V> c(Pair<K, V> just) {
                return add(just);
            }

            public AvlTree<K, V> c() {
                return AvlTree.this;
            }
        };
    }

    public Map<K, V> remove(Pair<K, V> mapping) {
        return null;
    }

    public MaybeC<Map<K, V>, Pair<K, V>> remove() {
        return null;
    }

    public Map<K, V> removeKey(K key) {
        return null;
    }

    public MaybeC<Map<K, V>, K> removeKey() {
        return null;
    }

    //---------------------------------------------------------------------------------------------
    // Node
    //---------------------------------------------------------------------------------------------

    public static abstract class Node<K extends Comparable<? super K>, V> extends AvlTree<K, V> {

        final K key;
        final V value;

        final AvlTree<K, V> left;
        final AvlTree<K, V> right;

        //---------------------------------------------------------------------------------------------
        // Constructor
        //---------------------------------------------------------------------------------------------

        private Node(K key, V value, AvlTree<K, V> left, AvlTree<K, V> right) {
            this.key = key;
            this.value = value;
            this.left = left;
            this.right = right;
        }

        //---------------------------------------------------------------------------------------------
        // Iterable Implementation
        //---------------------------------------------------------------------------------------------

        public Iterator<Pair<K, V>> iterator() {
            return null;
        }

        public <T> Set<T> map(Function1<Pair<K, V>, T> mapper) {
            return null;
        }

        public Map<K, V> filter(Function1<Pair<K, V>, Boolean> p) {
            return null;
        }

        public <T> T foldLeft(T start, Function2<T, Pair<K, V>, T> f) {
            return null;
        }

        public <T> T foldRight(T start, Function2<Pair<K, V>, T, T> f) {
            return null;
        }

        //---------------------------------------------------------------------------------------------
        // Collection Implementation
        //---------------------------------------------------------------------------------------------

        public boolean isEmpty() {
            return false;
        }

        public int size() {
            return left.size() + 1 + right.size();
        }

        public boolean contains(final Pair<K, V> element) {
            return get(element.getFst()).maybe(new MaybeC<Boolean, V>() {

                public Boolean c(V just) {
                    return element.equals(just);
                }

                public Boolean c() {
                    return false;
                }
            });
        }

        //---------------------------------------------------------------------------------------------
        // Map Implementation
        //---------------------------------------------------------------------------------------------

        public Maybe<V> apply(K key) {
            return get(key);
        }

        public Maybe<V> get(K key) {
            if (key.compareTo(this.key) < 0) {
                return left.get(key);
            } else if (key.compareTo(this.key) > 0) {
                return right.get(key);
            } else {
                return just(value);
            }
        }

        //---------------------------------------------------------------------------------------------
        // Node Interface
        //---------------------------------------------------------------------------------------------

        public K getKey() {
            return key;
        }

        public V getValue() {
            return value;
        }

        public AvlTree<K, V> getLeft() {
            return left;
        }

        public AvlTree<K, V> getRight() {
            return right;
        }

        //---------------------------------------------------------------------------------------------
        // Overridden Object Methods
        //---------------------------------------------------------------------------------------------

        @Override
        public String toString() {
            return "(" + left + ", (" + key + ", " + value + "), " + right + ")";
        }
    }

    //---------------------------------------------------------------------------------------------
    // PositiveNode
    //---------------------------------------------------------------------------------------------

    /**
     * A positive node is a node which balancing factor is plus one.
     * <p/>
     * At such a node the left subtree height is bigger than the right subtree height.
     *
     * @param <K> the type of the key
     * @param <V> the type of the value
     */
    public static final class PositiveNode<K extends Comparable<? super K>, V> extends Node<K, V> {

        private PositiveNode(K key, V value, Node<K, V> left, AvlTree<K, V> right) {
            super(key, value, left, right);
        }

        public Node<K, V> put(K key, V value) {
            if (key.compareTo(this.key) < 0) {
                final Node<K, V> newLeft = left.put(key, value);
                if (newLeft instanceof ZeroNode) {
                    return new PositiveNode<K, V>(this.key, this.value, newLeft, right);
                } else if (newLeft instanceof PositiveNode) {
                    return putAndRotateLeftLeft(newLeft);
                } else {
                    return putAndRotateLeftRight(newLeft);
                }
            } else if (key.compareTo(this.key) > 0) {
                return new ZeroNode<K, V>(this.key, this.value, left, right.put(key, value));
            } else {
                return new PositiveNode<K, V>(key, value, (Node<K, V>) left, right);
            }
        }

        private Node<K, V> putAndRotateLeftLeft(Node<K, V> newLeft) {
            return new ZeroNode<K, V>(newLeft.key
                    , newLeft.value
                    , newLeft.left
                    , new ZeroNode<K, V>(this.key, this.value, newLeft.right, right)
            );
        }

        private Node<K, V> putAndRotateLeftRight(Node<K, V> newLeft) {
            final Node<K, V> newLeftRight = (Node<K, V>) newLeft.right;
            return new ZeroNode<K, V>(newLeftRight.key
                    , newLeftRight.value
                    , new ZeroNode<K, V>(newLeft.key, newLeft.value, newLeft.left, newLeftRight.left)
                    , new ZeroNode<K, V>(this.key, this.value, left, right)
            );
        }
    }

    //---------------------------------------------------------------------------------------------
    // ZeroNode
    //---------------------------------------------------------------------------------------------

    public static final class ZeroNode<K extends Comparable<? super K>, V> extends Node<K, V> {

        private ZeroNode(K key, V value, AvlTree<K, V> left, AvlTree<K, V> right) {
            super(key, value, left, right);
        }

        public Node<K, V> put(K key, V value) {
            if (key.compareTo(this.key) < 0) {
                return new PositiveNode<K, V>(this.key, this.value, left.put(key, value), right);
            } else if (key.compareTo(this.key) > 0) {
                return new NegativeNode<K, V>(this.key, this.value, left, right.put(key, value));
            } else {
                return new ZeroNode<K, V>(key, value, left, right);
            }
        }
    }

    //---------------------------------------------------------------------------------------------
    // NegativeNode
    //---------------------------------------------------------------------------------------------

    /**
     * A negative node is a node which balancing factor is minus one.
     * <p/>
     * At such a node the right subtree height is bigger than the left subtree height.
     *
     * @param <K> the type of the key
     * @param <V> the type of the value
     */
    public static final class NegativeNode<K extends Comparable<? super K>, V> extends Node<K, V> {

        private NegativeNode(K key, V value, AvlTree<K, V> left, Node<K, V> right) {
            super(key, value, left, right);
        }

        public Node<K, V> put(K key, V value) {
            if (key.compareTo(this.key) < 0) {
                return new ZeroNode<K, V>(this.key, this.value, left.put(key, value), right);
            } else if (key.compareTo(this.key) > 0) {
                final Node<K, V> newRight = right.put(key, value);
                if (newRight instanceof ZeroNode) {
                    return new NegativeNode<K, V>(this.key, this.value, left, newRight);
                } else if (newRight instanceof NegativeNode) {
                    return putAndRotateRightRight(newRight);
                } else {
                    return putAndRotateRightLeft(newRight);
                }
            } else {
                return new NegativeNode<K, V>(key, value, left, (Node<K, V>) right);
            }
        }

        private Node<K, V> putAndRotateRightRight(Node<K, V> newRight) {
            return new ZeroNode<K, V>(newRight.key
                    , newRight.value
                    , new ZeroNode<K, V>(this.key, this.value, left, newRight.left)
                    , newRight.right
            );
        }

        private Node<K, V> putAndRotateRightLeft(Node<K, V> newRight) {
            final Node<K, V> newRightLeft = (Node<K, V>) newRight.left;
            return new ZeroNode<K, V>(newRightLeft.key
                    , newRightLeft.value
                    , new ZeroNode<K, V>(this.key, this.value, right, left)
                    , new ZeroNode<K, V>(newRight.key, newRight.value, newRight.right, newRightLeft.right)
            );
        }
    }

    //---------------------------------------------------------------------------------------------
    // Empty
    //---------------------------------------------------------------------------------------------

    public static final class Empty<K extends Comparable<? super K>, V> extends AvlTree<K, V> {

        private Empty() {
        }

        //---------------------------------------------------------------------------------------------
        // Iterable Implementation
        //---------------------------------------------------------------------------------------------

        public Iterator<Pair<K, V>> iterator() {
            return null;
        }

        public <T> Set<T> map(Function1<Pair<K, V>, T> mapper) {
            return null;
        }

        public Map<K, V> filter(Function1<Pair<K, V>, Boolean> p) {
            return null;
        }

        public <T> T foldLeft(T start, Function2<T, Pair<K, V>, T> f) {
            return null;
        }

        public <T> T foldRight(T start, Function2<Pair<K, V>, T, T> f) {
            return null;
        }

        //---------------------------------------------------------------------------------------------
        // Collection Implementation
        //---------------------------------------------------------------------------------------------

        public boolean isEmpty() {
            return true;
        }

        public int size() {
            return 0;
        }

        public boolean contains(Pair<K, V> element) {
            return false;
        }

        //---------------------------------------------------------------------------------------------
        // Map Implementation
        //---------------------------------------------------------------------------------------------

        public Maybe<V> apply(K k) {
            return nothing();
        }

        public Maybe<V> get(K key) {
            return nothing();
        }

        public ZeroNode<K, V> put(K key, V value) {
            return new ZeroNode<K, V>(key, value, AvlTree.<K, V>empty(), AvlTree.<K, V>empty());
        }

        //---------------------------------------------------------------------------------------------
        // Overridden Object Methods
        //---------------------------------------------------------------------------------------------

        @Override
        public String toString() {
            return "EMPTY";
        }
    }
}
