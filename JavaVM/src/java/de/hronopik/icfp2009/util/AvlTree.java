package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.Maybe.just;
import static de.hronopik.icfp2009.util.Maybe.nothing;

import static java.lang.Math.max;

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
    //
    //---------------------------------------------------------------------------------------------

    public abstract int height();

    public abstract List<V> toList();

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
            assert left instanceof Empty || ((Node<K, V>) left).key.compareTo(key) < 0 : "left key is smaller";
            assert right instanceof Empty || ((Node<K, V>) right).key.compareTo(key) > 0 : "right key is bigger";
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
        //
        //---------------------------------------------------------------------------------------------

        public int height() {
            return 1 + max(left.height(), right.height());
        }

        public List.Element<V> toList() {
            return left.toList().append(new LinkedList<V>(value, right.toList()));
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
            assert left.height() == right.height() + 1 : "left height is bigger";
        }

        public Node<K, V> put(K key, V value) {
            if (key.compareTo(this.key) < 0) {
                return putLeft(key, value);
            } else if (key.compareTo(this.key) > 0) {
                return putRight(key, value);
            } else {
                return putHere(key, value);
            }
        }

        private Node<K, V> left() {
            assert left instanceof Node : "left is a node";
            return (Node<K, V>) left;
        }

        /**
         * Puts the key value pair in the left subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see NegativeNode#putRight
         */
        private Node<K, V> putLeft(K key, V value) {
            if (left instanceof ZeroNode) {
                if (key.compareTo(left().key) < 0) {
                    return putAndRotateLeftLeft(key, value);
                } else if (key.compareTo(left().key) > 0) {
                    return putAndRotateLeftRight(key, value);
                } else {
                    return new PositiveNode<K, V>(this.key, this.value, left.put(key, value), right);
                }
            } else {
                return new PositiveNode<K, V>(this.key, this.value, left.put(key, value), right);
            }
        }

        /**
         * Puts the key value pair in the right subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see NegativeNode#putLeft
         */
        private Node<K, V> putRight(K key, V value) {
            final Node<K, V> newRight = right.put(key, value);
            if (right instanceof Empty) {
                return new ZeroNode<K, V>(this.key, this.value, left, newRight);
            } else if (right instanceof ZeroNode) {
                if (newRight instanceof ZeroNode) {
                    return new PositiveNode<K, V>(this.key, this.value, (Node<K, V>) left, newRight);
                } else {
                    return new ZeroNode<K, V>(this.key, this.value, left, newRight);
                }
            } else {
                return new PositiveNode<K, V>(this.key, this.value, (Node<K, V>) left, newRight);
            }
        }

        /**
         * Update existing node.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see NegativeNode#putHere
         */
        private PositiveNode<K, V> putHere(K key, V value) {
            return new PositiveNode<K, V>(key, value, left(), right);
        }

        private Node<K, V> putAndRotateLeftLeft(K key, V value) {
            if (left().left instanceof Empty) {
                assert right instanceof Empty : "right is also empty";
                assert left().right instanceof Empty : "left right is also empty";

                return new ZeroNode<K, V>(left().key
                        , left().value
                        , new ZeroNode<K, V>(key, value, AvlTree.<K, V>empty(), AvlTree.<K, V>empty())
                        , new ZeroNode<K, V>(this.key, this.value, left().right, this.right)
                );
            } else if (left().left instanceof ZeroNode) {
                final Node<K, V> leftLeft = left().left.put(key, value);
                if (leftLeft instanceof ZeroNode) {
                    return new PositiveNode<K, V>(this.key
                            , this.value
                            , new ZeroNode<K, V>(left().key, left().value, leftLeft, left().right)
                            , right
                    );
                } else {
                    return new ZeroNode<K, V>(left().key
                            , left().value
                            , leftLeft
                            , new ZeroNode<K, V>(this.key, this.value, left().right, right)
                    );
                }
            } else {
                return new PositiveNode<K, V>(this.key
                        , this.value
                        , new ZeroNode<K, V>(left().key, left().value, left().left.put(key, value), left().right)
                        , right
                );
            }
        }

        private Node<K, V> putAndRotateLeftRight(K key, V value) {
            if (left().right instanceof Empty) {
                assert right instanceof Empty : "right is also empty";
                assert left().left instanceof Empty : "left left is also empty";

                return new ZeroNode<K, V>(key
                        , value
                        , new ZeroNode<K, V>(left().key, left().value, left().left, AvlTree.<K, V>empty())
                        , new ZeroNode<K, V>(this.key, this.value, AvlTree.<K, V>empty(), this.right)
                );
            } else if (left().right instanceof ZeroNode) {
                final Node<K, V> leftRight = left().right.put(key, value);
                if (leftRight instanceof ZeroNode) {
                    return new PositiveNode<K, V>(this.key
                            , this.value
                            , new ZeroNode<K, V>(left().key, left().value, left().left, leftRight)
                            , right
                    );
                } else if (leftRight instanceof NegativeNode) {
                    return new ZeroNode<K, V>(leftRight.key
                            , leftRight.value
                            , new PositiveNode<K, V>(left().key, left().value, (Node<K, V>) left().left, leftRight.left)
                            , new ZeroNode<K, V>(this.key, this.value, leftRight.right, right)
                    );
                } else if (leftRight instanceof PositiveNode) {
                    return new ZeroNode<K, V>(leftRight.key
                            , leftRight.value
                            , new ZeroNode<K, V>(left().key, left().value, left().left, leftRight.left)
                            , new NegativeNode<K, V>(this.key, this.value, leftRight.right, (Node<K, V>) right)
                    );
                } else {
                    throw new RuntimeException("empty node not possible");
                }
            } else {
                return new PositiveNode<K, V>(this.key
                        , this.value
                        , new ZeroNode<K, V>(left().key, left().value, left().left, left().right.put(key, value))
                        , right
                );
            }
        }
    }

    //---------------------------------------------------------------------------------------------
    // ZeroNode
    //---------------------------------------------------------------------------------------------

    public static final class ZeroNode<K extends Comparable<? super K>, V> extends Node<K, V> {

        private ZeroNode(K key, V value, AvlTree<K, V> left, AvlTree<K, V> right) {
            super(key, value, left, right);
            assert left.height() == right.height() : "heights are equal";
        }

        public Node<K, V> put(K key, V value) {
            if (key.compareTo(this.key) < 0) {
                return putLeft(key, value);
            } else if (key.compareTo(this.key) > 0) {
                return putRight(key, value);
            } else {
                return putHere(key, value);
            }
        }

        /**
         * Puts the key value pair in the left subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         */
        private Node<K, V> putLeft(K key, V value) {
            if (left instanceof Empty) {
                assert right instanceof Empty : "right is also empty";
                return new PositiveNode<K, V>(this.key, this.value, left.put(key, value), right);
            } else if (left instanceof ZeroNode) {
                final Node<K, V> newLeft = left.put(key, value);
                if (newLeft instanceof ZeroNode) {
                    return new ZeroNode<K, V>(this.key, this.value, newLeft, right);
                } else {
                    return new PositiveNode<K, V>(this.key, this.value, newLeft, right);
                }
            } else {
                return new ZeroNode<K, V>(this.key, this.value, left.put(key, value), right);
            }
        }

        /**
         * Puts the key value pair in the right subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         */
        private Node<K, V> putRight(K key, V value) {
            if (right instanceof Empty) {
                assert left instanceof Empty : "left is also empty";
                return new NegativeNode<K, V>(this.key, this.value, left, right.put(key, value));
            } else if (right instanceof ZeroNode) {
                final Node<K, V> newRight = right.put(key, value);
                if (newRight instanceof ZeroNode) {
                    return new ZeroNode<K, V>(this.key, this.value, left, newRight);
                } else {
                    return new NegativeNode<K, V>(this.key, this.value, left, newRight);
                }
            } else {
                return new ZeroNode<K, V>(this.key, this.value, left, right.put(key, value));
            }
        }
        
        /**
         * Update existing node.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node         
         */
        private ZeroNode<K, V> putHere(K key, V value) {
            return new ZeroNode<K, V>(key, value, left, right);
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
            assert left.height() + 1 == right.height() : "right height is bigger";
        }

        public Node<K, V> put(K key, V value) {
            if (key.compareTo(this.key) < 0) {
                return putLeft(key, value);
            } else if (key.compareTo(this.key) > 0) {
                return putRight(key, value);
            } else {
                return putHere(key, value);
            }
        }

        private Node<K, V> right() {
            assert right instanceof Node : "right is a node";
            return (Node<K, V>) right;
        }

        /**
         * Puts the key value pair in the left subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see PositiveNode#putRight
         */
        private Node<K, V> putLeft(K key, V value) {
            final Node<K, V> newLeft = left.put(key, value);
            if (left instanceof Empty) {
                return new ZeroNode<K, V>(this.key, this.value, newLeft, right);
            } else if (left instanceof ZeroNode) {
                if (newLeft instanceof ZeroNode) {
                    return new NegativeNode<K, V>(this.key, this.value, newLeft, (Node<K, V>) right);
                } else {
                    return new ZeroNode<K, V>(this.key, this.value, newLeft, right);
                }
            } else {
                return new NegativeNode<K, V>(this.key, this.value, newLeft, (Node<K, V>) right);
            }
        }

        /**
         * Puts the key value pair in the right subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see PositiveNode#putLeft
         */
        private Node<K, V> putRight(K key, V value) {
            if (right instanceof ZeroNode) {
                if (key.compareTo(right().key) > 0) {
                    return putAndRotateRightRight(key, value);
                } else if (key.compareTo(right().key) < 0) {
                    return putAndRotateRightLeft(key, value);
                } else {
                    return new NegativeNode<K, V>(this.key, this.value, left, right.put(key, value));
                }
            } else {
                return new NegativeNode<K, V>(this.key, this.value, left, right.put(key, value));
            }
        }

        /**
         * Update existing node.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see PositiveNode#putHere
         */
        private NegativeNode<K, V> putHere(K key, V value) {
            return new NegativeNode<K, V>(key, value, left, right());
        }

        private Node<K, V> putAndRotateRightRight(K key, V value) {
            if (right().right instanceof Empty) {
                assert left instanceof Empty : "left is also empty";
                assert right().left instanceof Empty : "right left is also empty";

                return new ZeroNode<K, V>(right().key
                        , right().value
                        , new ZeroNode<K, V>(this.key, this.value, this.left, right().left)
                        , new ZeroNode<K, V>(key, value, AvlTree.<K, V>empty(), AvlTree.<K, V>empty())
                );
            } else if (right().right instanceof ZeroNode) {
                final Node<K, V> rightRight = right().right.put(key, value);
                if (rightRight instanceof ZeroNode) {
                    return new NegativeNode<K, V>(this.key
                            , this.value
                            , left
                            , new ZeroNode<K, V>(right().key, right().value, right().left, rightRight)
                    );
                } else {
                    return new ZeroNode<K, V>(right().key
                            , right().value
                            , new ZeroNode<K, V>(this.key, this.value, left, right().left)
                            , rightRight
                    );
                }
            } else {
                return new NegativeNode<K, V>(this.key
                        , this.value
                        , left
                        , new ZeroNode<K, V>(right().key, right().value, right().left, right().right.put(key, value))
                );
            }
        }

        private Node<K, V> putAndRotateRightLeft(K key, V value) {
            if (right().left instanceof Empty) {
                assert left instanceof Empty : "left is also empty";
                assert right().right instanceof Empty : "right right is also empty";

                return new ZeroNode<K, V>(key
                        , value
                        , new ZeroNode<K, V>(this.key, this.value, this.left, AvlTree.<K, V>empty())
                        , new ZeroNode<K, V>(right().key, right().value, AvlTree.<K, V>empty(), right().right)
                );
            } else if (right().left instanceof ZeroNode) {
                final Node<K, V> rightLeft = right().left.put(key, value);
                if (rightLeft instanceof ZeroNode) {
                    return new NegativeNode<K, V>(this.key
                            , this.value
                            , left
                            , new ZeroNode<K, V>(right().key, right().value, rightLeft, right().right)
                    );
                } else if (rightLeft instanceof PositiveNode) {
                    return new ZeroNode<K, V>(rightLeft.key
                            , rightLeft.value
                            , new ZeroNode<K, V>(this.key, this.value, left, rightLeft.left)
                            , new NegativeNode<K, V>(right().key, right().value, rightLeft.right, (Node<K, V>) right().right)
                    );
                } else if (rightLeft instanceof NegativeNode) {
                    return new ZeroNode<K, V>(rightLeft.key
                            , rightLeft.value
                            , new PositiveNode<K, V>(this.key, this.value, (Node<K, V>) left, rightLeft.left)
                            , new ZeroNode<K, V>(right().key, right().value, rightLeft.right, right().right)
                    );
                } else {
                    throw new RuntimeException("empty node not possible");
                }
            } else {
                return new NegativeNode<K, V>(this.key
                        , this.value
                        , left
                        , new ZeroNode<K, V>(right().key, right().value, right().left.put(key, value), right().right)
                );
            }
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
        //
        //---------------------------------------------------------------------------------------------

        public int height() {
            return 0;
        }

        public List<V> toList() {
            return List.nil();
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
