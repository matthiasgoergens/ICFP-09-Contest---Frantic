package de.hronopik.icfp2009.util;

import static de.hronopik.icfp2009.util.Maybe.just;
import static de.hronopik.icfp2009.util.Maybe.nothing;

import static java.lang.Math.max;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public abstract class IntAvlTree<V> implements Collection<V> {

    //---------------------------------------------------------------------------------------------
    // Constructors
    //---------------------------------------------------------------------------------------------

    private static final Empty<Object> EMPTY = new Empty<Object>();

    /**
     * Creates an empty {@linkplain IntAvlTree}.
     *
     * @param <V> the type of the value
     * @return an empty {@linkplain IntAvlTree}
     */
    @SuppressWarnings({"unchecked"})
    public static <V> Empty<V> empty() {
        return (Empty<V>) EMPTY;
    }

    public static <V> ZeroNode<V> singelton(int key, V value) {
        return IntAvlTree.<V>empty().put(key, value);
    }

    public static <V> IntAvlTree<V> fromList(List<V> list) {
        return list.foldLeft(new Empty<V>(), new Function2<IntAvlTree<V>, V, IntAvlTree<V>>() {
            public IntAvlTree<V> apply(IntAvlTree<V> tree, V value) {
                return tree.put(tree.size(), value);
            }
        });
    }

    /**
     * This private constructor prevents foreign instantiations.
     */
    private IntAvlTree() {
    }

    //---------------------------------------------------------------------------------------------
    // 
    //---------------------------------------------------------------------------------------------

    public abstract Maybe<V> get(int key);

    public abstract Node<V> put(int key, V value);

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public abstract int height();

    public abstract List<V> toList();

    //---------------------------------------------------------------------------------------------
    // Node
    //---------------------------------------------------------------------------------------------

    public static abstract class Node<V> extends IntAvlTree<V> {

        final int key;
        final V value;

        final IntAvlTree<V> left;
        final IntAvlTree<V> right;

        //---------------------------------------------------------------------------------------------
        // Constructor
        //---------------------------------------------------------------------------------------------

        private Node(int key, V value, IntAvlTree<V> left, IntAvlTree<V> right) {
            assert left instanceof Empty || ((Node<V>) left).key < key : "left key is smaller";
            assert right instanceof Empty || ((Node<V>) right).key > key : "right key is bigger";
            this.key = key;
            this.value = value;
            this.left = left;
            this.right = right;
        }

        //---------------------------------------------------------------------------------------------
        // Iterable Implementation
        //---------------------------------------------------------------------------------------------

        public Iterator<V> iterator() {
            return null;
        }

        public <T> Collection<T> map(Function1<V, T> mapper) {
            return null;
        }

        public Collection<V> filter(Function1<V, Boolean> p) {
            return null;
        }

        public <T> T foldLeft(T start, Function2<T, V, T> f) {
            return null;
        }

        public <T> T foldRight(T start, Function2<V, T, T> f) {
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

        public boolean contains(V element) {
            return toList().contains(element);
        }

        //---------------------------------------------------------------------------------------------
        // Map Implementation
        //---------------------------------------------------------------------------------------------

        public Maybe<V> get(int key) {
            if (key < this.key) {
                return left.get(key);
            } else if (key > this.key) {
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

        public int getKey() {
            return key;
        }

        public V getValue() {
            return value;
        }

        public IntAvlTree<V> getLeft() {
            return left;
        }

        public IntAvlTree<V> getRight() {
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
     * @param <V> the type of the value
     */
    public static final class PositiveNode<V> extends Node<V> {

        private PositiveNode(int key, V value, Node<V> left, IntAvlTree<V> right) {
            super(key, value, left, right);
            assert left.height() == right.height() + 1 : "left height is bigger";
        }

        public Node<V> put(int key, V value) {
            if (key < this.key) {
                return putLeft(key, value);
            } else if (key > this.key) {
                return putRight(key, value);
            } else {
                return putHere(key, value);
            }
        }

        private Node<V> left() {
            assert left instanceof Node : "left is a node";
            return (Node<V>) left;
        }

        /**
         * Puts the key value pair in the left subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see IntAvlTree.NegativeNode#putRight
         */
        private Node<V> putLeft(int key, V value) {
            if (left instanceof ZeroNode) {
                if (key < left().key) {
                    return putAndRotateLeftLeft(key, value);
                } else if (key > left().key) {
                    return putAndRotateLeftRight(key, value);
                } else {
                    return new PositiveNode<V>(this.key, this.value, left.put(key, value), right);
                }
            } else {
                return new PositiveNode<V>(this.key, this.value, left.put(key, value), right);
            }
        }

        /**
         * Puts the key value pair in the right subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see IntAvlTree.NegativeNode#putLeft
         */
        private Node<V> putRight(int key, V value) {
            final Node<V> newRight = right.put(key, value);
            if (right instanceof Empty) {
                return new ZeroNode<V>(this.key, this.value, left, newRight);
            } else if (right instanceof ZeroNode) {
                if (newRight instanceof ZeroNode) {
                    return new PositiveNode<V>(this.key, this.value, (Node<V>) left, newRight);
                } else {
                    return new ZeroNode<V>(this.key, this.value, left, newRight);
                }
            } else {
                return new PositiveNode<V>(this.key, this.value, (Node<V>) left, newRight);
            }
        }

        /**
         * Update existing node.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see IntAvlTree.NegativeNode#putHere
         */
        private PositiveNode<V> putHere(int key, V value) {
            return new PositiveNode<V>(key, value, left(), right);
        }

        private Node<V> putAndRotateLeftLeft(int key, V value) {
            if (left().left instanceof Empty) {
                assert right instanceof Empty : "right is also empty";
                assert left().right instanceof Empty : "left right is also empty";

                return new ZeroNode<V>(left().key
                        , left().value
                        , new ZeroNode<V>(key, value, IntAvlTree.<V>empty(), IntAvlTree.<V>empty())
                        , new ZeroNode<V>(this.key, this.value, left().right, this.right)
                );
            } else if (left().left instanceof ZeroNode) {
                final Node<V> leftLeft = left().left.put(key, value);
                if (leftLeft instanceof ZeroNode) {
                    return new PositiveNode<V>(this.key
                            , this.value
                            , new ZeroNode<V>(left().key, left().value, leftLeft, left().right)
                            , right
                    );
                } else {
                    return new ZeroNode<V>(left().key
                            , left().value
                            , leftLeft
                            , new ZeroNode<V>(this.key, this.value, left().right, right)
                    );
                }
            } else {
                return new PositiveNode<V>(this.key
                        , this.value
                        , new ZeroNode<V>(left().key, left().value, left().left.put(key, value), left().right)
                        , right
                );
            }
        }

        private Node<V> putAndRotateLeftRight(int key, V value) {
            if (left().right instanceof Empty) {
                assert right instanceof Empty : "right is also empty";
                assert left().left instanceof Empty : "left left is also empty";

                return new ZeroNode<V>(key
                        , value
                        , new ZeroNode<V>(left().key, left().value, left().left, IntAvlTree.<V>empty())
                        , new ZeroNode<V>(this.key, this.value, IntAvlTree.<V>empty(), this.right)
                );
            } else if (left().right instanceof ZeroNode) {
                final Node<V> leftRight = left().right.put(key, value);
                if (leftRight instanceof ZeroNode) {
                    return new PositiveNode<V>(this.key
                            , this.value
                            , new ZeroNode<V>(left().key, left().value, left().left, leftRight)
                            , right
                    );
                } else if (leftRight instanceof NegativeNode) {
                    return new ZeroNode<V>(leftRight.key
                            , leftRight.value
                            , new PositiveNode<V>(left().key, left().value, (Node<V>) left().left, leftRight.left)
                            , new ZeroNode<V>(this.key, this.value, leftRight.right, right)
                    );
                } else if (leftRight instanceof PositiveNode) {
                    return new ZeroNode<V>(leftRight.key
                            , leftRight.value
                            , new ZeroNode<V>(left().key, left().value, left().left, leftRight.left)
                            , new NegativeNode<V>(this.key, this.value, leftRight.right, (Node<V>) right)
                    );
                } else {
                    throw new RuntimeException("empty node not possible");
                }
            } else {
                return new PositiveNode<V>(this.key
                        , this.value
                        , new ZeroNode<V>(left().key, left().value, left().left, left().right.put(key, value))
                        , right
                );
            }
        }
    }

    //---------------------------------------------------------------------------------------------
    // ZeroNode
    //---------------------------------------------------------------------------------------------

    public static final class ZeroNode<V> extends Node<V> {

        private ZeroNode(int key, V value, IntAvlTree<V> left, IntAvlTree<V> right) {
            super(key, value, left, right);
            assert left.height() == right.height() : "heights are equal";
        }

        public Node<V> put(int key, V value) {
            if (key < this.key) {
                return putLeft(key, value);
            } else if (key > this.key) {
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
        private Node<V> putLeft(int key, V value) {
            if (left instanceof Empty) {
                assert right instanceof Empty : "right is also empty";
                return new PositiveNode<V>(this.key, this.value, left.put(key, value), right);
            } else if (left instanceof ZeroNode) {
                final Node<V> newLeft = left.put(key, value);
                if (newLeft instanceof ZeroNode) {
                    return new ZeroNode<V>(this.key, this.value, newLeft, right);
                } else {
                    return new PositiveNode<V>(this.key, this.value, newLeft, right);
                }
            } else {
                return new ZeroNode<V>(this.key, this.value, left.put(key, value), right);
            }
        }

        /**
         * Puts the key value pair in the right subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         */
        private Node<V> putRight(int key, V value) {
            if (right instanceof Empty) {
                assert left instanceof Empty : "left is also empty";
                return new NegativeNode<V>(this.key, this.value, left, right.put(key, value));
            } else if (right instanceof ZeroNode) {
                final Node<V> newRight = right.put(key, value);
                if (newRight instanceof ZeroNode) {
                    return new ZeroNode<V>(this.key, this.value, left, newRight);
                } else {
                    return new NegativeNode<V>(this.key, this.value, left, newRight);
                }
            } else {
                return new ZeroNode<V>(this.key, this.value, left, right.put(key, value));
            }
        }

        /**
         * Update existing node.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         */
        private ZeroNode<V> putHere(int key, V value) {
            return new ZeroNode<V>(key, value, left, right);
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
     * @param <V> the type of the value
     */
    public static final class NegativeNode<V> extends Node<V> {

        private NegativeNode(int key, V value, IntAvlTree<V> left, Node<V> right) {
            super(key, value, left, right);
            assert left.height() + 1 == right.height() : "right height is bigger";
        }

        public Node<V> put(int key, V value) {
            if (key < this.key) {
                return putLeft(key, value);
            } else if (key > this.key) {
                return putRight(key, value);
            } else {
                return putHere(key, value);
            }
        }

        private Node<V> right() {
            assert right instanceof Node : "right is a node";
            return (Node<V>) right;
        }

        /**
         * Puts the key value pair in the left subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see IntAvlTree.PositiveNode#putRight
         */
        private Node<V> putLeft(int key, V value) {
            final Node<V> newLeft = left.put(key, value);
            if (left instanceof Empty) {
                return new ZeroNode<V>(this.key, this.value, newLeft, right);
            } else if (left instanceof ZeroNode) {
                if (newLeft instanceof ZeroNode) {
                    return new NegativeNode<V>(this.key, this.value, newLeft, (Node<V>) right);
                } else {
                    return new ZeroNode<V>(this.key, this.value, newLeft, right);
                }
            } else {
                return new NegativeNode<V>(this.key, this.value, newLeft, (Node<V>) right);
            }
        }

        /**
         * Puts the key value pair in the right subtree.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see IntAvlTree.PositiveNode#putLeft
         */
        private Node<V> putRight(int key, V value) {
            if (right instanceof ZeroNode) {
                if (key > right().key) {
                    return putAndRotateRightRight(key, value);
                } else if (key < right().key) {
                    return putAndRotateRightLeft(key, value);
                } else {
                    return new NegativeNode<V>(this.key, this.value, left, right.put(key, value));
                }
            } else {
                return new NegativeNode<V>(this.key, this.value, left, right.put(key, value));
            }
        }

        /**
         * Update existing node.
         *
         * @param key   the key to put
         * @param value the value to put
         * @return a new instance of this node
         * @see IntAvlTree.PositiveNode#putHere
         */
        private NegativeNode<V> putHere(int key, V value) {
            return new NegativeNode<V>(key, value, left, right());
        }

        private Node<V> putAndRotateRightRight(int key, V value) {
            if (right().right instanceof Empty) {
                assert left instanceof Empty : "left is also empty";
                assert right().left instanceof Empty : "right left is also empty";

                return new ZeroNode<V>(right().key
                        , right().value
                        , new ZeroNode<V>(this.key, this.value, this.left, right().left)
                        , new ZeroNode<V>(key, value, IntAvlTree.<V>empty(), IntAvlTree.<V>empty())
                );
            } else if (right().right instanceof ZeroNode) {
                final Node<V> rightRight = right().right.put(key, value);
                if (rightRight instanceof ZeroNode) {
                    return new NegativeNode<V>(this.key
                            , this.value
                            , left
                            , new ZeroNode<V>(right().key, right().value, right().left, rightRight)
                    );
                } else {
                    return new ZeroNode<V>(right().key
                            , right().value
                            , new ZeroNode<V>(this.key, this.value, left, right().left)
                            , rightRight
                    );
                }
            } else {
                return new NegativeNode<V>(this.key
                        , this.value
                        , left
                        , new ZeroNode<V>(right().key, right().value, right().left, right().right.put(key, value))
                );
            }
        }

        private Node<V> putAndRotateRightLeft(int key, V value) {
            if (right().left instanceof Empty) {
                assert left instanceof Empty : "left is also empty";
                assert right().right instanceof Empty : "right right is also empty";

                return new ZeroNode<V>(key
                        , value
                        , new ZeroNode<V>(this.key, this.value, this.left, IntAvlTree.<V>empty())
                        , new ZeroNode<V>(right().key, right().value, IntAvlTree.<V>empty(), right().right)
                );
            } else if (right().left instanceof ZeroNode) {
                final Node<V> rightLeft = right().left.put(key, value);
                if (rightLeft instanceof ZeroNode) {
                    return new NegativeNode<V>(this.key
                            , this.value
                            , left
                            , new ZeroNode<V>(right().key, right().value, rightLeft, right().right)
                    );
                } else if (rightLeft instanceof PositiveNode) {
                    return new ZeroNode<V>(rightLeft.key
                            , rightLeft.value
                            , new ZeroNode<V>(this.key, this.value, left, rightLeft.left)
                            , new NegativeNode<V>(right().key, right().value, rightLeft.right, (Node<V>) right().right)
                    );
                } else if (rightLeft instanceof NegativeNode) {
                    return new ZeroNode<V>(rightLeft.key
                            , rightLeft.value
                            , new PositiveNode<V>(this.key, this.value, (Node<V>) left, rightLeft.left)
                            , new ZeroNode<V>(right().key, right().value, rightLeft.right, right().right)
                    );
                } else {
                    throw new RuntimeException("empty node not possible");
                }
            } else {
                return new NegativeNode<V>(this.key
                        , this.value
                        , left
                        , new ZeroNode<V>(right().key, right().value, right().left.put(key, value), right().right)
                );
            }
        }
    }

    //---------------------------------------------------------------------------------------------
    // Empty
    //---------------------------------------------------------------------------------------------

    public static final class Empty<V> extends IntAvlTree<V> {

        private Empty() {
        }

        //---------------------------------------------------------------------------------------------
        // Iterable Implementation
        //---------------------------------------------------------------------------------------------

        public Iterator<V> iterator() {
            return null;
        }

        public <T> Collection<T> map(Function1<V, T> mapper) {
            return null;
        }

        public Collection<V> filter(Function1<V, Boolean> p) {
            return null;
        }

        public <T> T foldLeft(T start, Function2<T, V, T> f) {
            return null;
        }

        public <T> T foldRight(T start, Function2<V, T, T> f) {
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

        public boolean contains(V element) {
            return false;
        }

        //---------------------------------------------------------------------------------------------
        // Map Implementation
        //---------------------------------------------------------------------------------------------

        public Maybe<V> get(int key) {
            return nothing();
        }

        public ZeroNode<V> put(int key, V value) {
            return new ZeroNode<V>(key, value, IntAvlTree.<V>empty(), IntAvlTree.<V>empty());
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