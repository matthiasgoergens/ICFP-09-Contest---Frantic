package de.hronopik.icfp2009.util;

import org.junit.Test;
import org.junit.Assert;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static de.hronopik.icfp2009.util.AvlTree.fromList;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class AvlTreeTest {

    @Test
    public void testPutLL() {

        // Build initial tree
        final AvlTree.Node<Integer, String> tree = AvlTree.singelton(4, "4").put(3, "3").put(5, "5");

        testZeroNode(tree, 4, "4");
        testZeroNode(tree.getLeft(), 3, "3");
        testZeroNode(tree.getRight(), 5, "5");

        // Put (2, "2")
        final AvlTree.Node<Integer, String> tree2 = tree.put(2, "2");

        testPositiveNode(tree2, 4, "4");
        final AvlTree.Node<Integer, String> left2 = testPositiveNode(tree2.getLeft(), 3, "3");
        testZeroNode(tree2.getRight(), 5, "5");
        testZeroNode(left2.getLeft(), 2, "2");

        // Put (1, "1")
        final AvlTree.Node<Integer, String> tree1 = tree2.put(1, "1");
        testPositiveNode(tree1, 4, "4");
        final AvlTree.Node<Integer, String> left1 = testZeroNode(tree1.getLeft(), 2, "2");
        testZeroNode(tree.getRight(), 5, "5");
        testZeroNode(left1.getLeft(), 1, "1");
        testZeroNode(left1.getRight(), 3, "3");
    }

    @Test
    public void testPutLR() {

        // Build initial tree
        final AvlTree.Node<Integer, String> tree = AvlTree.singelton(4, "4").put(3, "3").put(5, "5");

        testZeroNode(tree, 4, "4");
        testZeroNode(tree.getLeft(), 3, "3");
        testZeroNode(tree.getRight(), 5, "5");

        // Put (1, "1")
        final AvlTree.Node<Integer, String> tree1 = tree.put(1, "1");

        testPositiveNode(tree1, 4, "4");
        final AvlTree.Node<Integer, String> left1 = testPositiveNode(tree1.getLeft(), 3, "3");
        testZeroNode(tree1.getRight(), 5, "5");
        testZeroNode(left1.getLeft(), 1, "1");

        // Put (2, "2")
        final AvlTree.Node<Integer, String> tree2 = tree1.put(2, "2");
        testPositiveNode(tree2, 4, "4");
        final AvlTree.Node<Integer, String> left2 = testZeroNode(tree2.getLeft(), 2, "2");
        testZeroNode(tree.getRight(), 5, "5");
        testZeroNode(left2.getLeft(), 1, "1");
        testZeroNode(left2.getRight(), 3, "3");
    }

    @Test
    public void testPutRR() {

        // Build initial tree
        final AvlTree.Node<Integer, String> tree = AvlTree.singelton(4, "4").put(3, "3").put(5, "5");

        testZeroNode(tree, 4, "4");
        testZeroNode(tree.getLeft(), 3, "3");
        testZeroNode(tree.getRight(), 5, "5");

        // Put (6, "6")
        final AvlTree.Node<Integer, String> tree6 = tree.put(6, "6");

        testNegativeNode(tree6, 4, "4");
        testZeroNode(tree6.getLeft(), 3, "3");
        final AvlTree.Node<Integer, String> right6 = testNegativeNode(tree6.getRight(), 5, "5");
        testZeroNode(right6.getRight(), 6, "6");

        // Put (7, "7")
        final AvlTree.Node<Integer, String> tree67 = tree6.put(7, "7");
        testNegativeNode(tree67, 4, "4");
        testZeroNode(tree.getLeft(), 3, "3");
        final AvlTree.Node<Integer, String> right67 = testZeroNode(tree67.getRight(), 6, "6");
        testZeroNode(right67.getLeft(), 5, "5");
        testZeroNode(right67.getRight(), 7, "7");
    }

    @Test
    public void testPutRL() {

        // Build initial tree
        final AvlTree.Node<Integer, String> tree = AvlTree.singelton(4, "4").put(3, "3").put(5, "5");

        testZeroNode(tree, 4, "4");
        testZeroNode(tree.getLeft(), 3, "3");
        testZeroNode(tree.getRight(), 5, "5");

        // Put (7, "7")
        final AvlTree.Node<Integer, String> tree7 = tree.put(7, "7");

        testNegativeNode(tree7, 4, "4");
        testZeroNode(tree7.getLeft(), 3, "3");
        final AvlTree.Node<Integer, String> right7 = testNegativeNode(tree7.getRight(), 5, "5");
        testZeroNode(right7.getRight(), 7, "7");

        // Put (6, "6")
        final AvlTree.Node<Integer, String> tree6 = tree7.put(6, "6");
        testNegativeNode(tree6, 4, "4");
        testZeroNode(tree.getLeft(), 3, "3");
        final AvlTree.Node<Integer, String> right6 = testZeroNode(tree6.getRight(), 6, "6");
        testZeroNode(right6.getLeft(), 5, "5");
        testZeroNode(right6.getRight(), 7, "7");
    }

    @Test
    public void testFromList() {
        assertTrue(fromList(List.nil()).isEmpty());

        final AvlTree<Integer, String> tree = fromList(LinkedList.newInstance1("0"));
        testZeroNode(tree, 0, "0");

        final AvlTree<Integer, String> tree1 = fromList(LinkedList.newInstance2("0", "1"));
        final AvlTree.Node<Integer, String> node1 = testNegativeNode(tree1, 0, "0");
        testZeroNode(node1.getRight(), 1, "1");

        final AvlTree<Integer, String> tree2 = fromList(LinkedList.newInstance3("0", "1", "2"));
        final AvlTree.Node<Integer, String> node2 = testZeroNode(tree2, 1, "1");
        testZeroNode(node2.getLeft(), 0, "0");
        testZeroNode(node2.getRight(), 2, "2");

        final AvlTree<Integer, String> tree3 = fromList(LinkedList.newInstance4("0", "1", "2", "3"));
        final AvlTree.Node<Integer, String> node3 = testNegativeNode(tree3, 1, "1");
        testZeroNode(node3.getLeft(), 0, "0");
        final AvlTree.Node<Integer, String> node33 = testNegativeNode(node3.getRight(), 2, "2");
        testZeroNode(node33.getRight(), 3, "3");
    }

    @Test
    public void testToList() {
        assertTrue(fromList(List.nil()).toList().isEmpty());

        assertEquals(LinkedList.newInstance1("0"), fromList(LinkedList.newInstance1("0")).toList());
        assertEquals(LinkedList.newInstance2("0", "1"), fromList(LinkedList.newInstance2("0", "1")).toList());
        assertEquals(LinkedList.newInstance3("0", "1", "2"), fromList(LinkedList.newInstance3("0", "1", "2")).toList());
        assertEquals(LinkedList.newInstance4("0", "1", "2", "3"), fromList(LinkedList.newInstance4("0", "1", "2", "3")).toList());
    }


    //---------------------------------------------------------------------------------------------
    // Helper Methods
    //---------------------------------------------------------------------------------------------

    private <K extends Comparable<? super K>, V> AvlTree.Node<K, V> testPositiveNode(AvlTree<K, V> tree,
                                                                                     K key, V value) {
        assertTrue("positive node", tree instanceof AvlTree.PositiveNode);
        return testNode((AvlTree.Node<K,V>) tree, key, value);
    }

    private <K extends Comparable<? super K>, V> AvlTree.Node<K, V> testZeroNode(AvlTree<K, V> tree,
                                                                                     K key, V value) {
        assertTrue(tree instanceof AvlTree.ZeroNode);
        return testNode((AvlTree.Node<K,V>) tree, key, value);
    }

    private <K extends Comparable<? super K>, V> AvlTree.Node<K, V> testNegativeNode(AvlTree<K, V> tree,
                                                                                     K key, V value) {
        assertTrue(tree instanceof AvlTree.NegativeNode);
        return testNode((AvlTree.Node<K,V>) tree, key, value);
    }

    private <K extends Comparable<? super K>, V> AvlTree.Node<K, V> testNode(AvlTree.Node<K, V> tree,
                                                                             K key, V value) {

        assertEquals(key, tree.getKey());
        assertEquals(value, tree.getValue());

        return tree;
    }
}
