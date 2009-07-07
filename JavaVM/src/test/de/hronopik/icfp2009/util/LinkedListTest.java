package de.hronopik.icfp2009.util;

import org.junit.Test;
import org.junit.Assert;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class LinkedListTest {

    @Test
    public void testNewInstance1() {
        LinkedList<String> list = LinkedList.newInstance1("1");
        Assert.assertFalse(list.isEmpty());
        Assert.assertEquals(1, list.size());
        Assert.assertEquals("1", list.head());
    }

    @Test
    public void testNewInstance2() {
        LinkedList<String> list = LinkedList.newInstance2("1", "2");
        Assert.assertFalse(list.isEmpty());
        Assert.assertEquals(2, list.size());
        Assert.assertEquals("1", list.head());
        Assert.assertEquals("2", ((List.Element<String>) list.drop(1)).head());
    }

    @Test
    public void testNewInstance3() {
        LinkedList<String> list = LinkedList.newInstance3("1", "2", "3");
        Assert.assertFalse(list.isEmpty());
        Assert.assertEquals(3, list.size());
        Assert.assertEquals("1", list.head());
        Assert.assertEquals("2", ((List.Element<String>) list.drop(1)).head());
        Assert.assertEquals("3", ((List.Element<String>) list.drop(2)).head());
    }

    @Test
    public void testReverse() {
        Assert.assertEquals("[3, 2, 1]", LinkedList.newInstance3("1", "2", "3").reverse().toString());
    }

    @Test
    public void testToString() {
        Assert.assertEquals("[1, 2, 3]", LinkedList.newInstance3("1", "2", "3").toString());
    }
}
