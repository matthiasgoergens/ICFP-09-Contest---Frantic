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
        assertEquals("1", list.head());
    }

    @Test
    public void testNewInstance2() {
        LinkedList<String> list = LinkedList.newInstance2("1", "2");
        Assert.assertFalse(list.isEmpty());
        Assert.assertEquals(2, list.size());
        assertEquals("1", list.head());
        assertEquals("2", list.drop(1).head());
    }

    @Test
    public void testNewInstance3() {
        LinkedList<String> list = LinkedList.newInstance3("1", "2", "3");
        Assert.assertFalse(list.isEmpty());
        Assert.assertEquals(3, list.size());
        assertEquals("1", list.head());
        assertEquals("2", list.drop(1).head());
        assertEquals("3", list.drop(2).head());
    }

    @Test
    public void testToString() {
        Assert.assertEquals("[1, 2, 3]", LinkedList.newInstance3("1", "2", "3").toString()); 
    }

    //---------------------------------------------------------------------------------------------
    // Helper Methods
    //---------------------------------------------------------------------------------------------

    public static <T> void assertEquals(final Object expected, final Maybe<T> actual) {
        actual.maybe(new MaybeC<Void, T>() {
            public Void c(T r) {
                Assert.assertEquals(expected, r);
                return null;
            }

            public Void c() {
                Assert.fail();
                return null;
            }
        });
    }
}
