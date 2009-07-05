package de.hronopik.icfp2009.util;

import org.junit.Test;
import org.junit.Assert;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class ReadOnlyArrayTest {

    @Test
    public void testEmpty() {
        final ReadOnlyArray<Object> array = new ReadOnlyArray<Object>(List.nil());
        Assert.assertTrue("is empty", array.isEmpty());
        Assert.assertEquals("size is zero", 0, array.size());
        Assert.assertTrue("value at index -1 is Nothing", array.get(-1).isNothing());
        Assert.assertTrue("value at index 0 is Nothing", array.get(0).isNothing());
        Assert.assertTrue("value at index 1 is Nothing", array.get(1).isNothing());
    }

    @Test
    public void testSingle() {
        final ReadOnlyArray<Integer> array = new ReadOnlyArray<Integer>(LinkedList.newInstance1(1));
        Assert.assertFalse("is not empty", array.isEmpty());
        Assert.assertEquals("size is one", 1, array.size());
        Assert.assertTrue("value at index -1 is Nothing", array.get(-1).isNothing());
        Assert.assertTrue("value at index 0 is Just", array.get(0).isJust());
        Assert.assertTrue("value at index 1 is Nothing", array.get(1).isNothing());
        Assert.assertEquals("value at index 0 is 1", 1, ((Maybe.Just) array.get(0)).getValue());
    }
}
