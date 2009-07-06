package de.hronopik.icfp2009.vm;

import org.junit.Test;
import org.junit.Assert;
import org.junit.Before;
import de.hronopik.icfp2009.util.LinkedList;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class AvlMemoryTest {

    Memory memory;

    @Before
    public void setUp() {
        memory = new AvlMemory(LinkedList.newInstance4(1d, 2d, 3d, 4d), false);
    }

    @Test
    public void testRead() {
        Assert.assertEquals(1d, memory.getValue(0), 0d);
        Assert.assertEquals(2d, memory.getValue(1), 0d);
        Assert.assertEquals(3d, memory.getValue(2), 0d);
        Assert.assertEquals(4d, memory.getValue(3), 0d);
    }

    @Test
    public void testWrite() {
        memory = memory.setValue(11d);
        Assert.assertEquals(11d, memory.getValue(0), 0d);
        Assert.assertEquals(2d, memory.getValue(1), 0d);
        Assert.assertEquals(3d, memory.getValue(2), 0d);
        Assert.assertEquals(4d, memory.getValue(3), 0d);

        memory = memory.setValue(12d);
        Assert.assertEquals(11d, memory.getValue(0), 0d);
        Assert.assertEquals(12d, memory.getValue(1), 0d);
        Assert.assertEquals(3d, memory.getValue(2), 0d);
        Assert.assertEquals(4d, memory.getValue(3), 0d);

        memory = memory.setValue(13d);
        Assert.assertEquals(11d, memory.getValue(0), 0d);
        Assert.assertEquals(12d, memory.getValue(1), 0d);
        Assert.assertEquals(13d, memory.getValue(2), 0d);
        Assert.assertEquals(4d, memory.getValue(3), 0d);

        memory = memory.setValue(14d);
        Assert.assertEquals(11d, memory.getValue(0), 0d);
        Assert.assertEquals(12d, memory.getValue(1), 0d);
        Assert.assertEquals(13d, memory.getValue(2), 0d);
        Assert.assertEquals(14d, memory.getValue(3), 0d);

        memory = memory.setValue(21d);
        Assert.assertEquals(21d, memory.getValue(0), 0d);
        Assert.assertEquals(12d, memory.getValue(1), 0d);
        Assert.assertEquals(13d, memory.getValue(2), 0d);
        Assert.assertEquals(14d, memory.getValue(3), 0d);

        memory = memory.setValue(22d);
        Assert.assertEquals(21d, memory.getValue(0), 0d);
        Assert.assertEquals(22d, memory.getValue(1), 0d);
        Assert.assertEquals(13d, memory.getValue(2), 0d);
        Assert.assertEquals(14d, memory.getValue(3), 0d);
    }
}