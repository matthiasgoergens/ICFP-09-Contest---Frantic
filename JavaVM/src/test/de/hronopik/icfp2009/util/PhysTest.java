package de.hronopik.icfp2009.util;

import org.junit.Assert;
import org.junit.Test;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class PhysTest {

    @Test
    public void testHohmannSpeed1() {
        Assert.assertEquals(2466.486012212709, Phys.hohmannSpeed1(6557000, 42164000), 0);
    }

    @Test
    public void testHohmannSpeed2() {
        Assert.assertEquals(1482.9355710372588, Phys.hohmannSpeed2(6557000, 42164000), 0);
    }
}
