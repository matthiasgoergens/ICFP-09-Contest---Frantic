package de.hronopik.icfp2009.util;

import org.junit.Test;
import org.junit.Assert;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class ListsTest {

    @Test
    public void testSort() {
        LinkedList<String> list1 = LinkedList.newInstance3("1", "2", "3");
        LinkedList<String> list2 = LinkedList.newInstance3("3", "2", "1");
        Assert.assertEquals(list1, Lists.sort(list2));
    }
}
