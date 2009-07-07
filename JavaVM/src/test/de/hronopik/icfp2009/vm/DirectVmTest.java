package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.util.InputBuilder;
import de.hronopik.icfp2009.util.Map;
import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.util.Collections;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class DirectVmTest {

    DirectVm vm;

    @Before
    public void setUp() throws IOException {
        // Patrick: please run the test in icfp2009/run like all other things
        vm = new DirectVm("../task/bin1.obf");
    }

    @Test
    public void testStartSnapShoot1() {
        vm.createSnapshoot("start");
        assertEquals(0, vm.getStepIndex());

        Map<Integer, Double> output1 = vm.step(Collections.<Integer, Double>emptyMap());
        assertEquals(1, vm.getStepIndex());

        vm.reset("start");
        assertEquals(0, vm.getStepIndex());

        Map<Integer, Double> output2 = vm.step(Collections.<Integer, Double>emptyMap());
        assertEquals(1, vm.getStepIndex());

        assertEquals(output1, output2);
    }

    @Test
    public void testStartSnapShoot2() {
        vm.createSnapshoot("start");
        assertEquals(0, vm.getStepIndex());

        Map<Integer, Double> output1 = vm.step(Collections.<Integer, Double>emptyMap());
        assertEquals(1, vm.getStepIndex());
        vm.step(Collections.<Integer, Double>emptyMap());
        assertEquals(2, vm.getStepIndex());
        vm.step(Collections.<Integer, Double>emptyMap());
        assertEquals(3, vm.getStepIndex());

        vm.reset("start");
        assertEquals(0, vm.getStepIndex());

        Map<Integer, Double> output2 = vm.step(Collections.<Integer, Double>emptyMap());
        assertEquals(1, vm.getStepIndex());

        assertEquals(output1, output2);
    }

    @Test
    public void testStartSnapShoot3() {
        vm.createSnapshoot("start");
        assertEquals(0, vm.getStepIndex());

        java.util.Map<Integer, Double> inputs = new InputBuilder().add(16000, 1001).add(3, -2466.484135).build();

        Map<Integer, Double> output1 = vm.step(inputs);
        assertEquals(1, vm.getStepIndex());
        vm.step(Collections.<Integer, Double>emptyMap());
        assertEquals(2, vm.getStepIndex());
        vm.step(Collections.<Integer, Double>emptyMap());
        assertEquals(3, vm.getStepIndex());

        vm.reset("start");
        assertEquals(0, vm.getStepIndex());

        Map<Integer, Double> output2 = vm.step(inputs);
        assertEquals(1, vm.getStepIndex());

        assertEquals(output1, output2);
    }
}
