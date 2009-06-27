package de.hronopik.icfp2009.controller;

import de.hronopik.icfp2009.util.Phys;
import de.hronopik.icfp2009.util.Vector;
import de.hronopik.icfp2009.vm.DirectVm;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Task1Controller {

    private final String filename;

    private Vector targetPoint = new Vector(42164000, 0);
    private Vector oldPoint = new Vector(0, 42164000);

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public Task1Controller(String filename) {
        this.filename = filename;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public void run() throws IOException {
        DirectVm vm = new DirectVm(filename);

        Map<Integer, Double> inputs = new HashMap<Integer, Double>();
        inputs.put(16000, 1001d);
        inputs.put(2, 0d);
        inputs.put(3, -2466.485);
        //inputs.put(3, -2466.486012212709);

        Map<Integer, Double> outputs = vm.step(inputs);

        while (cont(vm.getStepIndex(), outputs)) {
            outputs = vm.step(Collections.<Integer, Double>emptyMap());
        }

        System.out.println("stepIndex = " + vm.getStepIndex());

        vm.undo();

        System.out.println("stepIndex = " + vm.getStepIndex());
        System.out.println("delta r = " + (outputs.get(4) - Phys.radius(oldPoint)));
        System.out.println("delta s = " + Phys.distance(targetPoint, oldPoint));

        // Write the output
        for (Map.Entry<Integer, Double> entry : outputs.entrySet()) {
            System.out.println(entry.getKey() + " " + entry.getValue());
        }

        double hohmannSpeed2 = Phys.hohmannSpeed2(6557000, Phys.radius(oldPoint));
        System.out.println("hohmannSpeed2 = " + hohmannSpeed2);

        inputs.put(2, 0d);
        inputs.put(3, hohmannSpeed2);

        outputs = vm.step(inputs);

        int h2Step = vm.getStepIndex();

        while (vm.getStepIndex() < h2Step + 901) {
            outputs = vm.step(Collections.<Integer, Double>emptyMap());
        }

        Vector currentPoint = new Vector(outputs.get(2), outputs.get(3));

        System.out.println("stepIndex = " + vm.getStepIndex());
        System.out.println("delta r = " + (outputs.get(4) - Phys.radius(currentPoint)));
        System.out.println("delta s = " + Phys.distance(targetPoint, currentPoint));

        // Write the output
        for (Map.Entry<Integer, Double> entry : outputs.entrySet()) {
            System.out.println(entry.getKey() + " " + entry.getValue());
        }
    }

    private boolean cont(int stepIndex, Map<Integer, Double> outputs) {
        if (stepIndex < 18800) {
            return true;
        }

        Vector currentPoint = new Vector(outputs.get(2), outputs.get(3));
        double oldTargetDistance = Phys.distance(targetPoint, oldPoint);
        double targetDistance = Phys.distance(targetPoint, currentPoint);

        System.out.println(stepIndex + ": delta s = " + targetDistance);
        if (oldTargetDistance > targetDistance) {
            oldPoint = currentPoint;
            return true;
        }

        return false;
    }

    public static void main(String[] args) throws IOException {
        new Task1Controller(args[0]).run();
    }
}
