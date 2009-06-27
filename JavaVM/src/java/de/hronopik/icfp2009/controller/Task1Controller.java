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
        inputs.put(3, -2466.486012212709);

        Map<Integer, Double> outputs = vm.step(inputs);

        while (vm.getStepIndex() < 18868) {
            outputs = vm.step(Collections.<Integer, Double>emptyMap());
        }

        System.out.println("stepIndex = " + vm.getStepIndex());
        System.out.println("delta r = " + Math.abs(outputs.get(4) - Phys.radius(new Vector(outputs.get(2), outputs.get(3)))));

        // Write the output
        for (Map.Entry<Integer, Double> entry : outputs.entrySet()) {
            System.out.println(entry.getKey() + " " + entry.getValue());
        }
    }

    private boolean continute(Map<Integer, Double> outputs) {
        return outputs.get(3) > 0;
    }

    public static void main(String[] args) throws IOException {
        new Task1Controller(args[0]).run();
    }
}
