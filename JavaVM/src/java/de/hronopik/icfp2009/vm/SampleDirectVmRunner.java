package de.hronopik.icfp2009.vm;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class SampleDirectVmRunner {

    public static void main(String[] args) throws IOException {
        DirectVm vm = new DirectVm(args[0]);

        Map<Integer, Double> inputs = new HashMap<Integer, Double>();
        inputs.put(16000, 1001d);

        Map<Integer, Double> outputs;
        do {
            outputs = vm.step(inputs);
        } while (outputs.get(0) == 0);

        // Write the output
        for (Map.Entry<Integer, Double> entry : outputs.entrySet()) {
            System.out.println(entry.getKey() + " " + entry.getValue());
        }
    }
}
