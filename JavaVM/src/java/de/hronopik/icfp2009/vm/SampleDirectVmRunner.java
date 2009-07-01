package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.util.Map;
import de.hronopik.icfp2009.util.MaybeC;
import de.hronopik.icfp2009.util.Pair;

import java.io.IOException;
import java.util.HashMap;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class SampleDirectVmRunner {

    public static void main(String[] args) throws IOException {
        DirectVm vm = new DirectVm(args[0]);

        java.util.Map<Integer, Double> inputs = new HashMap<Integer, Double>();
        inputs.put(16000, 1001d);

        Map<Integer, Double> outputs;
        do {
            outputs = vm.step(inputs);
        } while (outputs.get(0).maybe(RUN));

        // Write the output
        for (Pair<Integer, Double> entry : outputs.mappingSet()) {
            System.out.println(entry.getKey() + " " + entry.getValue());
        }
    }

    private static final MaybeC<Boolean, Double> RUN = new MaybeC<Boolean, Double>() {

        public Boolean c(Double r) {
            return r == 0;
        }

        public Boolean c() {
            throw new RuntimeException("Score output missing.");
        }
    };
}
