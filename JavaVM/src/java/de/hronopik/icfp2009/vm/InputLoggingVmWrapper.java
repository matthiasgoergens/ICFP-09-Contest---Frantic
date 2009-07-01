package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class InputLoggingVmWrapper implements Vm {


    private final Vm vm;


    private final StringBuilder sb = new StringBuilder();

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public InputLoggingVmWrapper(Vm vm) {
        this.vm = vm;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------


    public String getInput() {
        return sb.toString();
    }

    //---------------------------------------------------------------------------------------------
    // Vm Implementation
    //---------------------------------------------------------------------------------------------

    public int getStepIndex() {
        return vm.getStepIndex();
    }


    public Map<Integer, Double> step() {
        sb.append(".\n");
        return vm.step();
    }


    public Map<Integer, Double> step(java.util.Map<Integer, Double> inputs) {
        for (java.util.Map.Entry<Integer, Double> entry : inputs.entrySet()) {
            sb.append(String.valueOf(entry.getKey()));
            sb.append(" ");
            sb.append(String.valueOf(entry.getValue()));
            sb.append("\n");
        }
        sb.append(".\n");
        return vm.step(inputs);
    }
}
