package de.hronopik.icfp2009.vm;

import org.jetbrains.annotations.NotNull;

import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class InputLoggingVmWrapper implements Vm {

    @NotNull
    private final Vm vm;

    @NotNull
    private final StringBuilder sb = new StringBuilder();

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public InputLoggingVmWrapper(@NotNull Vm vm) {
        this.vm = vm;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    @NotNull
    public String getInput() {
        return sb.toString();
    }

    //---------------------------------------------------------------------------------------------
    // Vm Implementation
    //---------------------------------------------------------------------------------------------

    public int getStepIndex() {
        return vm.getStepIndex();
    }

    @NotNull
    public Map<Integer, Double> step() {
        sb.append(".\n");
        return vm.step();
    }

    @NotNull
    public Map<Integer, Double> step(@NotNull Map<Integer, Double> inputs) {
        for (Map.Entry<Integer, Double> entry : inputs.entrySet()) {
            sb.append(String.valueOf(entry.getKey()));
            sb.append(" ");
            sb.append(String.valueOf(entry.getValue()));
            sb.append("\n");
        }
        sb.append(".\n");
        return vm.step(inputs);
    }
}
