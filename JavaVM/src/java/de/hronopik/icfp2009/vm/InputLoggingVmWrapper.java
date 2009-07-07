package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.util.Function2;
import de.hronopik.icfp2009.util.Map;
import de.hronopik.icfp2009.util.Pair;
import de.hronopik.icfp2009.util.Pairs;

/**
 *
 *
 * @author Alexander Kiel
 * @version $Id$
 */
public class InputLoggingVmWrapper<T extends Vm<T>> implements Vm<InputLoggingVmWrapper<T>> {

    private final T vm;

    private final StringBuilder sb;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public InputLoggingVmWrapper(T vm) {
        this(vm, new StringBuilder());
    }

    private InputLoggingVmWrapper(T vm, StringBuilder sb) {
        this.vm = vm;
        this.sb = sb;
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

    public Pair<InputLoggingVmWrapper<T>, Map<Integer, Double>> step() {
        final Pair<T, Map<Integer, Double>> pair = vm.step();
        return Pairs.newPair(new InputLoggingVmWrapper<T>(pair.getFst(), sb), pair.getSnd());
    }

    public Pair<InputLoggingVmWrapper<T>, Map<Integer, Double>> step(Map<Integer, Double> input) {
        final StringBuilder sb = input.foldLeft(this.sb, new Function2<StringBuilder, Pair<Integer, Double>, StringBuilder>() {
            public StringBuilder apply(StringBuilder stringBuilder, Pair<Integer, Double> integerDoublePair) {
                return stringBuilder.append(integerDoublePair.getFst()).append(" ").append(integerDoublePair.getSnd()).append('\n');
            }
        }).append(".\n");
        final Pair<T, Map<Integer, Double>> pair = vm.step(input);
        return Pairs.newPair(new InputLoggingVmWrapper<T>(pair.getFst(), sb), pair.getSnd());
    }
}
