package de.hronopik.icfp2009.controller;

import de.hronopik.icfp2009.util.InputBuilder;
import de.hronopik.icfp2009.util.Pair;
import static de.hronopik.icfp2009.util.Phys.radius;
import de.hronopik.icfp2009.util.Vector;
import de.hronopik.icfp2009.vm.DirectVm;
import de.hronopik.icfp2009.vm.InputLoggingVmWrapper;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Task1FuelBurningController {

    private final static double R_EARTH = 6.357E6;

    @NotNull
    private final File binary;
    private final int scenario;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public Task1FuelBurningController(@NotNull File binary, int scenario) {
        this.binary = binary;
        this.scenario = scenario;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public void run() throws IOException {
        InputLoggingVmWrapper vm = new InputLoggingVmWrapper(new DirectVm(binary));

        // First config step
        Map<Integer, Double> outputs = vm.step(new InputBuilder(16000, scenario).build());

        assert vm.getStepIndex() == 1;

        //
        // Calc first Hohmann speed
        //

        // Get three positions for our speed
        Vector s0 = getPosition(outputs);
        Vector s1 = getPosition(vm.step());

        // Our radius around the earth
        double r1 = radius(s1);

        // Our target radius
        double r2 = outputs.get(4);

        Pair<Vector, Vector> positions = Pair.newPair(s0, s1);

        positions = new HohmannTransfer(r2).perform(vm, positions);

        double remFuel = vm.step().get(1);
        System.err.println("remFuel = " + remFuel);

        // Reserve
        remFuel -= 0;
        double r3 = R_EARTH;
        double fuel = remFuel + 1;
        while (fuel > remFuel) {
            r3 += 1;
            fuel = new HohmannTransfer(r3).getFuelConsumption(positions) * 2;
        }

        if (Math.abs(fuel - remFuel) > 100) {
            r3 = R_EARTH;
            fuel = remFuel + 1;
            while (fuel > remFuel) {
                r3 += 1;
                fuel = new HohmannTransfer(r3).getFuelConsumption(positions) * 4;
            }

            positions = new HohmannTransfer(r3).perform(vm, positions);
            positions = new HohmannTransfer(r2).perform(vm, positions);
        }


        positions = new HohmannTransfer(r3).perform(vm, positions);
        new HohmannTransfer(r2).perform(vm, positions);

        while (outputs.get(0) == 0) {
            outputs = vm.step();
        }

        System.err.println("score = " + outputs.get(0));
        System.err.println("fuel  = " + outputs.get(1));

        System.out.print(vm.getInput());
    }


    @NotNull
    private Map<Integer, Double> buildDeltaVInput(@NotNull Vector dv) {
        return new InputBuilder(2, dv.getX()).add(3, dv.getY()).build();
    }

    @NotNull
    private Vector getPosition(@NotNull Map<Integer, Double> outputs) {
        return new Vector(outputs.get(2), outputs.get(3)).flip();
    }

    public static void main(String[] args) throws IOException {
        new Task1FuelBurningController(new File(args[0]), Integer.valueOf(args[1])).run();
    }
}