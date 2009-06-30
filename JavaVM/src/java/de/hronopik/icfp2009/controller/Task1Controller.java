package de.hronopik.icfp2009.controller;

import de.hronopik.icfp2009.util.InputBuilder;
import de.hronopik.icfp2009.util.Phys;
import static de.hronopik.icfp2009.util.Phys.*;
import de.hronopik.icfp2009.util.Vector;
import de.hronopik.icfp2009.vm.DirectVm;
import de.hronopik.icfp2009.vm.InputLoggingVmWrapper;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import static java.lang.Math.abs;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Task1Controller {


    private final File binary;
    private final int scenario;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public Task1Controller(File binary, int scenario) {
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

        // The time we need for the Hohmann transfer
        double tho = hohmannTime1(r1, r2);
        int th = (int) Math.round(tho);
        double rh2 = Phys.hohmannTime1R2(r1, th);

        System.err.println("tho = " + tho);
        System.err.println("th = " + th);
        System.err.println("dr = " + abs(r2 - rh2));

        r2 = rh2;

        // The absolute value of the Hohmann speed
        double adv = hohmannSpeed1(r1, r2);

        // Our direction is
        Vector d = direction(s0, s1).normalize();

        // The vectorial Hohmann speed is
        Vector dv = d.scale(adv);

        //
        // Perform the step with first Hohmann speed
        //

        int timeBeforeHohmann1 = vm.getStepIndex();
        assert timeBeforeHohmann1 == 2;

        // Perform the speed change in s1
        vm.step(buildDeltaVInput(dv));

        //
        // Wait until Hohmann time ends
        //

        // Skip most of the Hohmann transfer time
        int timeBeforeHohmann2 = timeBeforeHohmann1 + th;
        while (vm.getStepIndex() < timeBeforeHohmann2 - 1) {
            outputs = vm.step();
        }

        Vector sh21 = getPosition(outputs);
        outputs = vm.step();
        Vector sh22 = getPosition(outputs);


        if (timeBeforeHohmann2 != vm.getStepIndex()) {
            throw new IllegalStateException();
        }

        r2 = radius(sh22);

        // The absolute value of the Hohmann speed 2
        double adv2 = hohmannSpeed2(r1, r2);

        // The vectorial Hohmann speed 2 is
        Vector dv2 = direction(sh21, sh22).normalize().scale(adv2);

        System.err.println("delta r   = " + (r2 - radius(sh22)));

        // Perform the speed change
        assert vm.getStepIndex() == timeBeforeHohmann2;
        vm.step(buildDeltaVInput(dv2));

        while (outputs.get(0) == 0) {
            outputs = vm.step();
        }

        System.err.println("score = " + outputs.get(0));
        System.err.println("fuel  = " + outputs.get(1));

        /*double maxDeltaR = 0;
        while (vm.getStepIndex() < timeBeforeHohmann2 + Phys.circulationTime(r2) * 2) {
            outputs = vm.step();
            //System.err.println(".");
            //System.err.println("stepIndex = " + vm.getStepIndex());
            *//*System.out.printf("%6d %10f, %10f\n", vm.getStepIndex() - timeBeforeHohmann2, (Phys.radius(getPosition(outputs)) - outputs.get(4)),
                    (Phys.radius(getPosition(outputs)) - r2));*//*
            double deltaR = abs(Phys.radius(getPosition(outputs)) - r2);
            if (deltaR > maxDeltaR) {
                maxDeltaR = deltaR;
            }
        }

        System.err.println("maxDeltaR = " + maxDeltaR);
        System.err.println("Phys.circulationTime(r2) = " + Phys.circulationTime(r2));*/

        System.out.print(vm.getInput());
    }

    private Vector direction(Vector s0, Vector s1) {
        Vector d0 = s1.minus(s0);
        Vector d1 = s1.turnClockwise();
        Vector d2 = s1.turnAntiClockwise();
        return d0.dot(d1) > d0.dot(d2) ? d1 : d2;
    }


    private Map<Integer, Double> buildDeltaVInput(Vector dv) {
        return new InputBuilder(2, dv.getX()).add(3, dv.getY()).build();
    }


    private Vector getPosition(Map<Integer, Double> outputs) {
        return new Vector(outputs.get(2), outputs.get(3)).flip();
    }

    public static void main(String[] args) throws IOException {
        new Task1Controller(new File(args[0]), Integer.valueOf(args[1])).run();
    }
}
