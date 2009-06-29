package de.hronopik.icfp2009.controller;

import de.hronopik.icfp2009.util.InputBuilder;
import de.hronopik.icfp2009.util.Phys;
import static de.hronopik.icfp2009.util.Phys.*;
import de.hronopik.icfp2009.util.Vector;
import de.hronopik.icfp2009.vm.DirectVm;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import static java.lang.Math.abs;
import static java.lang.Math.pow;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Task1Controller {

    @NotNull
    private final File binary;
    private final int scenario;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public Task1Controller(@NotNull File binary, int scenario) {
        this.binary = binary;
        this.scenario = scenario;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public void run() throws IOException {
        DirectVm vm = new DirectVm(binary);

        // First config step
        Map<Integer, Double> outputs = vm.step(new InputBuilder(16000, scenario).build());
        /*System.out.println("16000 " + scenario);
        System.out.println(".");*/

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
        int th = (int) Math.ceil(tho);
        double rh2 = Phys.hohmannTime1R2(r1, th);

        System.out.println("tho = " + tho);
        System.out.println("th = " + th);
        System.out.println("dr = " + abs(r2 - rh2));

        r2 = rh2;

        // The absolute value of the Hohmann speed
        double adv = hohmannSpeed1(r1, r2);

        // Our direction is
        Vector d = direction(s0, s1).normalize();//.plus(new Vector(-0.001291,0)).normalize();

        // The vectorial Hohmann speed is
        Vector dv = d.scale(adv);

        //
        // Perform the step with first Hohmann speed
        //

        int timeBeforeHohmann1 = vm.getStepIndex();
        assert timeBeforeHohmann1 == 2;

        // Perform the speed change in s1
        Vector s2 = getPosition(vm.step(buildDeltaVInput(dv)));
        /*System.out.println("2 " + dv.getX());
        System.out.println("3 " + dv.getY());
        System.out.println(".");*/

        //
        // Wait until Hohmann time ends
        //

        // Skip most of the Hohmann transfer time
        int timeBeforeHohmann2 = timeBeforeHohmann1 + th;
        while (vm.getStepIndex() < timeBeforeHohmann2 - 1) {
            outputs = vm.step();
            //System.out.println(".");
        }

        Vector sh21 = getPosition(outputs);
        outputs = vm.step();
        //System.out.println(".");
        Vector sh22 = getPosition(outputs);


        if (timeBeforeHohmann2 != vm.getStepIndex()) {
            throw new IllegalStateException();
        }

        r2 = radius(sh22);

        // The absolute value of the Hohmann speed 2
        double adv2 = hohmannSpeed2(r1, r2);

        // The vectorial Hohmann speed 2 is
        Vector dv2 = direction(sh21, sh22).normalize().scale(adv2);

        System.out.println("delta r   = " + (r2 - radius(sh22)));
        System.out.println("alpha = " + Phys.angle(sh22, dv2));

        // Perform the speed change
        assert vm.getStepIndex() == timeBeforeHohmann2;
        vm.step(buildDeltaVInput(dv2));
        /*System.out.println("2 " + dv2.getX());
        System.out.println("3 " + dv2.getY());
        System.out.println(".");*/

        /*while (vm.getStepIndex() < th + 1000) {
            outputs = vm.step();
        }*/

        double maxDeltaR = 0;
        while (vm.getStepIndex() < timeBeforeHohmann2 + Phys.circulationTime(r2) * 2) {
            outputs = vm.step();
            //System.out.println(".");
            //System.out.println("stepIndex = " + vm.getStepIndex());
            /*System.out.printf("%6d %10f, %10f\n", vm.getStepIndex() - timeBeforeHohmann2, (Phys.radius(getPosition(outputs)) - outputs.get(4)),
                    (Phys.radius(getPosition(outputs)) - r2));*/
            double deltaR = abs(Phys.radius(getPosition(outputs)) - r2);
            if (deltaR > maxDeltaR) {
                maxDeltaR = deltaR;
            }
        }

        System.out.println("maxDeltaR = " + maxDeltaR);
        System.out.println("Phys.circulationTime(r2) = " + Phys.circulationTime(r2));
    }

    private Vector direction(Vector s0, Vector s1) {
        Vector d0 = s1.minus(s0);
        Vector d1 = s1.turnClockwise();
        Vector d2 = s1.turnAntiClockwise();
        return d0.dot(d1) > d0.dot(d2) ? d1 : d2;
    }

    private boolean h1Approach(@NotNull Map<Integer, Double> oldOutputs, @NotNull Map<Integer, Double> outputs,
                               @NotNull Vector st, double r2) {
        Vector oldS = getPosition(oldOutputs);
        Vector s = getPosition(outputs);

        double oldRadius = radius(oldS);
        double radius = radius(s);

        double oldOrbitDistance = pow(r2 - oldRadius, 2);
        double orbitDistance = pow(r2 - radius, 2);

        double oldTargetDistance = distance(st, oldS);
        double targetDistance = distance(st, s);

        System.out.printf("%5f %5f %5f\n", targetDistance, oldRadius - radius, r2 - radius(s));

        //return orbitDistance < oldOrbitDistance;
        //return targetDistance < oldTargetDistance;
        return oldRadius < radius;
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
        new Task1Controller(new File(args[0]), Integer.valueOf(args[1])).run();
    }
}
