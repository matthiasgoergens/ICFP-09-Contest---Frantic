package de.hronopik.icfp2009.controller;

import de.hronopik.icfp2009.util.InputBuilder;
import de.hronopik.icfp2009.util.Pair;
import static de.hronopik.icfp2009.util.Pair.newPair;
import static de.hronopik.icfp2009.util.Phys.*;
import de.hronopik.icfp2009.util.Vector;
import de.hronopik.icfp2009.vm.Vm;
import org.jetbrains.annotations.NotNull;

import static java.lang.Math.abs;
import java.util.Map;

/**
 * Performs one Hohmann transfer from r1 to r2.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
public class HohmannTransfer {

    private final double r2;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public HohmannTransfer(double r2) {
        this.r2 = r2;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public double getFuelConsumption(@NotNull Pair<Vector, Vector> positions) {
        Vector s1 = positions.getB();

        // Our radius around the earth
        double r1 = radius(s1);

        // The time we need for the Hohmann transfer
        HohmannCorrection hohmannCorrection = new HohmannCorrection(r1, r2);
        double r2c = hohmannCorrection.getR();
        int th = hohmannCorrection.getT();

        // The absolute value of the Hohmann speed
        double adv = hohmannSpeed1(r1, r2c);
        double adv2 = hohmannSpeed2(r1, r2c);

        return abs(adv) + abs(adv2);
    }

    public Pair<Vector, Vector> perform(@NotNull Vm vm, @NotNull Pair<Vector, Vector> positions) {
        Vector s0 = positions.getA();
        Vector s1 = positions.getB();

        // Our radius around the earth
        double r1 = radius(s1);

        // The time we need for the Hohmann transfer
        HohmannCorrection hohmannCorrection = new HohmannCorrection(r1, r2);
        double r2c = hohmannCorrection.getR();
        int th = hohmannCorrection.getT();

        // The absolute value of the Hohmann speed
        double adv = hohmannSpeed1(r1, r2c);

        // Our direction is
        Vector d = direction(s0, s1).normalize();

        // The vectorial Hohmann speed is
        Vector dv = d.scale(adv);

        //
        // Perform the step with first Hohmann speed
        //

        int timeBeforeHohmann1 = vm.getStepIndex();

        // Perform the speed change in s1
        vm.step(buildDeltaVInput(dv));

        //
        // Wait until Hohmann time ends
        //

        // Skip most of the Hohmann transfer time
        int timeBeforeHohmann2 = timeBeforeHohmann1 + th;
        while (vm.getStepIndex() < timeBeforeHohmann2 - 2) {
            vm.step();
        }

        Vector s2 = getPosition(vm.step());
        Vector s3 = getPosition(vm.step());

        if (timeBeforeHohmann2 != vm.getStepIndex()) {
            throw new IllegalStateException();
        }

        // The absolute value of the Hohmann speed 2
        double adv2 = hohmannSpeed2(r1, radius(s3));

        // The vectorial Hohmann speed 2 is
        Vector dv2 = direction(s2, s3).normalize().scale(adv2);

        // Perform the speed change
        assert vm.getStepIndex() == timeBeforeHohmann2;
        Vector s4 = getPosition(vm.step(buildDeltaVInput(dv2)));

        return newPair(s3, s4);
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    private Vector direction(Vector s0, Vector s1) {
        Vector d0 = s1.minus(s0);
        Vector d1 = s1.turnClockwise();
        Vector d2 = s1.turnAntiClockwise();
        return d0.dot(d1) > d0.dot(d2) ? d1 : d2;
    }

    @NotNull
    private Map<Integer, Double> buildDeltaVInput(@NotNull Vector dv) {
        return new InputBuilder(2, dv.getX()).add(3, dv.getY()).build();
    }

    @NotNull
    private Vector getPosition(@NotNull Map<Integer, Double> outputs) {
        return new Vector(outputs.get(2), outputs.get(3)).flip();
    }
}
