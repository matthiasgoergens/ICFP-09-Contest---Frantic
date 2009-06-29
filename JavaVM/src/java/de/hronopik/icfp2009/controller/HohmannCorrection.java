package de.hronopik.icfp2009.controller;

import de.hronopik.icfp2009.util.Phys;
import static de.hronopik.icfp2009.util.Phys.hohmannTime1;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class HohmannCorrection {

    private final double r;
    private final int t;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public HohmannCorrection(double r1, double r2) {
        double to = hohmannTime1(r1, r2);
        t = (int) Math.round(to);
        r = Phys.hohmannTime1R2(r1, t);
        /*System.err.println("to = " + to);
        System.err.println("t  = " + t);
        System.err.println("dr = " + Math.abs(r - r2));*/
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public double getR() {
        return r;
    }

    public int getT() {
        return t;
    }
}
