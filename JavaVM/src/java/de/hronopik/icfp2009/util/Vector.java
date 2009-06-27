package de.hronopik.icfp2009.util;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public final class Vector {

    private final double x;
    private final double y;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public Vector(double x, double y) {
        this.x = x;
        this.y = y;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }
}