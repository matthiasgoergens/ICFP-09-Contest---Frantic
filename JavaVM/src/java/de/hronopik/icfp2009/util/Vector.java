package de.hronopik.icfp2009.util;

import org.jetbrains.annotations.NotNull;

import static java.lang.Math.sqrt;

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

    //---------------------------------------------------------------------------------------------
    // Operations
    //---------------------------------------------------------------------------------------------


    public Vector plus(Vector v) {
        return new Vector(x + v.x, y + v.y);
    }


    public Vector minus(Vector v) {
        return new Vector(x - v.x, y - v.y);
    }

    public double dot(Vector v) {
        return x * v.x + y * v.y;
    }


    public Vector scale(double a) {
        return new Vector(x * a, y * a);
    }


    public Vector normalize() {
        return scale(1 / length());
    }

    public double length() {
        return sqrt(x * x + y * y);
    }


    public Vector flip() {
        return new Vector(-x, -y);
    }


    public Vector turnClockwise() {
        //noinspection SuspiciousNameCombination
        return new Vector(y, -x);
    }


    public Vector turnAntiClockwise() {
        //noinspection SuspiciousNameCombination
        return new Vector(-y, x);
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }
}