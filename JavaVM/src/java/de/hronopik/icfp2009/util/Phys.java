package de.hronopik.icfp2009.util;

import org.jetbrains.annotations.NotNull;

import static java.lang.Math.*;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Phys {

    public static final double G = 6.67428E-11;
    public static final double m_e = 6e24;

    public static final double mu = G * m_e;

    public static double radius(@NotNull Vector s_t) {
        return sqrt(s_t.getX() * s_t.getX() + s_t.getY() * s_t.getY());
    }

    public static double distance(@NotNull Vector s_1, @NotNull Vector s_2) {
        double x = s_1.getX() - s_2.getX();
        double y = s_1.getY() - s_2.getY();
        return sqrt(x * x + y * y);
    }

    public static double angle(@NotNull Vector s_1, @NotNull Vector s_2) {
        return acos(s_1.dot(s_2) / s_1.length() / s_2.length()) * 180 / PI;
    }

    /**
     * Returns the speed at the point {@code s_t0}.
     *
     * @param s_t0 the first point
     * @param s_t1 the second point
     * @return the speed at the point {@code s_t0}
     */
    @NotNull
    public static Vector speedT0(@NotNull Vector s_t0, @NotNull Vector s_t1) {
        Vector g_t0 = gravitation(s_t0);
        return s_t1.minus(s_t0).minus(g_t0.scale(0.5));
    }

    @NotNull
    public static Vector speed(@NotNull Vector s0, @NotNull Vector s1, @NotNull Vector s2) {
        Vector g0 = gravitation(s0);
        Vector g1 = gravitation(s1);
        return s2.minus(s1).minus(g1).minus(g0).scale(2);
    }

    @NotNull
    public static Vector speedPatrick(@NotNull Vector s0, @NotNull Vector s1, @NotNull Vector s2) {
        Vector v0 = speedT0(s0, s1);
        Vector v1 = speedT0(s1, s2);
        return v0.plus(v1).scale(0.5);
    }

    @NotNull
    public static Vector nextPoint(@NotNull Vector s, @NotNull Vector v, @NotNull Vector dv) {
        return s.plus(v).plus(gravitation(s).plus(dv).scale(0.5));
    }

    public static double hohmannSpeed1(double r1, double r2) {
        return sqrt(mu / r1) * (sqrt(2 * r2 / (r1 + r2)) - 1);
    }

    public static double hohmannSpeed2(double r1, double r2) {
        return sqrt(mu / r2) * (1 - sqrt(2 * r1 / (r1 + r2)));
    }

    public static double hohmannTime1(double r1, double r2) {
        return PI * sqrt(pow(r1 + r2, 3) / (8 * mu));
    }

    public static double hohmannTime1R2(double r1, int th) {
        return pow(8 * mu, 1d / 3) * pow(th / PI, 2d / 3) - r1;
    }

    /**
     * Returns the time a orbiter needs for one complete circulation at the given radius.
     *
     * @param r the radius of the circular orbit
     * @return the time a orbiter needs for one complete circulation
     */
    public static double circulationTime(double r) {
        return sqrt(4 * PI * PI / mu) * pow(r, 3d / 2);
    }

    public static double orbitSpeed(double r) {
        return sqrt(2 * mu / r);
    }

    public static double gravitation(double r) {
        return mu / (r * r);
    }

    @NotNull
    public static Vector gravitation(@NotNull Vector s) {
        return s.normalize().flip().scale(gravitation(radius(s)));
    }
}
