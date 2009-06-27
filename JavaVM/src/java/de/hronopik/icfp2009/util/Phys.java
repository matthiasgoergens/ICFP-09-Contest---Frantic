package de.hronopik.icfp2009.util;

import org.jetbrains.annotations.NotNull;

import static java.lang.Math.sqrt;

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

    /**
     * Returns the speed between the two given points.
     *
     * @param s_t  the first point
     * @param s_tt the second point
     * @return the speed between the two given points
     */
    @NotNull
    public static Vector speed(@NotNull Vector s_t, @NotNull Vector s_tt) {
        double r_square = s_t.getX() * s_t.getX() + s_t.getY() * s_t.getY();
        double g = -G * m_e / r_square / sqrt(r_square);
        double gt_x = g * s_t.getX();
        double gt_y = g * s_t.getY();
        return new Vector(s_tt.getX() - s_t.getX() - gt_x / 2, s_tt.getY() - s_t.getY() - gt_y / 2);
    }

    public static double hohmannSpeed1(double r1, double r2) {
        return sqrt(mu / r1) * (sqrt(2 * r2 / (r1 + r2)) - 1);
    }

    public static double hohmannSpeed2(double r1, double r2) {
        return sqrt(mu / r2) * (1 - sqrt(2 * r1 / (r1 + r2)));
    }
}
