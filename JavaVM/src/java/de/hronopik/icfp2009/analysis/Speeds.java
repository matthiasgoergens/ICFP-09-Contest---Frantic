package de.hronopik.icfp2009.analysis;

import de.hronopik.icfp2009.io.VmReader;
import de.hronopik.icfp2009.util.Vector;

import java.io.EOFException;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import static java.lang.Math.sqrt;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Speeds {

    static final double G = 6.67428E-11;
    static final double m_e = 6e24;

    public static void main(String[] args) throws IOException {
        if (args.length != 1) {
            System.err.println("Expect one argument, a vm output file.");
            System.exit(1);
            return;
        }

        VmReader reader = new VmReader(new FileReader(new File(args[0])));
        try {

            Map<Integer, Double> oldInputs = reader.readInputs();

            while (true) {
                Map<Integer, Double> inputs = reader.readInputs();

                Vector speed = speed(oldInputs, inputs);

                System.out.println("v_x = " + speed.getX());
                System.out.println("v_y = " + speed.getY());

                oldInputs = inputs;
            }
        } catch (EOFException e) {
            // Ignore
        }
    }

    static Vector speed(Map<Integer, Double> inputs_t, Map<Integer, Double> inputs_tt) {
        return speed(new Vector(inputs_t.get(2), inputs_t.get(3)), new Vector(inputs_tt.get(2), inputs_tt.get(3)));
    }

    static Vector speed(Vector s_t, Vector s_tt) {
        double r_square = s_t.getX() * s_t.getX() + s_t.getY() * s_t.getY();
        double g = -G * m_e / r_square / sqrt(r_square);
        double gt_x = g * s_t.getX();
        double gt_y = g * s_t.getY();
        return new Vector(s_tt.getX() - s_t.getX() - gt_x / 2, s_tt.getY() - s_t.getY() - gt_y / 2);
    }
}
