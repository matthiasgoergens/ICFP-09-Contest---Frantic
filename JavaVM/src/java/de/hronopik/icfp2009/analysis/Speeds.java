package de.hronopik.icfp2009.analysis;

import de.hronopik.icfp2009.io.VmReader;
import de.hronopik.icfp2009.util.Phys;
import de.hronopik.icfp2009.util.Vector;

import java.io.EOFException;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Speeds {


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
        return Phys.speed(new Vector(inputs_t.get(2), inputs_t.get(3)), new Vector(inputs_tt.get(2), inputs_tt.get(3)));
    }


}
