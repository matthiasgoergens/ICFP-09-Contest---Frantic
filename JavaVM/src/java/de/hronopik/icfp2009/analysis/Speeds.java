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

                Vector s_t = new Vector(oldInputs.get(2), oldInputs.get(3));
                Vector s_tt = new Vector(inputs.get(2), inputs.get(3));
                Vector speed = Phys.speedT0(s_t, s_tt);

                System.out.println("r   = " + Phys.radius(s_t));
                System.out.println("v_x = " + speed.getX());
                System.out.println("v_y = " + speed.getY());

                oldInputs = inputs;
            }
        } catch (EOFException e) {
            // Ignore
        }
    }
}
