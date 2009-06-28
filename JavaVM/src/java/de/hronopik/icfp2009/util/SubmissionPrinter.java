package de.hronopik.icfp2009.util;

import de.hronopik.icfp2009.io.SimulationFrame;
import de.hronopik.icfp2009.io.SimulationHeader;
import de.hronopik.icfp2009.io.SubmissionInputStream;
import org.jetbrains.annotations.NotNull;

import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class SubmissionPrinter {

    public static void main(String[] args) throws IOException {
        if (args.length != 1) {
            System.err.println("Expect one argument, the submission binary.");
            System.exit(1);
            return;
        }

        SubmissionInputStream in = new SubmissionInputStream(new FileInputStream(new File(args[0])));

        print(in.readHeader());
        try {
            while (true) {
                print(in.readFrame());
            }
        } catch (EOFException e) {
            //Ignore
        }
    }

    private static void print(@NotNull SimulationHeader header) {
        System.out.println("       Header       ");
        System.out.println("--------------------");
        System.out.printf("TeamID      | %6d\n", header.getTeamId());
        System.out.printf("ScenarioID  | %6d\n", header.getScenarioNumber());
        System.out.println("--------------------");
    }

    private static void print(@NotNull SimulationFrame frame) {
        System.out.printf("TimeStep    | %6d\n", frame.getTimeStep());
        System.out.printf("Count       | %6d\n", frame.getInputs().size());
        for (Map.Entry<Integer, Double> input : frame.getInputs().entrySet()) {
            System.out.printf("Addr        | %6d\n", input.getKey());
            System.out.printf("Value       | %f\n", input.getValue());
        }
        System.out.println("--------------------");
    }
}
