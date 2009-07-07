package de.hronopik.icfp2009.io;

import org.jetbrains.annotations.NotNull;

import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class Frames {


    public static List<OrbitBinaryFrame> readFromFile(File file) throws IOException {
        OrbitBinaryInputStream in = new OrbitBinaryInputStream(new FileInputStream(file));
        List<OrbitBinaryFrame> frames = new ArrayList<OrbitBinaryFrame>();
        try {
            while (true) {
                frames.add(in.readFrame());
            }
        } catch (EOFException e) {
            in.close();
            return frames;
        }
    }
}
