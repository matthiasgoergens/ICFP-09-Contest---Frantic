package de.hronopik.icfp2009.vm;

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

    @NotNull
    public static List<Frame> readFromFile(@NotNull File file) throws IOException {
        OrbitInputStream in = new OrbitInputStream(new FileInputStream(file));
        List<Frame> frames = new ArrayList<Frame>();
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
