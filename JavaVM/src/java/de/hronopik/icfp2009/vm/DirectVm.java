package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.io.Frames;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class DirectVm extends AbstractVm {

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public DirectVm(String filename) throws IOException {
        this(new File(filename));
    }

    public DirectVm(File binary) throws IOException {
        super(Frames.readFromFile(binary));
    }
}
