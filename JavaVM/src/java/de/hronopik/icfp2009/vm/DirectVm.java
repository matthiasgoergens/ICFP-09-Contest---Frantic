package de.hronopik.icfp2009.vm;

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

    public DirectVm(@NotNull String filename) throws IOException {
        super(Frames.readFromFile(new File(filename)));
    }
}
