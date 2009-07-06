package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.model.ROM;
import de.hronopik.icfp2009.model.RAM;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface Memory extends ROM {

    double getValue(final int address);

    boolean isStatus();

    Memory setValue(final double value);

    Memory setStatus(boolean status);

    Memory copy();
}
