package de.hronopik.icfp2009.model;

/**
 * A read-only memory.
 *
 * @author Alexander Kiel
 * @version $Id$
 */
public interface ROM {

    double getValue(int address);

     boolean isStatus();
}
