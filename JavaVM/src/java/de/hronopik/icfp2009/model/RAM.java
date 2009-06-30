package de.hronopik.icfp2009.model;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface RAM extends ROM {

    void setValue(int address, double value);
}
