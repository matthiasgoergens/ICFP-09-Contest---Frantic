package de.hronopik.icfp2009.model;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public interface OutputPorts {

    public interface Assignment {

        int getAddress();

        double getValue();
    }

    void setValue(int address, double value);
}
