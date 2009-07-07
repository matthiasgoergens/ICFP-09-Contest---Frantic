package de.hronopik.icfp2009.vm;

import de.hronopik.icfp2009.model.InputPorts;

/**
 * @author Alexander Kiel
* @version $Id$
*/
public class EmptyInputPorts implements InputPorts {

    public final static InputPorts EMPTY_INPUT = new EmptyInputPorts();

    public double getValue(int address) {
        return 0;
    }
}
