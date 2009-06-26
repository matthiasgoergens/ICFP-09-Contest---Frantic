package de.hronopik.icfp2009.vm;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public enum CompParam implements Parameter {

    // Do not change the order of the operations!

    LTZ {
        boolean isCompZero(double value) {
            return value < 0;
        }
    },

    LEZ {
        boolean isCompZero(double value) {
            return value <= 0;
        }
    },

    EQZ {
        boolean isCompZero(double value) {
            return value == 0;
        }
    },

    GEZ {
        boolean isCompZero(double value) {
            return value >= 0;
        }
    },

    GTZ {
        boolean isCompZero(double value) {
            return value > 0;
        }
    };

    abstract boolean isCompZero(double value);

    /**
     * Returns the parameter according to the given opcode.
     *
     * @param opcode the opcode of the parameter to return
     * @return the parameter
     */
    public static CompParam fromOpcode(int opcode) {
        return values()[opcode];
    }
}
