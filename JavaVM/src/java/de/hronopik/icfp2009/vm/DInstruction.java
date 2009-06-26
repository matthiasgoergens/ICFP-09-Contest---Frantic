package de.hronopik.icfp2009.vm;

import org.jetbrains.annotations.NotNull;

import java.util.Map;

/**
 * A D-Type instruction.
 *
 * @author Alexander Kiel
 * @version $Id$
 * @see "task-1.0.pdf 2.2 p.3"
 */
final class DInstruction extends Instruction {

    @NotNull
    private final DOp op;
    private final int r1;
    private final int r2;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    DInstruction(int address, @NotNull DOp op, int r1, int r2) {
        super(address);
        this.op = op;
        this.r1 = r1;
        this.r2 = r2;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    @NotNull
    public DOp getOp() {
        return op;
    }

    public int getR1() {
        return r1;
    }

    public int getR2() {
        return r2;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    boolean execute(boolean status, double[] values, @NotNull Map<Integer, Double> inputs,
                    @NotNull Map<Integer, Double> outputs) {
        switch(op) {
            case Add:
                values[address] = values[r1] + values[r2];                
        }
        return status;
    }

    //---------------------------------------------------------------------------------------------
    // Overridden Object Methods
    //---------------------------------------------------------------------------------------------

    @Override
    public String toString() {
        return op + "(" + r1 + ", " + r2 + ")";
    }
}

