package de.hronopik.icfp2009.io;

import de.hronopik.icfp2009.model.*;
import static de.hronopik.icfp2009.model.SOp.Cmpz;
import de.hronopik.icfp2009.util.Maybe;
import static de.hronopik.icfp2009.util.Maybe.just;
import static de.hronopik.icfp2009.util.Maybe.nothing;

import java.io.EOFException;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class OrbitBinaryInputStream extends FilterInputStream {

    private int frameAddress;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public OrbitBinaryInputStream(InputStream in) {
        super(in);
        frameAddress = 0;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------


    public final OrbitBinaryFrame readFrame() throws IOException {
        OrbitBinaryFrame result;
        if (frameAddress % 2 == 0) {
            result = readEvenFrame();
        } else {
            result = readOddFrame();
        }
        frameAddress++;
        return result;
    }

    /**
     * Reads the frame at even frame addresses.
     * <p/>
     * Reads first the value and than the instruction.
     *
     * @return the frame
     * @throws IOException if an I/O error occurs
     * @see "task-1.0.pdf 2.4 p.4"
     */

    public final OrbitBinaryFrame readEvenFrame() throws IOException {
        double value = readValue();
        Instruction instruction = readInstruction();
        return new OrbitBinaryFrame(instruction, value);
    }

    /**
     * Reads the frame at odd frame addresses.
     * <p/>
     * Reads first the instruction and than the value.
     *
     * @return the frame
     * @throws IOException if an I/O error occurs
     * @see "task-1.0.pdf 2.4 p.4"
     */

    public final OrbitBinaryFrame readOddFrame() throws IOException {
        Instruction instruction = readInstruction();
        double value = readValue();
        return new OrbitBinaryFrame(instruction, value);
    }


    private Instruction readInstruction() throws IOException {
        long ch1 = in.read();
        int ch2 = in.read();
        int ch3 = in.read();
        int ch4 = in.read();
        if ((ch1 | ch2 | ch3 | ch4) < 0) {
            throw new EOFException();
        }

        // The 32-bit unsigned instruction code value (have to be a long)
        long instructionCode = (ch4 << 24) + (ch3 << 16) + (ch2 << 8) + ch1;

        int opcode = (int) ((instructionCode >> 28) & 0xF);
        if (opcode != 0) {

            // We have a D-Type instruction here
            int r1 = (int) ((instructionCode >> 14) & 0x3FFF);
            int r2 = (int) (instructionCode & 0x3FFF);
            return new DInstruction(frameAddress, DOp.fromOpcode(opcode), r1, r2);

        } else {

            // We have a S-Type instruction here
            return createSInstruction(instructionCode);
        }
    }

    private double readValue() throws IOException {
        long ch1 = in.read();
        long ch2 = in.read();
        long ch3 = in.read();
        long ch4 = in.read();
        long ch5 = in.read();
        long ch6 = in.read();
        long ch7 = in.read();
        long ch8 = in.read();
        if ((ch1 | ch2 | ch3 | ch4 | ch5 | ch6 | ch7 | ch8) < 0) {
            throw new EOFException();
        }
        long longValue = (ch8 << 56) + (ch7 << 48) + (ch6 << 40) + (ch5 << 32) +
                (ch4 << 24) + (ch3 << 16) + (ch2 << 8) + ch1;
        return Double.longBitsToDouble(longValue);
    }

    private SInstruction createSInstruction(long instructionCode) {

        SOpI op = SOp.fromOpcode((int) ((instructionCode >> 24) & 0xF));

        Maybe<?> param;
        if (op == Cmpz) {
            param = just(CompParam.fromOpcode((int) ((instructionCode >> 21) & 0x7)));
        } else {
            param = nothing();
        }
        int r1 = (int) (instructionCode & 0x3FFF);
        return new SInstruction(frameAddress, op, param, r1);
    }
}
