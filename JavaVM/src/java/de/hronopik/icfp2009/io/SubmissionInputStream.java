package de.hronopik.icfp2009.io;

import org.jetbrains.annotations.NotNull;

import java.io.EOFException;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import static java.lang.String.format;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class SubmissionInputStream extends FilterInputStream {

    private final static long MAGIC_NUMBER = 0xCAFEBABE;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public SubmissionInputStream(InputStream in) {
        super(in);
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------


    public final SimulationHeader readHeader() throws IOException {
        long magicNumber = readUnsignedInteger();
        if (magicNumber != MAGIC_NUMBER) {
            throw new IOException(format("Excpected magic number [%08X] but was [%08X].", MAGIC_NUMBER, magicNumber));
        }
        return new SimulationHeader((int) readUnsignedInteger(), (int) readUnsignedInteger());
    }


    public final SimulationFrame readFrame() throws IOException {
        int timeStep = (int) readUnsignedInteger();
        int count = (int) readUnsignedInteger();
        Map<Integer, Double> inputs = new LinkedHashMap<Integer, Double>();
        for (int i = 0; i < count; i++) {
            long portAddress = readUnsignedInteger();
            if (portAddress > 0x3FFF) {
                throw new IOException("Port address " + portAddress + " is out of range [0x3FFF, 0x0].");
            }
            double value = readValue();
            inputs.put((int) portAddress, value);
        }
        return new SimulationFrame(timeStep, inputs);
    }

    //---------------------------------------------------------------------------------------------
    // Helper Methods
    //---------------------------------------------------------------------------------------------

    private long readUnsignedInteger() throws IOException {
        long ch1 = in.read();
        int ch2 = in.read();
        int ch3 = in.read();
        int ch4 = in.read();

        if ((ch1 | ch2 | ch3 | ch4) < 0) {
            throw new EOFException();
        }

        return (ch4 << 24) + (ch3 << 16) + (ch2 << 8) + ch1;
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
}