package de.hronopik.icfp2009.util;

import java.util.logging.Formatter;
import java.util.logging.LogRecord;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public class InstructionTraceFormatter extends Formatter {

    @Override
    public String format(LogRecord record) {
        StringBuilder sb = new StringBuilder();
        sb.append(record.getMessage());
        sb.append('\n');
        return sb.toString();
    }
}
