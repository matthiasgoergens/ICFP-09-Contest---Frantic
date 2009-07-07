package de.hronopik.icfp2009.model;

/**
 * @author Alexander Kiel
* @version $Id$
*/
public class StatusRegister {

    private boolean status = false;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    public StatusRegister() {
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public boolean isStatus() {
        return status;
    }

    public void setStatus(boolean status) {
        this.status = status;
    }
}
