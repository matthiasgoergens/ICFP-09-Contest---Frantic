package de.hronopik.icfp2009.io;

/**
 * @author Alexander Kiel
 * @version $Id$
 */
public final class SimulationHeader {

    private final int teamId;

    private final int scenarioNumber;

    //---------------------------------------------------------------------------------------------
    // Constructor
    //---------------------------------------------------------------------------------------------

    SimulationHeader(int teamId, int scenarioNumber) {
        this.teamId = teamId;
        this.scenarioNumber = scenarioNumber;
    }

    //---------------------------------------------------------------------------------------------
    //
    //---------------------------------------------------------------------------------------------

    public int getTeamId() {
        return teamId;
    }

    public int getScenarioNumber() {
        return scenarioNumber;
    }
}