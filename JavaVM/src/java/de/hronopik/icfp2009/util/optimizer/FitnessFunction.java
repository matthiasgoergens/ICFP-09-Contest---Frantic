package de.hronopik.icfp2009.util.optimizer;

/**
 * Created by IntelliJ IDEA. User: patrick Purpose:
 */
public interface FitnessFunction {

	/**
	 * This returns the score for a specific parameter set.
	 * @param parms the n parameters of an n-dimensional problem.
	 * @return The potential, score, fitness... name it like you want.
	 *
	 */
	public double f(double parms[] );

	            
	/**
	 * To ensure that a minimum is found it is useful to have an initial guess
	 * for the minimizer.
	 * @return initial values for calling f(...).
	 */
	public double[] getInitialGuess();

	/**
	 * The dimension of the problem
	 * @return the dimension
	 */
	public int getDimension();

	/**
	 * This should return an initial stepsize for the problems parameters.
	 * When you are not sure, then smaller is better.
	 * @return list containing the stepsize for each parameter.
	 */
	double[] getInitialStepSize();

}
