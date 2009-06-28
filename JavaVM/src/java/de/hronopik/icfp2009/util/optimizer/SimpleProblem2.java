package de.hronopik.icfp2009.util.optimizer;

/**
 * Created by IntelliJ IDEA. User: patrick Purpose:
 */
public class SimpleProblem2 implements FitnessFunction{

	int FUNCTION_CALLS = 0;
	int DIM = 2;


	public double f(double[] p) {
		FUNCTION_CALLS ++;
		double x = p[0] - 0.4432;
		double y = p[1] + 0.9987;
		return -Math.exp(-(x*x+ y*y));
	}

	public double[] getInitialGuess() {
		return new double[]{0.0d,0.0d};
	}

	public int getDimension() {
		return DIM;
	}

	public double[] getInitialStepSize() {
		return new double[]{0.1,0.1};
	}

	public int getFUNCTION_CALLS() {
		return FUNCTION_CALLS;
	}

	public static void main(String[] args) {
		SimpleProblem2 p = new SimpleProblem2();
		SimpleMinimizer minimizer = new SimpleMinimizer(p);
		System.out.println("Minimum is: "+minimizer.minimize());
		System.out.println("At: (" + minimizer.getMin()[0]+","+minimizer.getMin()[1]+")\n");
		System.out.println("We called " + p.getFUNCTION_CALLS()+" times the f function.");


	}

}