package de.hronopik.icfp2009.util.optimizer;

/**
 * Created by IntelliJ IDEA. User: patrick Purpose:
 */
public class SimpleProblem implements FitnessFunction{

	int DIM = 2;


	public double f(double[] p) {
		double x = p[0] - 0.2;
		double y = p[1] + 0.3;
		return x*x+ y*y;
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


	public static void main(String[] args) {
		SimpleMinimizer minimizer = new SimpleMinimizer(new SimpleProblem());
		System.out.println("Minimum is: "+minimizer.minimize());
		System.out.println("At: (" + minimizer.getMin()[0]+","+minimizer.getMin()[1]+")\n");
		
	}

}