package de.hronopik.icfp2009.util.NewtonOptimizer;

/**
 * Created by IntelliJ IDEA.
 * User: patrick
 * Purpose: It will try to find a minimum to a given problem.
 */
public class SimpleMinimizer {
	FitnessFunction problem;
	double min[];
	double dp[]; //The initial stepsize of the parameters
	int dim;


	public SimpleMinimizer(FitnessFunction problem) {
		this.problem = problem;
		dim = problem.getDimension();

	}

	double[] step(double oldValues[]){
		dp = problem.getInitialStepSize();
		double curF = problem.f(oldValues);
		for(int n=0; n<dim; ++n){
			// check whether we are running in the right direction.
			if(f(oldValues,dp[n],n)>curF) dp[n] = -dp[n];
			// double the stepsize until we are over the minimum.
			do{
			  double newF = f(oldValues,dp[n],n);
				double decrease = curF - newF;
				if(decrease <= 0.0d){
					break;
				}
				dp[n] *= 2.0d;
				curF = newF;
			}while(true);



		}
		return oldValues;

	}

	double f(double values[], double deltaValueN, int n){
		values[n] += deltaValueN;
		return(problem.f(values));
	}

}
