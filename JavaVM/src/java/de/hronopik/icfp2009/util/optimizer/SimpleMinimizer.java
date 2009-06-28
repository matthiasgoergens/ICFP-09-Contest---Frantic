package de.hronopik.icfp2009.util.optimizer;

/**
 * Created by IntelliJ IDEA.
 * User: patrick
 * Purpose: It will try to find a minimum to a given problem.
 */
public class SimpleMinimizer {
	static final int MAX_ITER = 500;
	static final double EPS = 10e-11d;
	static final double TOLERANCE = 4*EPS;
	FitnessFunction problem;
	double min[];
	int dim;


	public SimpleMinimizer(FitnessFunction problem) {
		this.problem = problem;
		dim = problem.getDimension();

	}

	public double minimize(){
		double parms1[] = problem.getInitialGuess(), parms2[];
		double err1 = problem.f(parms1), err2;

		int iter = 0;
		do{
			parms2 = step(parms1);
			err2 = problem.f(parms2);
			double errDistance = Math.abs(err1 - err2);
			if(errDistance <TOLERANCE) break;
			if(err2 > err1){
				//we are not getting better
				err2 = err1;
				parms2 = parms1;
				break;
			}
			parms1 = parms2;
			err1 = err2;
		}while(iter<MAX_ITER);
		min = parms2;
		return err2;
	}

	double[] step(double inputParams[]){
		int iter = 0;

		double params[] = new double[dim];
		System.arraycopy(inputParams,0,params,0,dim);
		double dp[] = problem.getInitialStepSize();
		double curF = problem.f(params);
		for(int n=0; n<dim; ++n){
			// check whether we are running in the right direction.
			if(f(params,dp[n],n)>curF) dp[n] = -dp[n];
			// double the stepsize until we are over the minimum.
			do{
			  double newF = f(params,dp[n],n);
				double decrease = curF - newF;
				if(decrease <= 0.0d){
					break;
				}
				dp[n] *= 2.0d;
				curF = newF;
				++iter;
			}while(iter<MAX_ITER);

			dp[n] *= 0.5;
			double midPosition = params[n] + dp[n];
			do{
				dp[n] *= 0.5;
				double leftPosition = midPosition - dp[n];
				double rightPosition = midPosition + dp[n];
				double valueAtLefSide = f(params,leftPosition,n);
				double valueAtRightSide = f(params, rightPosition,n);
				if(Math.abs(valueAtLefSide-valueAtRightSide)>TOLERANCE){
					midPosition = valueAtLefSide<valueAtRightSide?leftPosition:rightPosition;
				}
				//if the values are not changing significantely, we may be found the minimum.
				if(Math.abs(leftPosition-rightPosition)<=EPS) break;
				++iter;
				}while(iter<MAX_ITER);

			//our new starting point is then
			params[n] += midPosition;
			curF = problem.f(params);
			}
		//now we minimized dimension after dimension and stored the result in oldvalues.
		return params;

	}

	double f(double values[], double deltaValueN, int n){
		double newVals[] = new double[values.length];
		System.arraycopy(values,0,newVals,0,values.length);
		newVals[n] += deltaValueN;
		return(problem.f(newVals));
	}

	public double[] getMin() {
		return min;
	}
}
