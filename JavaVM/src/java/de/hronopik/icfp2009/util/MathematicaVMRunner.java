package de.hronopik.icfp2009.util;

import de.hronopik.icfp2009.vm.DirectVm;
import de.hronopik.icfp2009.util.optimizer.FitnessFunction;
import de.hronopik.icfp2009.util.optimizer.SimpleMinimizer;

import java.io.IOException;
import java.util.HashMap;
import java.util.Collections;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA. User: patrick Purpose:
 */
public class MathematicaVMRunner implements FitnessFunction{

	double r2 = 4.2164e7d;

	public static DirectVm vm;
	String path;

	public MathematicaVMRunner(String pathToProblem) {
		path = pathToProblem;
		try {
			vm = new DirectVm(path);
		} catch (IOException e) {
			e.printStackTrace();
		}

		vm.createSnapshoot("start");


	}


	public Double[] runToTurn(double dvx, double dvy){
		int MAX_ITER = 100000;
		vm.reset("start");
		java.util.Map<Integer, Double> inputs = new HashMap<Integer, Double>();
//		inputs.put(16000, 1001d);
		inputs.put(2, dvx);
		inputs.put(3, dvy);
		ArrayList<Double> result = new ArrayList<Double>();


		double x1,y1,x2,y2;

		Map<Integer, Double> outputs = vm.step(inputs);
		x1 = outputs.get(2).maybe(Continuations.<Double>fail("output 2 not existent"));
		y1 = outputs.get(3).maybe(Continuations.<Double>fail("output 3 not existent"));
		result.add(x1);
		result.add(y1);

        vm.createSnapshoot("undo");
		for (int iter = 0; iter < MAX_ITER; ++iter) {
			outputs = vm.step(Collections.<Integer, Double>emptyMap());
			x2 = outputs.get(2).maybe(Continuations.<Double>fail("output 2 not existent"));
			y2 = outputs.get(3).maybe(Continuations.<Double>fail("output 3 not existent"));
			result.add(x2);
			result.add(y2);

			if((x1*x1+y1*y1)>(x2*x2+y2*y2)&&iter>500) break;
			x1 = x2;
			y1 = y2;
		}
		vm.reset("undo");
		return(result.toArray(new Double[result.size()]));
	}

	public double[] runToTurn2(double dvx, double dvy, double rGoal){
		long MAX_ITER = 1000000;
		vm.reset("start");
		java.util.Map<Integer, Double> inputs = new HashMap<Integer, Double>();
		inputs.put(16000, 1001d);
		inputs.put(2, dvx);
		inputs.put(3, dvy);


		double x1,y1,x2,y2;

		Map<Integer, Double> outputs = vm.step(inputs);
		x1 = outputs.get(2).maybe(Continuations.<Double>fail("output 2 not existent"));
		y1 = outputs.get(3).maybe(Continuations.<Double>fail("output 3 not existent"));
		System.out.println("POSITION "+ x1 + " " + y1);

		for (long iter = 0; iter < MAX_ITER; ++iter) {
			outputs = vm.step(Collections.<Integer, Double>emptyMap());
			x2 = outputs.get(2).maybe(Continuations.<Double>fail("output 2 not existent"));
			y2 = outputs.get(3).maybe(Continuations.<Double>fail("output 3 not existent"));

			double r1 = (x1*x1+y1*y1);
			double r2 = (x2*x2+y2*y2);
			if( r1>r2 && Math.abs(Math.sqrt(r1)-rGoal) < 6.357e6  ) break;
			x1 = x2;
			y1 = y2;
		}
//		vm.undo();
//		vm.createSnapshoot("TURN");
		return(new double[]{vm.getStepIndex(),Math.sqrt(x1*x1+y1*y1)});
	}

	public Double[] orbitError(double dvx, double dvy, double wantedR, int MAX_ITER){
		vm.reset("TURN");
		java.util.Map<Integer, Double> inputs = new HashMap<Integer, Double>();
		inputs.put(16000, 1001d);
		inputs.put(2, dvx);
		inputs.put(3, dvy);
		double x1,y1, resultDiff= 0.0d;
		ArrayList<Double> pos = new ArrayList<Double>();

		Map<Integer, Double> outputs = vm.step(inputs);

		for (long iter = 0; iter < MAX_ITER; ++iter) {
			x1 = outputs.get(2).maybe(Continuations.<Double>fail("output 2 not existent"));
			y1 = outputs.get(3).maybe(Continuations.<Double>fail("output 3 not existent"));
			pos.add(x1); pos.add(y1);
			double diff = Math.abs(wantedR - Math.sqrt(x1*x1+y1*y1));
			resultDiff += diff;
			outputs = vm.step(Collections.<Integer, Double>emptyMap());
		}
		pos.add(resultDiff);
		return (pos.toArray(new Double[pos.size()]));
	}

	/**
	 *
	 * @param dvy1 first boost
	 * @param dvy2 second boost
	 * @param stepMarker1 when to give the second boost
	 * @param MAX_ITER  max steps. if -1 then we stop if goal is reached.
	 * @return positions and at the end the score.
	 */
	public Double[] makeRun(double dvy1, double dvy2, int stepMarker1,  int MAX_ITER){
		try {
			vm = new DirectVm(path);
		} catch (IOException e) {
			e.printStackTrace();
		}
		ArrayList<Double> result = new ArrayList<Double>();
		java.util.Map<Integer, Double> inputs = new HashMap<Integer, Double>();
		inputs.put(16000, 1001d);
		inputs.put(2, 0.0d);
		inputs.put(3, dvy1);


		double x1,y1;

		Map<Integer, Double> outputs = vm.step(inputs);
		int step = vm.getStepIndex();
		x1 = outputs.get(2).maybe(Continuations.<Double>fail("output 2 not existent"));
		y1 = outputs.get(3).maybe(Continuations.<Double>fail("output 3 not existent"));
		result.add(x1);
		result.add(y1);

		for (long iter = step; iter < stepMarker1; ++iter) {
			outputs = vm.step(Collections.<Integer, Double>emptyMap());
			x1 = outputs.get(2).maybe(Continuations.<Double>fail("output 2 not existent"));
			y1 = outputs.get(3).maybe(Continuations.<Double>fail("output 3 not existent"));
			result.add(x1);
			result.add(y1);
		}

		inputs = new HashMap<Integer, Double>();
		inputs.put(2, 0.0d);
		inputs.put(3, dvy2);
		outputs = vm.step(inputs);

		boolean breakme = false;
	 	if(MAX_ITER==-1){
			 MAX_ITER = 1000000;
			 breakme = true;
		 }
		for(long iter = stepMarker1+1; iter<MAX_ITER; ++iter ){
			if(breakme && outputs.get(0).maybe(Continuations.<Double>fail("output 0 not existent"))>0) break; //If goal reached, break up
			x1 = outputs.get(2).maybe(Continuations.<Double>fail("output 2 not existent"));
			y1 = outputs.get(3).maybe(Continuations.<Double>fail("output 2 not existent"));
			result.add(x1);
			result.add(y1);
			outputs = vm.step(Collections.<Integer, Double>emptyMap());
		}
		result.add(outputs.get(0).maybe(Continuations.<Double>fail("output 0 not existent"))); //Store the score
		return(result.toArray(new Double[result.size()]));
	}


	public double f(double[] parms) {
		double val[] = runToTurn2(0.0d, parms[0],r2);
		double potential = Math.abs(r2-val[1])/r2;

		System.err.println(parms[0]+ " -> "+ " with "+ potential +"   Iter("+vm.getStepIndex()+")");
		return potential;
	}

	public double[] getInitialGuess() {
		return new double[]{-2466.4};
	}

	public int getDimension() {
		return 1;
	}

	public double[] getInitialStepSize() {
		return new double[]{0.01};  //To change body of implemented methods use File | Settings | File Templates.
	}

	public static void main(String[] args) {

		//TODO: start to believe that this is never going to work because the f()'s are far too slow.. shit.
		MathematicaVMRunner run = new MathematicaVMRunner("/home/patrick/icfp/task/bin1.obf");
		SimpleMinimizer minimizer = new SimpleMinimizer(run);
		minimizer.minimize();
		System.out.println(minimizer.getMin()[0]);
		

	}


}
