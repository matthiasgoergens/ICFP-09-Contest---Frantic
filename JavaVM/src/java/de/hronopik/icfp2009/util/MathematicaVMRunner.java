package de.hronopik.icfp2009.util;

import de.hronopik.icfp2009.vm.DirectVm;

import java.io.IOException;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA. User: patrick Purpose:
 */
public class MathematicaVMRunner {
	DirectVm vm, vmSave;
	String path;

	public MathematicaVMRunner(String pathToProblem) {
		path = pathToProblem;


	}


	public Double[] runToTurn(double dvx, double dvy){
		int MAX_ITER = 100000;
		vm.reset("startme");
		Map<Integer, Double> inputs = new HashMap<Integer, Double>();
//		inputs.put(16000, 1001d);
		inputs.put(2, dvx);
		inputs.put(3, dvy);
		ArrayList<Double> result = new ArrayList<Double>();


		double x1,y1,x2,y2;

		Map<Integer, Double> outputs = vm.step(inputs);
		x1 = outputs.get(2);
		y1 = outputs.get(3);
		result.add(x1);
		result.add(y1);

		for (int iter = 0; iter < MAX_ITER; ++iter) {
			outputs = vm.step(Collections.<Integer, Double>emptyMap());
			x2 = outputs.get(2);
			y2 = outputs.get(3);
			result.add(x2);
			result.add(y2);

			if((x1*x1+y1*y1)>(x2*x2+y2*y2)&&iter>500) break;
			x1 = x2;
			y1 = y2;
		}
		vm.undo();
		vm.createSnapshoot("TURN");
		return(result.toArray(new Double[result.size()]));
	}

	public double[] runToTurn2(double dvx, double dvy, double rGoal){
		long MAX_ITER = 1000000;
		try {
			vm = new DirectVm(path);
		} catch (IOException e) {
			e.printStackTrace();
		}
		Map<Integer, Double> inputs = new HashMap<Integer, Double>();
		inputs.put(16000, 1001d);
		inputs.put(2, dvx);
		inputs.put(3, dvy);


		double x1,y1,x2,y2;

		Map<Integer, Double> outputs = vm.step(inputs);
		x1 = outputs.get(2);
		y1 = outputs.get(3);

		for (long iter = 0; iter < MAX_ITER; ++iter) {
			outputs = vm.step(Collections.<Integer, Double>emptyMap());
			x2 = outputs.get(2);
			y2 = outputs.get(3);

			double r1 = (x1*x1+y1*y1);
			double r2 = (x2*x2+y2*y2);
			if( r1>r2 && Math.abs(Math.sqrt(r1)-rGoal) < 6.357e6  ) break;
			x1 = x2;
			y1 = y2;
		}
		vm.undo();
		vm.createSnapshoot("TURN");
		return(new double[]{vm.getStepIndex(),Math.sqrt(x1*x1+y1*y1)});
	}

	public Double[] orbitError(double dvx, double dvy, double wantedR, int MAX_ITER){
		vm.reset("TURN");
		Map<Integer, Double> inputs = new HashMap<Integer, Double>();
		inputs.put(16000, 1001d);
		inputs.put(2, dvx);
		inputs.put(3, dvy);
		double x1,y1, resultDiff= 0.0d;
		ArrayList<Double> pos = new ArrayList<Double>();

		Map<Integer, Double> outputs = vm.step(inputs);

		for (long iter = 0; iter < MAX_ITER; ++iter) {
			x1 = outputs.get(2);
			y1 = outputs.get(3);
			pos.add(x1); pos.add(y1);
			double diff = Math.abs(wantedR - Math.sqrt(x1*x1+y1*y1));
			resultDiff += diff;
			outputs = vm.step(Collections.<Integer, Double>emptyMap());
		}
		pos.add(resultDiff);
		return (pos.toArray(new Double[pos.size()]));
	}

	public Double[] makeRun(double dvy1, double dvy2, int stepMarker1,  int MAX_ITER){
		try {
			vm = new DirectVm(path);
		} catch (IOException e) {
			e.printStackTrace();
		}
		ArrayList<Double> result = new ArrayList<Double>();
		Map<Integer, Double> inputs = new HashMap<Integer, Double>();
		inputs.put(16000, 1001d);
		inputs.put(2, 0.0d);
		inputs.put(3, dvy1);


		double x1,y1;

		Map<Integer, Double> outputs = vm.step(inputs);
		int step = vm.getStepIndex();
		x1 = outputs.get(2);
		y1 = outputs.get(3);
		result.add(x1);
		result.add(y1);

		for (long iter = step; iter < stepMarker1; ++iter) {
			outputs = vm.step(Collections.<Integer, Double>emptyMap());
			x1 = outputs.get(2);
			y1 = outputs.get(3);
			result.add(x1);
			result.add(y1);
		}

		inputs = new HashMap<Integer, Double>();
		inputs.put(2, 0.0d);
		inputs.put(3, dvy2);
		outputs = vm.step(inputs);

		

		for(long iter = stepMarker1+1; iter<MAX_ITER; ++iter ){
//			if(outputs.get(0)>0) break; //If goal reached, break up
			x1 = outputs.get(2);
			y1 = outputs.get(3);
			result.add(x1);
			result.add(y1);
			outputs = vm.step(Collections.<Integer, Double>emptyMap());
		}
		result.add(outputs.get(0)); //Store the score
		return(result.toArray(new Double[result.size()]));
	}


	public static void main(String[] args) {
		MathematicaVMRunner run = new MathematicaVMRunner("/home/patrick/icfp/task/bin1.obf");
		Double res1[] = run.runToTurn(0,-2466);
		Double res2[] = run.runToTurn(0,-2466);
		System.out.println("Laengen: "+res1.length +" und " +res2.length);

	}


}
