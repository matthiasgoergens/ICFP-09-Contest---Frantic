package de.hronopik.icfp2009.util;

import java.util.Map;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA. User: patrick Purpose:
 */
public class MathematicaConverter {

	public static double[][] fromMap(Map<Integer, Double> m){
		double result[][] = new double[m.size()][2];
		int i = 0;
		for(Integer key: m.keySet()){
			result[i][0] = key;
			result[i][1] = m.get(key);
		}
		return result;
	}

	public static Map<Integer, Double> toMap(int keys[], double values[]){
		HashMap<Integer, Double> result = new HashMap<Integer, Double>(keys.length);
		for(int i=0; i<keys.length; ++i){
			result.put(keys[i], values[i]);
		}
		return result;
	}

}
