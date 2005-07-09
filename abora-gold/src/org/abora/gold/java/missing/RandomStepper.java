/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import java.util.Random;

import org.abora.gold.xpp.basic.Heaper;

public class RandomStepper extends Heaper {
	private final Random random = new Random();

	private int value;
	
	public RandomStepper() {
		super();
		step();
	}

	public static RandomStepper make(int i, int j, int k) {
		//TODO what about the params?
		return new RandomStepper();
	}

	public static RandomStepper make(int i) {
		//TODO what about the params?
		return new RandomStepper();
	}

	public int value() {
		return value;
	}
	
	public void step() {
		value = random.nextInt();
	}
}
