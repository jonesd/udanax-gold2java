/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.xpp.basic.Heaper;

public class IntegerVar extends Heaper {

	public IntegerVar() {
		super();
	}
	public IntegerVar(int var) {
		throw new UnsupportedOperationException();
	}

	public static IntegerVar zero() {
		throw new UnsupportedOperationException();
	}
	public int DOTasLong() {
		throw new UnsupportedOperationException();
	}

	public int integer() {
		throw new UnsupportedOperationException();
	}
	
	public IntegerVar min(IntegerVar var) {
		throw new UnsupportedOperationException();		
	}
	
	public int DOThashForEqual() {
		throw new UnsupportedOperationException();		
	}
}
