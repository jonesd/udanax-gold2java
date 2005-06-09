/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing.smalltalk;

import org.abora.gold.xpp.basic.Category;
import org.abora.gold.xpp.fluid.FluidVar;


public class Smalltalk {

	public Smalltalk() {
		super();
	}

	public static Category at(Symbol className) {
		throw new UnsupportedOperationException();
	}

	public static Category at(String className) {
		throw new UnsupportedOperationException();
	}

	public static void garbageCollect() {
		throw new UnsupportedOperationException();
	}

	public static void safeAtPut(String varName, FluidVar var) {
		throw new UnsupportedOperationException();
	}

	public static Category ifAbsent(String clName) {
		throw new UnsupportedOperationException();
	}

	public static Category ifAbsent(String varName, Object object) {
		throw new UnsupportedOperationException();
	}

}
