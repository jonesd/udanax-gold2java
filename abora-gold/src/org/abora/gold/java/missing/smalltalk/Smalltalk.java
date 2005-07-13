/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing.smalltalk;

import java.util.HashMap;
import java.util.Map;

import org.abora.gold.xcvr.Recipe;
import org.abora.gold.xpp.basic.Category;
import org.abora.gold.xpp.fluid.FluidVar;


public class Smalltalk {

	private static final Map map = new HashMap();
	
	public Smalltalk() {
		super();
	}

	public static Category at(Symbol className) {
		throw new UnsupportedOperationException();
	}

	public static Category at(String className) {
		return (Category)map.get(className);
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

	public static Recipe associationAt(String xpp_cuisine) {
		//TODO this must be wrong!
		return (Recipe)map.get(xpp_cuisine);
	}

}
