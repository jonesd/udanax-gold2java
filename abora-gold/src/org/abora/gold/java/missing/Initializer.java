/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.java.missing.smalltalk.Array;

public class Initializer {

	public Initializer() {
		super();
	}

	public static void enterDoMain() {
		throw new UnsupportedOperationException();
	}

	public static void exitDoMain() {
		throw new UnsupportedOperationException();
	}

	//TODO Array argv shouldn't it be String[]
	public static void enterDoMain(int argc, Array argv) {
		throw new UnsupportedOperationException();
	}

}
