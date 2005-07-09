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

public class FHash extends Heaper {

	public FHash() {
		super();
	}
	public static int fastHashUInt32(int i) {
		//TODO do something better here!
		return Math.abs(i);
	}
	
	public static int fastHash(int i) {
		//TODO do something better here!
		return Math.abs(i);
	}

	public static int fastHash(double d) {
		//TODO do something better here!
		return (int)Math.abs(d);
	}

	public static int fastHash(String s) {
		throw new UnsupportedOperationException();
	}
	public static int fastHashString(String prob) {
		throw new UnsupportedOperationException();
	}
}
