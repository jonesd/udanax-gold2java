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

public class Urdi extends Heaper {

	public Urdi() {
		super();
	}

	public UrdiView makeWriteView() {
		throw new UnsupportedOperationException();
	}

	public static Urdi urdi(String fname, int count) {
		throw new UnsupportedOperationException();
	}

	public byte usableSnarfs() {
		throw new UnsupportedOperationException();
	}

	public int getDataSizeOfSnarf(int i) {
		throw new UnsupportedOperationException();
	}

	public int usableStages() {
		throw new UnsupportedOperationException();
	}

}
