/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.collection.basic.UInt8Array;
import org.abora.gold.xpp.basic.Heaper;

public class SnarfHandle extends Heaper {

	public SnarfHandle() {
		super();
	}
	public int get32(int unknown) {
		throw new UnsupportedOperationException();
	}
	public UInt8Array getDataP() {
		throw new UnsupportedOperationException();
	}
	public int getSnarfID() {
		throw new UnsupportedOperationException();
	}
	public int getDataSize() {
		throw new UnsupportedOperationException();
	}
	public void makeWritable() {
		throw new UnsupportedOperationException();
	}
	public void moveBytes(int offsetToMove, int sweeper, int count) {
		throw new UnsupportedOperationException();
	}
	public void put32(int a, int b) {
		throw new UnsupportedOperationException();
	}
	public boolean isWritable() {
		throw new UnsupportedOperationException();
	}

}
