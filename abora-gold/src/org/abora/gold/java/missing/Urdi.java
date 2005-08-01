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
	private String filename;
	private int lruCount;

	public Urdi() {
		super();
	}
	
	public Urdi(String filename, int lruCount) {
		super();
		this.filename = filename;
		this.lruCount = lruCount;
	}

	public UrdiView makeWriteView() {
		//TODO something more interesting here...
		return new UrdiView(this);
	}

	public static Urdi urdi(String fname, int lruCount) {
		return new Urdi(fname, lruCount);
	}

	public byte usableSnarfs() {
		//TODO rubbish - See SnarfInfoHandle
		return 16;
	}

	public int getDataSizeOfSnarf(int i) {
		throw new UnsupportedOperationException();
	}

	public int usableStages() {
		throw new UnsupportedOperationException();
	}

	public UrdiView makeReadView() {
		//TODO something more interesting here...
		return new UrdiView(this);
	}

}
