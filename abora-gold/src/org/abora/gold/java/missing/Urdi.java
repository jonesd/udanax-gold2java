/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

import org.abora.gold.collection.basic.Int32Array;
import org.abora.gold.collection.basic.UInt8Array;
import org.abora.gold.xpp.basic.Heaper;

public class Urdi extends Heaper {
	private String filename;
	private int lruCount;

	private Map space = new HashMap();
	private int totalSnarfs = 16;
	
	//TODO guess at size. Seems like it needs to be a multiple of 4 in size
	private static final int SNARF_SIZE = 16000;
	
	public Urdi(String filename, int lruCount) {
		super();
		this.filename = filename;
		this.lruCount = lruCount;
		
		for (int i = 0; i < totalSnarfs; i++) {
			UInt8Array snarfSpace = UInt8Array.make(SNARF_SIZE);
			space.put(new Integer(i), snarfSpace);
		}
	}

	public UrdiView makeWriteView() {
		//TODO something more interesting here...
		return new UrdiView(this, true);
	}

	public static Urdi urdi(String fname, int lruCount) {
		return new Urdi(fname, lruCount);
	}

	public int usableSnarfs() {
		//TODO rubbish - See SnarfInfoHandle
		return totalSnarfs;
	}

	public int getDataSizeOfSnarf(int i) {
		return SNARF_SIZE;
	}

	public int usableStages() {
		//TODO have no idea what this means...
		return 100;
	}

	public UrdiView makeReadView() {
		//TODO something more interesting here...
		return new UrdiView(this, false);
	}
	
	public void printOn(PrintWriter oo) {
		oo.print(getAboraClass().name());
		oo.print("(");
		oo.print(filename);
		oo.print(")");
	}

	protected UInt8Array getSpace(int snarfID) {
		return (UInt8Array)space.get(new Integer(snarfID));
	}

}
