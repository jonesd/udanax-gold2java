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

import org.abora.gold.collection.basic.Int32Array;
import org.abora.gold.collection.basic.UInt8Array;
import org.abora.gold.java.exception.AboraRuntimeException;
import org.abora.gold.xpp.basic.Heaper;

public class SnarfHandle extends Heaper {

	private final int snarfId;
	private boolean writable = false;
	private UInt8Array array = null;
	
	public SnarfHandle(int snarfId, UInt8Array array) {
		super();
		
		this.snarfId = snarfId;
		this.array = array;
	}
	
	public int get32(int bytePosition) {
		return array.int32At(bytePosition);
	}
	
	public UInt8Array getDataP() {
		if (!isWritable()) {
			throw new AboraRuntimeException("Must be writeable");
		}
		return array;
	}
	
	public int getSnarfID() {
		return snarfId;
	}
	public int getDataSize() {
		return array.count();
	}
	public void makeWritable() {
		writable = true;
	}
	public void moveBytes(int offsetToMove, int sweeper, int count) {
		throw new UnsupportedOperationException();
	}
	public void put32(int bytePosition, int value) {
		array.storeInt32(bytePosition, value);
	}
	public boolean isWritable() {
		return writable;
	}

	public void printOn(PrintWriter oo) {
		oo.print(getAboraClass().name());
		oo.print("(");
		oo.print(snarfId);
		oo.print(")");
	}
}
