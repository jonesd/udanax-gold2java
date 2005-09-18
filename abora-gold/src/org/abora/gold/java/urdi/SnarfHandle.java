/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.urdi;

import java.io.PrintWriter;

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
		//TODO should this be aliased on our internal data, or a clone?
//		mustBeWritable();
		return array;
	}

	private void mustBeWritable() {
		if (!isWritable()) {
			throw new AboraRuntimeException("Must be writeable");
		}
	}
	
	public int getSnarfID() {
		return snarfId;
	}
	
	public int getDataSize() {
		return array.count();
	}
	
	public void makeWritable() {
		if (writable) {
			return;
		}
		array = (UInt8Array)array.copy();
		writable = true;
	}
	
	public void moveBytes(int source, int destination, int count) {
		//TODO implementation/arguments/efficiency?
		mustBeWritable();
		if (source == destination) {
			//TODO does this indicate a problem in the caller?
			return;
		}
		if (source > destination) {
			throw new IllegalArgumentException("destination="+destination+" after source="+source);
		}
		for (int i = 0; i < count; i++) {
			int value = array.at(source+i);
			//TODO clear old?
			array.put(source+i, 0);
			array.put(destination+i, value);
		}
	}
	public void put32(int bytePosition, int value) {
		mustBeWritable();
		array.storeInt32(bytePosition, value);
	}
	public boolean isWritable() {
		return writable;
	}

	public void printOn(PrintWriter oo) {
		oo.print(getAboraClass().name());
		oo.print("(");
		oo.print(snarfId);
		oo.print(",");
		oo.print(isWritable() ? "Write" : "Read");
		oo.print(")");
	}

	//TODO review the access/requirement of this method
	protected UInt8Array contents() {
		mustBeWritable();

		return array;
	}

	public UInt8Array getData() {
			//TODO should this be aliased on our internal data, or a clone?
			mustBeWritable();
			return getDataP();
		}
}
