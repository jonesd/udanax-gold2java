/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.urdi;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import org.abora.gold.collection.basic.UInt8Array;
import org.abora.gold.xpp.basic.Heaper;

public class UrdiView extends Heaper {

	private Urdi urdi;
	private boolean isWritable = false;
	
	 boolean spent = false;
	
	private Map handles = new HashMap();
	
	public UrdiView(Urdi urdi, boolean isWritable) {
		super();
		this.urdi = urdi;
		this.isWritable = isWritable;
	}

	public int getDataSizeOfSnarf(int/*SnarfID*/ mySnarfID) {
		//TODO implement properly
		ensureNotSpent();
		return urdi.getDataSizeOfSnarf(mySnarfID);
	}
	
	private void ensureNotSpent() {
		if (spent) {
			throw new IllegalStateException("UrdiView spent: "+this);
		}
	}

	public void commitWrite() {
		ensureNotSpent();
		SortedSet toCommit = new TreeSet(new Comparator() {
			public int compare(Object o1, Object o2) {
				SnarfHandle h1 = (SnarfHandle)o1;
				SnarfHandle h2 = (SnarfHandle)o2;
				return h1.getSnarfID() - h2.getSnarfID();
			}
		});
		for (Iterator iter = handles.values().iterator(); iter.hasNext();) {
			SnarfHandle handle = (SnarfHandle) iter.next();
			if (handle.isWritable()) {
				toCommit.add(handle);
			}
		}
		urdi.writeSnarfs(toCommit);
	}

	public void becomeRead() {
		isWritable = false;
	}

	public SnarfHandle makeReadHandle(int/*SnarfID*/ snarfID) {
		//TODO placeholder
		UInt8Array snarfSpace = urdi.getSpace(snarfID);
		SnarfHandle snarfHandle = makeHandle(snarfID, snarfSpace);
		return snarfHandle;
	}

	private SnarfHandle makeHandle(int snarfID, UInt8Array snarfSpace) {
		ensureNotSpent();
		SnarfHandle existingHandle = (SnarfHandle) handles.get(new Integer(snarfID));
		if (existingHandle != null) {
			if (existingHandle.isWritable()) {
				throw new IllegalStateException("Already made writable handle for snarfID: "+snarfID);
			} else {
				return existingHandle;
			}
		} else {
			SnarfHandle snarfHandle = new SnarfHandle(snarfID, snarfSpace);
			handles.put(new Integer(snarfID), snarfHandle);
			return snarfHandle;
		}
	}

	public SnarfHandle makeErasingHandle(int/*SnarfID*/ snarfID) {
		//TODO placeholder
		UInt8Array snarfSpace = UInt8Array.make(Urdi.SNARF_SIZE);
		SnarfHandle snarfHandle = makeHandle(snarfID, snarfSpace);
		snarfHandle.makeWritable();
		return snarfHandle;
	}
}
