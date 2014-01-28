/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.gold.java.urdi;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.xpp.basic.Heaper;

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
