/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing.smalltalk;

import java.util.ArrayList;
import java.util.List;


public class Array {

	private final List contents;
	
	public Array() {
		this(10);
	}
	
	public Array(int size) {
		contents = new ArrayList(size);
	}

	//TODO sort out this Array vs IntArray madness
	public static IntArray newWithAll(int i, int i1) {
		throw new UnsupportedOperationException();
	}

	public int size() {
		return contents.size();
	}
	
	public static Array with(Object a) {
		Array array = new Array();
		array.put(1, a);
		return array;
	}

	public static Array with(Object a, Object b) {
		Array array = new Array();
		array.put(1, a);
		array.put(2, b);
		return array;
	}

	/**
	 * @param i1 one-based index
	 */
	public Object at(int i1) {
		return contents.get(i1-1);
	}
	
	/**
	 * @param i1 one-based index
	 */
	public void put(int i1, Object v) {
		int i0 = i1 -1;
		if (i0 == contents.size()) {
			contents.add(v);
		} else {
			contents.set(i0, v);
		}
	}

	public void basicAtPut(int i, Object arg1) {
		put(i, arg1);
	}

}
