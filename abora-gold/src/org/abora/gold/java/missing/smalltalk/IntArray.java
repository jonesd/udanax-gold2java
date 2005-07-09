/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing.smalltalk;

import java.util.Arrays;


public class IntArray {

	private final int[] array;
	
	public IntArray(int size) {
		array = new int[size];
	}

	public IntArray(int size, int defaultValue) {
		this(size);
		Arrays.fill(array, defaultValue);
	}

	public static IntArray newWithAll(int i, int i1) {
		throw new UnsupportedOperationException();
	}

	public int size() {
		return array.length;
	}
	
	public static IntArray with(int a) {
		throw new UnsupportedOperationException();
	}

	public static IntArray with(int a, int b) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Return the integer value at the index.
	 * 
	 * @param i 1 based index
	 */
	public int at(int i) {
		return array[i-1];
	}

	/**
	 * @param i 1 based index
	 */
	public void put(int i, int v) {
		array[i-1] = v;
	}

}
