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



public class OrderedCollection {
	
	private final List contents;

	public OrderedCollection() {
		this(10);
	}
	
	public OrderedCollection(int c) {
		super();
		contents = new ArrayList(c);
	}
	
	public static OrderedCollection with(Object a, Object b) {
		throw new UnsupportedOperationException();
	}

	public void add(Object heaper) {
		contents.add(heaper);
	}

	public int size() {
		return contents.size();
	}
	
	/**
	 * @param index one-based index
	 */
	public Object get(int index) {
		return contents.get(index);
	}

}
