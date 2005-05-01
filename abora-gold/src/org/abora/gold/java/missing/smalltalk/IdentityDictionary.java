/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing.smalltalk;


public class IdentityDictionary {

	public IdentityDictionary() {
		super();
	}

	//TODO int params are just to make things easier. Do we need to convert to object?
	public Object at(Object key) {
		throw new UnsupportedOperationException();
	}
	
	public int ifAbsent(Object key, int putIfAbsent) {
		throw new UnsupportedOperationException();
	}
	
	public void put(Object key, int value) {
		throw new UnsupportedOperationException();
	}

}
