/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on the Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 * 
 * $Id$
 */
package org.abora.gold.collection.basic;

import org.abora.gold.primtab.PrimSetExecutor;
import org.abora.gold.xpp.basic.Heaper;

//TODO need to know more about the implementation
//TODO should this extend SharedPtrArray
public class WeakPtrArray extends PtrArray {
	//TODO use Java weak ptr to implement

	//////////////////////////////////////////////
	// Constructors
	
	public WeakPtrArray(int count) {
		super(count);
		throw new UnsupportedOperationException();
	}

	public WeakPtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		super(size, from, sourceOffset, count, destOffset);
		throw new UnsupportedOperationException();
	}

	protected WeakPtrArray(Heaper[] buffer) {
		super(buffer);
		throw new UnsupportedOperationException();
	}

	public static PtrArray make(Heaper heaper, int count) {
		throw new UnsupportedOperationException();
	}

}
