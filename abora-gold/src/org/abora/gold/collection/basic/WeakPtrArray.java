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

import org.abora.gold.wparray.XnExecutor;

//TODO need to know more about the implementation
//TODO should this extend SharedPtrArray
public class WeakPtrArray extends PtrArray {
	//TODO should we use Java weak ptr to implement

	//TODO how is this executor used?
	protected final XnExecutor executor;
	
	//////////////////////////////////////////////
	// Constructors
	
	public WeakPtrArray(XnExecutor executor, int count) {
		super(count);
		this.executor = executor;
	}

//	public WeakPtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
//		super(size, from, sourceOffset, count, destOffset);
//		throw new UnsupportedOperationException();
//	}

//	protected WeakPtrArray(Heaper[] buffer) {
//		super(buffer);
//		throw new UnsupportedOperationException();
//	}

	public static PtrArray make(XnExecutor executor, int count) {
		return new WeakPtrArray(executor, count);
	}

}
