/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.collection.basic;

import org.abora.gold.wparray.XnExecutor;

public class WeakPtrArray extends PtrArray {
	protected WeakPtrArray(int count, int size) {
		super(-1, -1);
	}
	
	public static PtrArray make(XnExecutor executor, int a) {
		throw new UnsupportedOperationException();
	}

}
