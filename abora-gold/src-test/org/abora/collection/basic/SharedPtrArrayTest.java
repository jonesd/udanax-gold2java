/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.collection.basic;

import junit.framework.TestCase;

import org.abora.gold.collection.basic.SharedPtrArray;

//TODO don't really know how this class is to be used

public class SharedPtrArrayTest extends TestCase {

	public SharedPtrArrayTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(Int32ArrayTest.class);
	}

	public void testMakeCount() {
		SharedPtrArray array = (SharedPtrArray)SharedPtrArray.make(0);
		assertEquals(0, array.count());

		array = (SharedPtrArray)SharedPtrArray.make(1);
		assertEquals(1, array.count());
		assertEquals(null, array.fetch(0));
		
		try {
			SharedPtrArray.make(-1);
			fail("-1");
		} catch (NegativeArraySizeException e) {
			//expected
		}
	}
	
	public void testShare() {
		SharedPtrArray array = (SharedPtrArray)SharedPtrArray.make(2);
		assertEquals(0, array.shareCount());
		
		// shareMore
		array.shareMore();
		assertEquals(1, array.shareCount());
		array.shareMore();
		assertEquals(2, array.shareCount());

		// shareLess
		array.shareLess();
		assertEquals(1, array.shareCount());
		array.shareLess();
		assertEquals(0, array.shareCount());
		
		//TODO shareLess to take shareCount below 0?
		//TODO shareMore'ing until you wrap the count?
	}

}
