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
package info.dgjones.abora.collection.basic;

import junit.framework.TestCase;

import info.dgjones.abora.gold.collection.basic.SharedPtrArray;

//TODO don't really know how this class is to be used

public class SharedPtrArrayTest extends TestCase {

	public SharedPtrArrayTest(String arg0) {
		super(arg0);
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
