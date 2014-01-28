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
package info.dgjones.abora.gold.collection.basic;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class SharedPtrArray extends PtrArray {
	private int myShareCount = 0;

	public static void initializeClassAttributes() {
		//TODO just made up out of thin air - totally wrong!!!
		AboraSupport.findAboraClass(SharedPtrArray.class).setAttributes( new Set().add("CONCRETE").add("PSEUDOCOPY"));
	}

	//////////////////////////////////////////////
	// Constructors

	protected SharedPtrArray(int count) {
		super(count);
	}
	
	public SharedPtrArray(Rcvr rcvr) {
		super(rcvr);
	}


	protected SharedPtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		super(size, from, sourceOffset, count, destOffset);
	}

	protected SharedPtrArray(Heaper[] buffer) {
		super(buffer);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create a PtrArray filled with NULLs */
	public static PtrArray make(int count) {
		return new SharedPtrArray(count);
	}

	/** create a SharedPtrArray filled with the indicated data in 'from' */
	public static PtrArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new SharedPtrArray(size, from, sourceOffset, count, destOffset);
	}

	public static PtrArray make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static PtrArray make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static PtrArray make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static PtrArray make(PrimArray from) {
		return make(from.count(), from, 0);
	}

	/** create a PtrArray filled with the data from 'buffer' */
	public static PtrArray make(Heaper[] buffer) {
		return new SharedPtrArray(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PtrArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	public PrimSpec spec() {
		return PrimSpec.sharedPointer();
	}

	public int shareCount() {
		return myShareCount;
	}

	public void shareLess() {
		myShareCount -= 1;
		//TODO should we throw an exception or just ground myShareCount to 0?
		if (myShareCount < 0) {
			throw new AboraRuntimeException("Share fallen below 0: "+this);
		}
	}

	public void shareMore() {
		myShareCount += 1;
	}
}
