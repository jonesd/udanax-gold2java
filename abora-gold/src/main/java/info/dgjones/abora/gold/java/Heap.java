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
package info.dgjones.abora.gold.java;

import info.dgjones.abora.gold.collection.basic.PrimArray;

public class Heap {
	//@todo should be called PrimArray Heap - just couldnt create a class with 
	// that name in Eclipse - possibly due to a bug
	private Heap myNextHeap;

	/** start of contiguous space at end */
	private int[] myEndSpace;
	/** table of live PrimArrays on this heap */
	PrimArray  myPrimArrayTable;
	/** Last word in heap */
	int[] myLastWord;
	/** PrimArray table entry after first hole in heap or NULL */
	PrimArray myFirstAfterHole;
	/* the actual heap storage */
	int[] myHeapStore;

	/** list of all PrimArrayHeaps in system */
	static Heap FirstHeap;

	/**
	 * Constructor for Heap.
	 */
	public Heap(int[] size) {
		super();
	}

	/** allocate from particular heap in sizeof(Int32) units and register array */
	public int[] allocate(int nWords, PrimArray pa) {
		throw new UnsupportedOperationException();
	}

	/** compacts array table after GC */
	public void removeDestructedArrays() {
		throw new UnsupportedOperationException();
	}

	private void compact() {
		throw new UnsupportedOperationException();
	}

	/** compacts all array tables after GC */
	public static void cleanup() {
		throw new UnsupportedOperationException();
	}

	/** from any heap */
	public static int[] getStorage(int nWords, PrimArray pa) {
		throw new UnsupportedOperationException();
	}

}
