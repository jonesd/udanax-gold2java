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
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

//TODO need to know more about the implementation
//TODO should this extend SharedPtrArray
public class WeakPtrArray extends PtrArray {
	//TODO should we use Java weak ptr to implement

	//TODO how is this executor used?
	protected final XnExecutor executor;
	
	public static void initializeClassAttributes() {
		//TODO just made up out of thin air - totally wrong!!!
		AboraSupport.findAboraClass(WeakPtrArray.class).setAttributes( new Set().add("CONCRETE").add("PSEUDOCOPY"));
	}

	//////////////////////////////////////////////
	// Constructors
	
	public WeakPtrArray(XnExecutor executor, int count) {
		super(count);
		this.executor = executor;
	}
	
	public WeakPtrArray(Rcvr rcvr) {
		super(rcvr);
		executor = (XnExecutor)rcvr.receiveHeaper();
	}


	public WeakPtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		super(size, from, sourceOffset, count, destOffset);
		executor = new XnExecutor() {
			public void execute(int estateIndex) {
				throw new UnsupportedOperationException();
			};
		};
	}

	protected WeakPtrArray(Heaper[] buffer) {
		super(buffer);
		executor = new XnExecutor() {
			public void execute(int estateIndex) {
				throw new UnsupportedOperationException();
			};
		};
	}

	protected WeakPtrArray(int count) {
		super(count);
		executor = new XnExecutor() {
			public void execute(int estateIndex) {
				throw new UnsupportedOperationException();
			};
		};
	}

	public static PtrArray make(XnExecutor executor, int count) {
		return new WeakPtrArray(executor, count);
	}
	
	//TODO need to override most of the primary array static implementation
	
	/** create a PtrArray filled with the indicated data in 'from' */
	public static PtrArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new WeakPtrArray(size, from, sourceOffset, count, destOffset);
	}
	
	public static PtrArray make(int count) {
		return new WeakPtrArray(count);
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

	/** create a PtrArray filled with data from 'buffer' */
	public static PtrArray make(Heaper[] buffer) {
		return new WeakPtrArray(buffer);
	}

	/** create a zero size PtrArray */
	public static PtrArray empty() {
		//TODO cache empty array
		return make(0);
	}


	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (WeakPtrArray) source, sourceOffset, count, destOffset);
		//		RPTR(PrimArray) PtrArray::makeNew (Int32 size,
		//						   APTR(PrimArray) source,
		//						   Int32 sourceOffset,
		//						   Int32 count,
		//						   Int32 destOffset)
		//		{
		//			return PtrArray::make (size, CAST(PtrArray,source), sourceOffset, count,
		//					   destOffset);
		//		}
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(executor);
	}

}
