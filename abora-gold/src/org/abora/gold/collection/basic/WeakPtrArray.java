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

import org.abora.gold.java.AboraSupport;
import org.abora.gold.java.missing.smalltalk.Set;
import org.abora.gold.wparray.XnExecutor;
import org.abora.gold.xpp.basic.Heaper;

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


}
