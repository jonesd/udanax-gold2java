/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.collection.basic;

import java.io.PrintWriter;

import org.abora.gold.java.exception.AboraRuntimeException;
import org.abora.gold.x.PrimFloatValue;
import org.abora.gold.x.PrimIEEE32;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xpp.basic.Heaper;

public class IEEE32Array extends PrimFloatArray {
	private float[] myStorage = null;

	protected IEEE32Array(int count, int datumSize) {
		super(count, datumSize);
		myStorage = new float[count];
	}

	protected IEEE32Array(int count) {
		this(count, 4);
	}
	//	IEEE32Array (Int32 count, TCSJ);

	protected IEEE32Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(-1, -1);
		throw new UnsupportedOperationException();
	}

	protected IEEE32Array(int count, int[] buffer) {
		this(-1, -1);
		throw new UnsupportedOperationException();
	}

	/** create an IEEE32Array filled with zeros */
	public static IEEE32Array make(int count) {
		return new IEEE32Array(count);
	}

	/** create an IEEE32Array filled with the indicated data in 'from' */
	public static IEEE32Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		IEEE32Array array = new IEEE32Array(size);
		array.addData(destOffset, (PrimDataArray) from, sourceOffset, count);
		return array;
	}

	public static IEEE32Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static IEEE32Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static IEEE32Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create an IEEE32Array filled with the data at 'buffer' */
	public static IEEE32Array make(float[] buffer) {
		IEEE32Array array = make(buffer.length);
		//@todo should we copy data or use it?
		for (int i = 0; i < buffer.length; i++) {
			array.storeIEEE32(i, buffer[i]);
		}
		return array;
	}

	/** Store an actual floating point value */
	public void storeIEEE32(int index, float value) {
		myStorage[rangeCheck(index)] = value;

		//		INLINE void IEEE32Array::storeIEEE32 (Int32 index, IEEE32 value){
		//			/* Store a floating point value */
		//    
		//			((IEEE32*)this->storage())[this->rangeCheck (index)] = value;
		//		}
	}

	/** Get an actual floating point number */
	public float iEEE32At(int index) {
		return myStorage[rangeCheck(index)];

		//		INLINE IEEE32 IEEE32Array::iEEE32At (Int32 index){
		//			/* Get an actual floating point number */
		//
		//			return ((IEEE32*)this->storage())[this->rangeCheck (index)];
		//		}
	}

	public void storeFloat(int index, double value) {
		storeIEEE32(index, (float) value);

		//		void IEEE32Array::storeFloat (Int32 index, IEEE64 value){
		//			/* Store a floating point value */
		//
		//			this->storeIEEE32(index, value);
		//		}
	}

	public double floatAt(int index) {
		return (double) iEEE32At(index);

		//		IEEE64 IEEE32Array::floatAt (Int32 index){
		//			/* Get an actual floating point number */
		//
		//			return (IEEE64) this->iEEE32At(index);
		//		}
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new AboraRuntimeException(AboraRuntimeException.NULL_VALUE);
		}
		storeIEEE32(index, ((PrimFloatValue) value).asIEEE32());

		//		void IEEE32Array::storeValue (Int32 index, APTR(Heaper) OR(NULL) value){
		//			if (value == NULL) {
		//			BLAST(NULL_VALUE);
		//			}
		//			this->storeIEEE32(index, CAST(PrimFloatValue,value)->asIEEE32());
		//		}
	}

	public Heaper fetchValue(int index) {
		return PrimIEEE32.make(iEEE32At(index));

		//		RPTR(Heaper) OR(NULL) IEEE32Array::fetchValue (Int32 index) {
		//			return PrimIEEE32::make(this->iEEE32At(index));
		//		}
	}

	public PrimSpec spec() {
		return PrimSpec.iEEE32();

		//		RPTR(PrimSpec) IEEE32Array::spec (){
		//			return PrimSpec::iEEE32();
		//		}
	}

	/** Return the maximum word size that can be stored in this array */
	public int bitCount() {
		return 32;

		//		Int32 IEEE32Array::bitCount () {
		//			/* Return the maximum bits/entry that can be stored in this array */
		//
		//			return 32;
		//		}
	}

	public void storeAll(Heaper value, int count, int start) {
		int n = count() - start;
		if (count > n) {
			throw new AboraRuntimeException(AboraRuntimeException.INDEX_OUT_OF_BOUNDS);
		}
		if (count >= 0) {
			n = count;
		}
		float f;
		if (value == null) {
			f = 0.0f;
		} else {
			f = ((PrimFloatValue) value).asIEEE32();
		}
		for (int i = 0; i < n; i += 1) {
			storeIEEE32(start + i, f);
		}

		//		void IEEE32Array::storeAll (APTR(Heaper) value/* = NULL*/, 
		//						Int32 count/* = -1*/,
		//						Int32 start/* = Int32Zero*/)
		//		{
		//			IEEE64 f;
		//			Int32 n;
		//
		//			n = this->count() - start;
		//			if (count > n) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//			if (count >= 0) {
		//			n = count;
		//			}
		//			if (value == NULL) {
		//			f = 0.0;
		//			} else {
		//			f = CAST(PrimFloatValue,value)->asIEEE32();
		//			}
		//			for (Int32 i = 0; i < n; i += 1) {
		//			this->storeIEEE32(start + i, f);
		//			}
		//		}
	}

	public void copyToBuffer(float[] buffer, int size, int count, int start) {
		int bufSize = buffer.length;
		int n;
		if (count >= 0) {
			n = count;
		} else {
			n = count() - start;
		}
		if (n > bufSize) {
			n = bufSize;
		}
		for (int i = 0; i < n; i += 1) {
			buffer[i] = iEEE32At(start + i);
		}

		//		void IEEE32Array::copyToBuffer (void * buffer,
		//						Int32 size,
		//						Int32 count /*= -1*/,
		//						Int32 start /* = Int32Zero*/)
		//		{
		//			Int32 bufSize;
		//			Int32 n;
		//
		//			bufSize = size / sizeof(IEEE32);
		//			if (count >= 0) {
		//			n = count;
		//			} else {
		//			n = this->count() - start;
		//			}
		//			if (n > bufSize) {
		//			n = bufSize;
		//			}
		//			MEMMOVE (buffer, (IEEE32*)this->storage() + start,
		//				 (int)(n * sizeof(IEEE32)));
		//		}
	}

	protected int compareData(int myStart, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IEEE32Array) {
			IEEE32Array o = (IEEE32Array) other;
			for (int i = 0; i < count; i += 1) {
				float cmp = iEEE32At(i + myStart) - o.iEEE32At(i + otherStart);
				if (cmp != 0.0) {
					return ((int) cmp) < 0 ? -1 : 1;
				}
			}
			return 0;
		} else {
			return super.compareData(myStart, other, otherStart, count);
		}

		//		Int32 IEEE32Array::compareData (Int32 start, 
		//						APTR(PrimDataArray) other,
		//						Int32 otherStart,
		//						Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(IEEE32Array, o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				IEEE32 cmp;
		//				cmp = this->iEEE32At(i + start) - o->iEEE32At(i + otherStart);
		//				if (cmp != 0.0) {
		//					return ((Int32) cmp) < 0 ? -1 : 1;
		//				}
		//				}
		//				return 0;
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				return this->PrimFloatArray::compareData (start, other,
		//									  otherStart, count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//			return 0;
		//		}
	}

	protected int signOfNonZeroAfter(int start) {
		for (int i = start; i < count(); i += 1) {
			float val = iEEE32At(i);
			if (val < 0.0) {
				return -1;
			}
			if (val > 0.0) {
				return +1;
			}
		}
		return 0;

		//		Int32 IEEE32Array::signOfNonZeroAfter (Int32 index) {
		//			for (Int32 i = index; i < this->count(); i += 1) {
		//			IEEE32 val;
		//	
		//			if ((val = this->iEEE32At(i)) < 0.0) {
		//				return -1;
		//			}
		//			if (val > 0.0) {
		//				return +1;
		//			}
		//			}
		//			return 0;
		//		}
	}

	protected void addData(int myStart, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IEEE32Array) {
			IEEE32Array o = (IEEE32Array) other;
			for (int i = 0; i < count; i += 1) {
				storeIEEE32(i + myStart, iEEE32At(i + myStart) + o.iEEE32At(i + otherStart));
			}
		} else {
			super.addData(myStart, other, otherStart, count);
		}

		//		void IEEE32Array::addData (Int32 start, 
		//					   APTR(PrimDataArray) other,
		//					   Int32 otherStart,
		//					   Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(IEEE32Array,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				this->storeIEEE32 (i + start,
		//						   this->iEEE32At(i + start) 
		//						   + o->iEEE32At(i + otherStart));
		//				}
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				this->PrimFloatArray::addData (start, other, otherStart, count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//		}
	}

	protected void subtractData(int myStart, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IEEE32Array) {
			IEEE32Array o = (IEEE32Array) other;
			for (int i = 0; i < count; i += 1) {
				storeIEEE32(i + myStart, iEEE32At(i + myStart) - o.iEEE32At(i + otherStart));
			}
		} else {
			super.subtractData(myStart, other, otherStart, count);
		}

		//		void IEEE32Array::subtractData (Int32 start, 
		//						APTR(PrimDataArray) other,
		//						Int32 otherStart,
		//						Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(IEEE32Array,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				this->storeIEEE32 (i + start,
		//						   this->iEEE32At(i + start) 
		//						   - o->iEEE32At(i + otherStart));
		//				}
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				this->PrimFloatArray::subtractData (start, other, otherStart,
		//								count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//		}
	}

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(iEEE32At(index));

		//		void IEEE32Array::printElementOn (Int32 index, ostream& oo)
		//		{
		//			oo << this->iEEE32At (index);
		//		}
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimFloatArray) source, sourceOffset, count, destOffset);

		//		RPTR(PrimArray) IEEE32Array::makeNew (Int32 size,
		//							  APTR(PrimArray) source,
		//							  Int32 sourceOffset,
		//							  Int32 count,
		//							  Int32 destOffset)
		//		{
		//			return IEEE32Array::make (size, CAST(PrimFloatArray,source),
		//						  sourceOffset, count, destOffset);
		//		}
	}

}
