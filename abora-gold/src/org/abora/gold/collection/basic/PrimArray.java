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

import org.abora.gold.java.AboraRuntimeException;
import org.abora.gold.java.Heap;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xpp.basic.Heaper;

/**
 * Array objects in smalltalk vary in size, while in x++ the
 * allocate separate storage for the varying part.  Thus they
 * require very different Recipes.
 *
 * Recipes are normally registered by the class'' associated
 * Recipe class at its own initialization time.  In smalltalk,
 * the Array classes share a common Recipe class,
 * (STArrayRecipe), which registers an instance of itself for
 * each appropriate concrete subclass of PrimArray.X.
 * 
 * (WeakPtrArrays are not in the browser because they currently 
 * have no clients and may disappear.)
 */
public class PrimArray extends Heaper {

	/** the number of elements contained */
	private int myCount;
	//Int32 myCount;  /* the number of elements contained */

	/** in units of PrimArrayHeap allocation */
	//	private int mySize;
	//Int32 mySize;   /* in units of PrimArrayHeap allocation */

	//	private int[] myStorage;
	//NOCOPY Int32 * myStorage;

	protected static int OurGutsCount = 0;

	/**
	 * we get the datum size all the way up here so that the caller does not
	 * have to repeat whatever calculation went into newCount
	 */
	protected PrimArray(int count, int datumSize) {
		super();
		//assert(PrimArray::OurGutsCount == 0);// zzz reg for allowing the debugger to do guts of
		myCount = count;
		//		if (myCount != 0) {
		//			mySize = alignUp(count * datumSize) / 4 /*sizeof(int)*/;
		//			myStorage = Heap.getStorage(mySize, this);
		//		} else {
		//			mySize = 0;
		//			myStorage = null;
		//		}
	}

	/**
	 * How many elements the array can hold
	 */
	public int count() {
		return myCount;

		//		INLINE Int32 PrimArray::count () {
		//			return myCount;
		//		}
	}

	/**
	 * Store a value; may be a Heaper, NULL, or a PrimValue as appropriate
	 * to PrimArray subclass.  It is expected that most PrimArray clients
	 * will want to use less abstract access methods
	 */
	public void storeValue(int index, Heaper value) {
		throw new UnsupportedOperationException();
	}

	/** 
	 * Fetch a value; may be a Heaper, NULL, or a PrimValue as appropriate
	 * to PrimArray subclass.  It is expected that most PrimArray clients
	 * will want to use less abstract access methods.
	 */
	public Heaper fetchValue(int index) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Same fetchValue except it will BLAST if value is NULL 
	 */
	public Heaper getValue(int index) {
		Heaper result = fetchValue(index);
		if (result == null) {
			throw new AboraRuntimeException("NOT_IN_TABLE");
		}
		return result;
		/*
				RPTR(Heaper) PrimArray::getValue (Int32 index) {
					SPTR(Heaper) result;
		
					result = this->fetchValue (index);
					if (result == NULL) {
					BLAST(NotInTable);
					}
					return result;
				}
		*/
	}

	/** 
	 * A description of the kinds of things which can be stored 
	 * in  this array
	 */
	public PrimSpec spec() {
		throw new UnsupportedOperationException();
	}

	/** Whether the two ranges contain semantically the same 
	 * values.   Two non-NULL pointers match iff the Heapers they
	 * point  to are isEqual. Two integers match iff they have the  same  value,
	 * even though they may be represented as different sizes. Two floats
	 * likewise.
	 */
	public boolean contentsEqual(PrimArray other) {
		throw new UnsupportedOperationException();
	}

	/**
	 * A hash of the entire contents of the array. If two arrays are
	 * contentsEqual, they will have the same contentsHash.
	 */
	public int contentsHash() {
		return elementsHash();
	}

	/**
	 * Copy n elements from the other array into this one. The other  array must
	 * be of a compatible type.
	 */
	public void storeMany(int to, PrimArray other, int count, int from) {
		int n;

		if (count < 0) {
			n = other.count() - from;
		} else {
			n = count;
		}
		if (to + n > count() || from + n > other.count()) {
			throw new AboraRuntimeException(AboraRuntimeException.INDEX_OUT_OF_BOUNDS);
		}
		copyElements(to, other, from, n);
	}

	/**
	 * Copy n elements from the other array into this one. The other  array must
	 * be of a compatible type.
	 */
	public void storeMany(int to, PrimArray other, int count) {
		storeMany(to, other, count, 0);
	}

	/**
	 * Copy n elements from the other array into this one. The other  array must
	 * be of a compatible type.
	 */
	public void storeMany(int to, PrimArray other) {
		storeMany(to, other, -1);
	}

	/**
	 * Make a copy of a piece of this array.  With no arguments,  copy the
	 * entire array.  With one argument copy count elements  from the beginning
	 * of the array.  With two arguments, copy 'count'  elements starting from
	 * start.  The third and fourth arguments both  default to 0	and
	 * represent the number of null/0 elements to put in  the result
	 * before/after the copied elements.  If after = 10, for  instance, the
	 * resulting array would be 10 larger than count, and  the copied elements
	 * would start at index 10.
	 */
	public PrimArray copy(int count, int start, int before, int after) {
		int copyCount;

		if (count < 0) {
			copyCount = count() - start;
		} else {
			copyCount = count;
			if (start + copyCount > count()) {
				throw new AboraRuntimeException(AboraRuntimeException.INDEX_OUT_OF_BOUNDS);
			}
		}
		return makeNew(copyCount + before + after, this, start, copyCount, before);

	}

	public PrimArray copy(int count, int start, int before) {
		return copy(count, start, before, 0);
	}

	public PrimArray copy(int count, int start) {
		return copy(count, start, 0);
	}

	public PrimArray copy(int count) {
		return copy(count, 0);
	}

	public PrimArray copy() {
		return copy(-1);
	}

	/**
	 *  Make a copy of the array into a larger array.  The array has
	 * 'after'  slots after the copied elements.
	 */
	public PrimArray copyGrow(int after) {
		return copy(count(), 0, 0, after);
	}

	/**
	 * The index of the nth occurrence of the given value at or   after (before
	 * if n is negative) the given index, or -1 if   there is none.
	 */
	public int indexOf(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public int indexOf(Heaper value, int start) {
		return indexOf(value, start, 1);
	}

	public int indexOf(Heaper value) {
		return indexOf(value, 0);
	}

	/**
	 * The index of the nth occurrence of the given sequence of values at or
	 * after (before if n is negative) the given starting index, or -1 if there
	 * is none. Negative numbers for start are relative to the end of the array.
	 */
	public int indexOfElements(PrimArray other, int valueCount, int valueStart, int start, int nth) {
		throw new UnsupportedOperationException();
		//		int valueN;
		//		int result;
		//		int n;
		//
		//		if (count() == 0 || nth == 0) {
		//			return -1;
		//		}
		//		if (valueCount < 0) {
		//			valueN = other.count();
		//		} else {
		//			if (valueCount > other.count()) {
		//				throw new AboraRuntimeException(AboraRuntimeException.INDEX_OUT_OF_BOUNDS);
		//			}
		//			valueN = valueCount;
		//		}
		//		if (start >= 0) {
		//			result = start;
		//		} else {
		//			result = count() + start;
		//		}
		//		n = nth < 0 ? -nth : nth;
		//		boolean forward = nth > 0;
		//		for (;;) {
		//			{
		//				if (forward ? (result > count()) - valueN : result < 0) {
		//					return -1;
		//				}
		//				if (elementsEqual(result, other, valueStart, valueN).asBoolean()) {
		//					n -= 1;
		//					if (n == 0) {
		//						return result;
		//					}
		//				}
		//				if (forward) {
		//					result += 1;
		//				} else {
		//					result -= 1;
		//				}
		//			}
		//		}
	}

	public int indexOfElements(PrimArray other, int valueCount, int valueStart, int start) {
		return indexOfElements(other, valueCount, valueStart, start, 1);
	}

	public int indexOfElements(PrimArray other, int valueCount, int valueStart) {
		return indexOfElements(other, valueCount, valueStart, 0);
	}

	public int indexOfElements(PrimArray other, int valueCount) {
		return indexOfElements(other, valueCount, 0);
	}

	public int indexOfElements(PrimArray other) {
		return indexOfElements(other, -1);
	}

	/**
	 * The index of the nth occurrence of anything but the given value at or
	 * after (before if n is negative) the given index, or -1 if there is none.
	 */
	public int indexPast(Heaper value, int start, int n) {
		int i;
		int idx;
		for (idx = start, i = 0; i < n && idx < count(); idx++) {
			Heaper ptr = fetchValue(idx);
			if (ptr != null && !ptr.isEqual(value)) {
				i++;
			}
		}
		return i > 0 ? idx : -1;
	}

	public int indexPast(Heaper value, int start) {
		return indexPast(value, start, 1);
	}

	public int indexPast(Heaper value) {
		return indexPast(value, 0);
	}

	/** 
	 * Set a range of elements to have the same value
	 */
	public void storeAll(Heaper value, int count, int start) {
		throw new UnsupportedOperationException();
	}

	public void storeAll(Heaper value, int count) {
		storeAll(value, count, 0);
	}

	public void storeAll(Heaper value) {
		storeAll(value, -1);
	}

	public void storeAll() {
		storeAll(null);
	}

	/**
	 * Copy of a piece of this array into the provided buffer with
	 * size bytes of space available.  The default is to start at
	 * the beginning and go to the end.  The elements will be copied
	 * from this array beginning with start, and taking as many
	 * of count elements or upto the end of this array as will fit in
	 * size bytes.  WARNING:  Note that if this array is a PtrArray,
	 * the pointers copied to buffer will not be considered as references
	 * for garbage collection.  Therefore the buffer should not be allowed
	 * to contain the only pointer to an object in a garbage collecting
	 * environment.
	 */
	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}

	public void copyToBuffer(int[] buffer, int size, int count) {
		copyToBuffer(buffer, size, count, 0);
	}

	public void copyToBuffer(int[] buffer, int size) {
		copyToBuffer(buffer, size, -1);
	}

	/**
	 * Whether the two ranges contain the same values, using the criteria
	 * defined in contentsEqual
	 */
	public boolean elementsEqual(int here, PrimArray other, int there, int n) {
		throw new UnsupportedOperationException();
	}

	public boolean elementsEqual(int here, PrimArray other, int there) {
		return elementsEqual(here, other, there, -1);
	}

	public boolean elementsEqual(int here, PrimArray other) {
		return elementsEqual(here, other, 0);
	}

	/**
	 * A hash of the range of values out of the array. If two ranges are
	 * elementsEqual, they will have the same hash. For data values, additional
	 * zeros on the end make no difference to the hash.
	 */
	public int elementsHash(int count, int start) {
		throw new UnsupportedOperationException();
	}

	public int elementsHash(int count) {
		return elementsHash(count, 0);
	}

	public int elementsHash() {
		return elementsHash(-1);
	}

	public void printOn(PrintWriter oo) {
		String before = "[";
		if (count() == 0) {
			oo.print("[empty");
		} else {
			for (int i = 0; i < count(); i += 1) {
				oo.print(before);
				printElementOn(i, oo);
				before = " ";
			}
		}
		oo.print("]");
	}

	protected int rangeCheck(int index) {
		if (index < 0 || index >= count()) {
			outOfBounds();
		}
		return index;

		//		INLINE Int32 PrimArray::rangeCheck (Int32 index) {
		//		#if ! (defined(NO_PRIMARRAY_RANGE_CHECK) || defined(PRODUCT))
		//			if (index < 0 || index >= this->count()) {
		//			this->outOfBounds();
		//			}
		//		#endif /* NO_PRIMARRAY_RANGE_CHECK */
		//			return index;
		//		}
	}

	protected int[] storage() {
		throw new UnsupportedOperationException();
	}

	private void receivePrimArray(Rcvr rcvr) {
		throw new UnsupportedOperationException();
	}

	protected void printElementOn(int index, PrintWriter oo) {
		throw new UnsupportedOperationException();
	}

	/**
	 * subclasses with non-32 bit or other interesting values should override
	 */
	protected void copyElements(int to, PrimArray source, int from, int count) {
		int n = count;
		if (n == -1) {
			n = source.count() - from;
		}
		if (n < 0 || to < 0 || from < 0 || from + n > source.count() || to + n > count()) {
			throw new AboraRuntimeException(AboraRuntimeException.COPY_OUT_OF_BOUNDS);
		}
		//		if (getCategory() == source.getCategory()) {
		//			/* we can hold the source storage pointer since this is atomic */
		//			for (int i = 0; i < n; i += 1) {
		//				myStorage[to + i] = source.storage()[from + i];
		//			}
		//		} else {
		/* since the types aren''t the same, we have to do it the hard way */
		for (int i = 0; i < n; i += 1) {
			storeValue(to + i, source.fetchValue(from + i));
			//			}
		}
	}

	protected void zeroElements(int from, int count) {
		throw new UnsupportedOperationException();
		//		int n = count;
		//
		//		if (n < 0) {
		//			n = count();
		//		}
		//		if (n + from > count()) {
		//			throw new AboraRuntimeException(AboraRuntimeException.TOO_MANY_ZEROS);
		//		}
		//		if (from < 0) {
		//			throw new AboraRuntimeException(AboraRuntimeException.BOGUS_START_INDEX);
		//		}
		//		Arrays.fill(myStorage, from, from + n, 0);
	}

	protected void zeroElements(int from) {
		zeroElements(from, -1);
	}

	protected void zeroElements() {
		zeroElements(0);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public static void cleanup() {
		Heap.cleanup();
	}

	/** 
	 * the canonical out-of-bounds blast for prim arrays
	 */
	protected void outOfBounds() {
		throw new AboraRuntimeException(AboraRuntimeException.INDEX_OUT_OF_BOUNDS);
	}

	/** used in compaction */
	protected int size() {
		throw new UnsupportedOperationException();
	}
	/** used in compaction */
	protected void moveTo(int newLoc) {
		throw new UnsupportedOperationException();
	}
}
