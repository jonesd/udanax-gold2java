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

import java.io.PrintWriter;

import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Array object of a fixed size composed of elements of the same type.
 */
public abstract class PrimArray extends Heaper {
	//TODO Implement Collection?

	//////////////////////////////////////////////
	// Constructors

	/**
	 * Construct a new array.
	 * 
	 * Restrict public access to constructor; use suitable static
	 * factory method instead.  
	 */
	protected PrimArray() {
		super();
	}
	
	public PrimArray(Rcvr rcvr) {
		super(rcvr);
	}

	/**
	 * Return a new array of the same type as <code>this</code>
	 * initialized with the elements of <code>source</code> specified by
	 * the index range, prepended by <code>destOffset</code> and postpended by
	 * null or 0 elements.
	 * 
	 * @param size size of new array.
	 * @param source source of elements to be initialized with in the new array.
	 * @param sourceOffset first index of the <code>source</code> array to be included in the range.
	 * @param count number of elements of <code>source</code> to include in the new array
	 * @param destOffset first index of new array to be initialized with <code>source</code> elements.
	 * @return the new array.
	 */
	protected abstract PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset);

	/**
	 * Return the number of elements the array can hold.
	 */
	public abstract int count();

	/** 
	 * A description of the kinds of things which can be stored 
	 * in  this array
	 */
	public abstract PrimSpec spec();

	//////////////////////////////////////////////
	// Accessing

	/**
	 * Store a value; may be a Heaper, null, or a PrimValue as appropriate
	 * to PrimArray subclass. An exception will be throw if the value is
	 * not compatible with the elements <code>this</code> can hold.
	 * <p>
	 * It is expected that most PrimArray clients will want to use
	 * less abstract access methods
	 * 
	 * @param index index in array the element will be stored at.
	 * @param value heaper to store in <code>this</code>.
	 * @throws ClassCastException if <code>value</code> is not compatible with the elements of this array
	 * @throws IllegalArgumentException if <code>value</code> can not be held by this array
	 */
	public abstract void storeValue(int index, Heaper value);

	/** 
	 * Fetch a value; may be a Heaper, null, or a PrimValue as appropriate
	 * to PrimArray subclass.
	 * <p> 
	 * It is expected that most PrimArray clients
	 * will want to use less abstract access methods.
	 * 
	 * @param index index in array whose element will be returned
	 * @return the heaper at the specified <code>index</code>.
	 */
	public abstract Heaper fetchValue(int index);

	/** 
	 * Fetch a value; may be a Heaper or a PrimValue as appropriate
	 * to PrimArray subclass, or throw an exception if value is null.
	 * <p>
	 * It is expected that most PrimArray clients will want to use
	 * less abstract access methods.
	 * 
	 * @param index index in array whose element will be returned.
	 * @return the heaper at the specified <code>index</code>.
	 * @throws NotInTableException if a null value was fetched.
	 */
	public Heaper getValue(int index) {
		Heaper result = fetchValue(index);
		if (result == null) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
		}
		return result;
	}

	//////////////////////////////////////////////
	// Comparing and Hashing

	//TODO made up
	public boolean isEqual(Heaper other) {
		if (other instanceof PrimArray) {
			PrimArray o = (PrimArray) other;
			return contentsEqual(o);
		} else {
			return false;
		}
	}

	/** 
	 * Whether the two ranges contain semantically the same 
	 * values.   Two non-NULL pointers match iff the Heapers they
	 * point  to are isEqual. Two integers match iff they have the  same  value,
	 * even though they may be represented as different sizes. Two floats
	 * likewise.
	 */
	public abstract boolean contentsEqual(PrimArray other);

	/**
	 * A hash of the entire contents of the array. If two arrays are
	 * contentsEqual, they will have the same contentsHash.
	 */
	public int contentsHash() {
		return elementsHash();
	}

	/**
	 * Whether the two ranges contain the same values, using the criteria
	 * defined in contentsEqual
	 */
	public abstract boolean elementsEqual(int here, PrimArray other, int there, int n);

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
	public abstract int elementsHash(int count, int start);

	public int elementsHash(int count) {
		return elementsHash(count, 0);
	}

	public int elementsHash() {
		return elementsHash(-1);
	}

	//////////////////////////////////////////////
	// Bulk Storing

	/** 
	 * Fill a consequitive range of elements with the supplied <code>value</code>.
	 *  
	 * @param value to store within range or 0.0 if null.
	 * @param count number of consequentive elements in range or all
	 * 			elements from <code>start</code> if -1.
	 * @param start index of first element in range.
	 * @throws ClassCastException if <code>other</code> is not compatible with this array
	 * @throws IllegalArgumentException if <code>other</code>s elements can not be held by this array
	 */
	//TODO possibly rename to fill(...) to better match Java terminology
	public abstract void storeAll(Heaper value, int count, int start);

	public void storeAll(Heaper value, int count) {
		storeAll(value, count, 0);
	}

	/** 
	 * Fill every element of this to value or null or 0 depending on the type of array.
	 */
	public void storeAll(Heaper value) {
		storeAll(value, -1);
	}
	
	public void storeAll() {
		storeAll(zeroElement());
	}

	/**
	 * Copy the respective elements of <code>other</code> to <code>this</code> over the specified index range.
	 * The other array must be of a compatible type. 
	 * 
	 * @param to first index of receiver to be included in the receivers range..
	 * @param other other elements to be copied into this.
	 * @param count number of elements from the other array included in range, or -1 for
	 * 			all others elements starting at from. Fail if <code>count</code> is
	 * 			greater than number of available elements.
	 * @param from first index of the other array to be included in the range.
	 * @throws ClassCastException if <code>other</code> is not compatible with this array
	 * @throws IllegalArgumentException if <code>other</code>s elements can not be held by this array
	 */
	public void storeMany(int to, PrimArray other, int count, int from) {
		int n;

		if (count < 0) {
			n = other.count() - from;
		} else {
			n = count;
		}
		if (to + n > count() || from + n > other.count()) {
			throw new IndexOutOfBoundsException();
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
	 * Return a copy of this including just the elements specified by
	 * the index range prepended by <code>before</code>, and postpended by <code>after</code>
	 * number of null or 0 elements.
	 * <p>
	 * If <code>before == 10</code>, then the the resulting array with be 10 elements
	 * larger and the copied elements would start at index 10.
	 * 
	 * @param count number of elements to copy, or -1 for all from and after start
	 * @param start index of first element to copy from
	 * @param before number of leading null or 0 elements to include before the copied
	 * 	elements in the returned array.
	 * @param after number of trailing null or 0 elements to include after the copied
	 * 	elements in the returned array.
	 * @return PrimArray a copy of this.
	 */
	public PrimArray copy(int count, int start, int before, int after) {
		int copyCount;

		if (count < 0) {
			copyCount = count() - start;
		} else {
			copyCount = count;
			if (start + copyCount > count()) {
				throw new IndexOutOfBoundsException();
			}
		}
		return makeNew(copyCount + before + after, this, start, copyCount, before);
	}

	/**
	 * Return a copy of this including just the elements specified by
	 * the index range prepended by <code>before</code> number of null or 0 elements.
	 * <p>
	 * If <code>before == 10</code>, then the the resulting array with be 10 elements
	 * larger and the copied elements would start at index 10.
	 * 
	 * @param count number of elements to copy, or -1 for all from and after start
	 * @param start index of first element to copy from
	 * @param before number of leading null or 0 elements in include before the copied
	 * 	elements in the returned array.
	 * @return PrimArray a copy of this.
	 */
	public PrimArray copy(int count, int start, int before) {
		return copy(count, start, before, 0);
	}

	/**
	 * Return a copy of this including just the elements specified by
	 * the index range.
	 * 
	 * @param count number of elements to copy, or -1 for all from and after start
	 * @param start index of first element to copy from
	 * @return PrimArray a copy of this
	 */
	public PrimArray copy(int count, int start) {
		return copy(count, start, 0);
	}

	/**
	 * Return a copy of this including just the first count elements.
	 * 
	 * @param count
	 * @return PrimArray a copy of this.
	 */
	public PrimArray copy(int count) {
		return copy(count, 0);
	}

	/**
	 * Return a copy of this.
	 * 
	 * @return a copy of this.
	 */
	public PrimArray copy() {
		return copy(-1);
	}

	/**
	 * Return a copy of this with <code>after</code> null/0 elements
	 * postpended to the copied elements.
	 * 
	 * @param after number of trailing null or 0 elements to include after the copied
	 * 	elements in the returned array.
	 */
	public PrimArray copyGrow(int after) {
		return copy(count(), 0, 0, after);
	}

	/**
	 * Copy the respective elements of <code>other</code> to <code>this</code> over the specified index range.
	 * The other array must be of a compatible type. 
	 * 
	 * @param to first index of receiver to be included in the receivers range..
	 * @param other other elements to be copied into this.
	 * @param from first index of the other array to be included in the range.
	 * @param count number of elements from the other array included in range, or -1 for
	 * 			all others elements starting at from. Fail if <code>count</code> is
	 * 			greater than number of available elements.
	 */
	protected void copyElements(int to, PrimArray other, int from, int count) {
		int n = count;
		if (n == -1) {
			n = other.count() - from;
		}
		if (n < 0 || to < 0 || from < 0 || from + n > other.count() || to + n > count()) {
			throw new IndexOutOfBoundsException();
		}
		for (int i = 0; i < n; i += 1) {
			storeValue(to + i, other.fetchValue(from + i));
		}
	}

	//////////////////////////////////////////////
	// Searching/Finding

	/**
	 * Return the index of the nth occurrence of the given value at or
	 * after (before if nth is negative) the given index, or -1 if
	 * there is none.
	 * 
	 * @param value element that is to be matched
	 * @param start index to start the search. If positive start from that index,
	 * 			if negative start from relatie to end of array
	 * @param nth nth occurrence of the matched value at or after the start if
	 * 			positive, or at or before if negative. A 0 nth immediately fails.
	 * @return index of element matched or -1 if there is none
	 */
	public abstract int indexOf(Heaper value, int start, int n);

	/**
	 * Return the index of the first occurrence of the given value at or
	 * after (before if nth is negative) the given index, or -1 if
	 * there is none.
	 * 
	 * @param value element that is to be matched
	 * @param start index to start the search. If positive start from that index,
	 * 			if negative start from relatie to end of array
	 * @return index of element matched or -1 if there is none
	 */
	public int indexOf(Heaper value, int start) {
		return indexOf(value, start, 1);
	}

	/**
	 * Return the index of the first occurrence of the given value,
	 *  or -1 if there is none.
	 * 
	 * @param value element that is to be matched
	 * @return index of element matched or -1 if there is none
	 */
	public int indexOf(Heaper value) {
		return indexOf(value, 0);
	}

	/**
	 * Return the index of the nth occurrence of anything but the given value at or
	 * after (before if nth is negative) the given index, or -1 if
	 * there is none.
	 * 
	 * @param value anything except this element that is to be matched
	 * @param start index to start the search. If positive start from that index,
	 * 			if negative start from relatie to end of array
	 * @param nth nth occurrence of the matched value at or after the start if
	 * 			positive, or at or before if negative. A 0 nth immediately fails.
	 * @return index of element matched or -1 if there is none
	 */
	public abstract int indexPast(Heaper value, int start, int n);
	//TODO original X++ version had an implementation but it did
	// not appear to match spec, or inherited versions, so removed

	/**
	 * Return the index of the first occurrence of anything but the given value at or
	 * after (before if nth is negative) the given index, or -1 if
	 * there is none.
	 * 
	 * @param value anything except this element that is to be matched
	 * @param start index to start the search. If positive start from that index,
	 * 			if negative start from relatie to end of array
	 * @return index of element matched or -1 if there is none
	 */
	public int indexPast(Heaper value, int start) {
		return indexPast(value, start, 1);
	}

	/**
	 * Return the index of the first occurrence of anything but the given value,
	 * or -1 if there is none.
	 * 
	 * @param value anything except this element that is to be matched
	 * @return index of element matched or -1 if there is none
	 */
	public int indexPast(Heaper value) {
		return indexPast(value, 0);
	}

	/**
	 * Return the index of the <code>nth</code> occurrence of the given sequence
	 * of values of other at or after (before if <code>nth</code> is negative) the given
	 * index in this, or -1 if there is none. Negative numbers for <code>start</code> are
	 * relative to the end of the array.
	 * 
	 * @param other array of elements that is being searched for in this.
	 * @param otherCount number of elements from <code>other</code> that is being
	 * 	searched for, or -1 to include all elements after <code>otherStart</code>.
	 * @param otherStart index of first element of <code>other</code> to start searching for.
	 * @param start index to start the search from. If positive start from that index,
	 * 			if negative start from relative to end of array where -1 is the last valid index.
	 * @param nth nth occurrence of the matched value at or after the <code>start</code> if
	 * 			positive, or at or before if negative. A 0 <code>nth</code> immediately fails.
	 * @return index of match or -1 if failed
	 */
	public int indexOfElements(PrimArray other, int otherCount, int otherStart, int start, int nth) {
		if (count() == 0 || nth == 0) {
			return -1;
		}
		int otherN;
		if (otherCount < 0) {
			otherN = other.count();
		} else {
			if (otherCount > other.count()) {
				throw new IndexOutOfBoundsException();
			}
			otherN = otherCount;
		}
		int result;
		if (start >= 0) {
			result = start;
		} else {
			result = count() + start;
		}
		int n = nth < 0 ? -nth : nth; //TODO = Math.abs(nth);
		boolean forward = nth > 0;
		for (;;) {
			{
				if (forward ? (result > count() - otherN) : result < 0) {
					return -1;
				}
				if (elementsEqual(result, other, otherStart, otherN)) {
					n -= 1;
					if (n == 0) {
						return result;
					}
				}
				if (forward) {
					result += 1;
				} else {
					result -= 1;
				}
			}
		}
	}

	/**
	 * Return the index of the first occurrence of the given sequence
	 * of values of <code>other</code> at or after the given index
	 * index in this, or -1 if there is none. Negative numbers for <code>start</code> are
	 * relative to the end of the array.
	 * 
	 * @param other array of elements that is being searched for in this.
	 * @param otherCount number of elements from <code>other</code> that is being
	 * 	searched for, or -1 to include all elements after <code>otherStart</code>.
	 * @param otherStart index of first element of <code>other</code> to start searching for.
	 * @param start index to start the search from. If positive start from that index,
	 * 			if negative start from relative to end of array where -1 is the last valid index.
	 * @return index of match or -1 if failed
	 */
	public int indexOfElements(PrimArray other, int otherCount, int otherStart, int start) {
		return indexOfElements(other, otherCount, otherStart, start, 1);
	}

	/**
	 * Return the index of the first occurrence of the given sequence
	 * of values of <code>other</code> in <code>this</code>, or -1 if there is none.at or after (before if <code>nth</code> is negative) the given
	 * 
	 * @param other array of elements that is being searched for in this.
	 * @param otherCount number of elements from <code>other</code> that is being
	 * 	searched for, or -1 to include all elements after <code>otherStart</code>.
	 * @param otherStart index of first element of <code>other</code> to start searching for.
	 * @return index of match or -1 if failed
	 */
	public int indexOfElements(PrimArray other, int otherCount, int otherStart) {
		return indexOfElements(other, otherCount, otherStart, 0);
	}

	/**
	 * Return the index of the first occurrence of the given sequence
	 * of values of <code>other</code> in <code>this</code>, or -1 if there is none.at or after (before if <code>nth</code> is negative) the given
	 * 
	 * @param other array of elements that is being searched for in this.
	 * @param otherCount number of elements from <code>other</code> that is being
	 * 	searched for, or -1 to include all elements.
	 * @return index of match or -1 if failed
	 */
	public int indexOfElements(PrimArray other, int otherCount) {
		return indexOfElements(other, otherCount, 0);
	}

	/**
	 * Return the index of the first occurrence of <code>other</code> in this 
	 * or -1 if there is none. 
	 * 
	 * @param other array of elements that is being searched for in this.
	 * @return index of match or -1 if failed
	 */
	public int indexOfElements(PrimArray other) {
		return indexOfElements(other, -1);
	}

	//////////////////////////////////////////////
	// Printing

	public void printOn(PrintWriter oo) {
		if (count() == 0) {
			oo.print("[empty");
		} else {
			String before = "[";
			for (int i = 0; i < count(); i += 1) {
				oo.print(before);
				printElementOn(i, oo);
				before = " ";
			}
		}
		oo.print("]");
	}

	/**
	 * Print a representation of the element at the given <code>index</code>. 
	 * 
	 * @param index index of element to be printed.
	 * @param oo print stream to write element representation to.
	 */
	protected void printElementOn(int index, PrintWriter oo) {
		Heaper value = fetchValue(index);
		if (value != null) {
			value.printOn(oo);
		} else {
			oo.print("null");
		}
	}

	//////////////////////////////////////////////
	// Helper methods

	public void zeroElements(int from, int count) {
		Heaper zeroElement = zeroElement();
		storeAll(zeroElement, count, from);
	}

	public void zeroElements(int from) {
		zeroElements(from, -1);
	}

	public void zeroElements() {
		zeroElements(0);
	}

	/**
	 * Return a suitable immutable zero element for this type of array; either 0 or null.
	 * 
	 * @return zero element for this type of array
	 */	
	protected abstract Heaper zeroElement();
	
	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
//		System.out.println("Ignor sendSelfTo() for: "+this);
//		throw new UnsupportedOperationException();
	}

}
