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

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class PtrArray extends PrimArray {
	protected final Heaper[] storage;

	public static void initializeClassAttributes() {
		//TODO just made up out of thin air - totally wrong!!!
		AboraSupport.findAboraClass(PtrArray.class).setAttributes( new Set().add("CONCRETE").add("PSEUDOCOPY"));
	}

	//////////////////////////////////////////////
	// Constructors

	protected PtrArray(int count) {
		super();
		storage = new Heaper[count];
	}
	
	public PtrArray(Rcvr rcvr) {
		super(rcvr);
		int count = rcvr.receiveUInt32();
		storage = new Heaper[count];
		for (int i = 0; i < storage.length; i++) {
			store(i, rcvr.receiveHeaper());
		}
	}


	protected PtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected PtrArray(Heaper[] source) {
		this(source.length);
		System.arraycopy(source, 0, storage, 0, source.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create a PtrArray filled with NULLs */
	public static PtrArray make(int count) {
		return new PtrArray(count);
	}

	/** create a PtrArray filled with the indicated data in 'from' */
	public static PtrArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new PtrArray(size, from, sourceOffset, count, destOffset);
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
		return new PtrArray(buffer);
	}

	/** create a zero size PtrArray */
	public static PtrArray empty() {
		//TODO cache empty array
		return make(0);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PtrArray) source, sourceOffset, count, destOffset);
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

	//////////////////////////////////////////////
	// Accessing

	public void store(int index, Heaper pointer) {
		storage[index] = pointer;
	}

	/**
	 * Retrieve a single element from the array. Does array bounds checking.
	 * BLAST if NULL
	 */
	public Heaper get(int index) {
		//TODO how does this differ from getValue?
		Heaper result = fetch(index);
		if (result == null) {
			throw new IllegalArgumentException("NullEntry");
		}
		return result;
		//		INLINE RPTR(Heaper) PtrArray::get (Int32 index) {
		//			WPTR(Heaper) result = this->fetch(index);
		//			if (result == NULL) {
		//			PtrArray::nullEntry();
		//			}
		//			return result;
		//		}
	}

	/**
	 * Retrieve a single element from the array. Does array bounds checking.
	 * Non-pointer arrays box up the contents in a PrimValue object.
	 */
	public Heaper fetch(int index) {
		//TODO how does this differ from fetchValue?
		return storage[index];
		//		INLINE RPTR(Heaper) OR(NULL) PtrArray::fetch (Int32 index){
		//			return ((Heaper**)this->storage())[this->rangeCheck (index)];
		//		}
	}

	public void storeValue(int index, Heaper value) {
		store(index, value);
		//		void PtrArray::storeValue (Int32 index, APTR(Heaper) OR(NULL) value){
		//			this->store(index, value);
		//		}
	}

	public Heaper fetchValue(int index) {
		return fetch(index);
		//		RPTR(Heaper) OR(NULL) PtrArray::fetchValue (Int32 index) {
		//			return this->fetch(index);
		//		}
	}

	public PrimSpec spec() {
		return PrimSpec.pointer();
	}

	public int count() {
		return storage.length;
	}

	//////////////////////////////////////////////
	// Comparing and Hashing

	public boolean contentsEQ(PtrArray other) {
		if (count() != other.count()) {
			return false;
		}
		PtrArray o = (PtrArray) other;
		for (int i = 0; i < count(); i += 1) {
			if (fetch(i) != o.fetch(i)) {
				return false;
			}
		}
		return true;
		//		BooleanVar PtrArray::contentsEQ (APTR(PtrArray) other){
		//			if (this->count() != other->count()) {
		//			return FALSE;
		//			}
		//			SPTR(PtrArray) o = CAST(PtrArray,other);
		//			for (Int32 i = 0; i < this->count(); i += 1) {
		//			if (this->unsafeFetch(i) != o->unsafeFetch(i)) {
		//				return FALSE;
		//			}
		//			}
		//			return TRUE;
		//		}
	}

	public boolean contentsEqual(PrimArray other) {
		PtrArray o = (PtrArray) other;

		if (count() != other.count()) {
			return false;
		}
		for (int i = 0; i < count(); i += 1) {
			Heaper a = fetch(i);
			Heaper b = o.fetch(i);

			if (!(a == b || a != null && b != null && a.isEqual(b))) {
				return false;
			}
		}
		return true;
		//		BooleanVar PtrArray::contentsEqual (APTR(PrimArray) other){
		//			SPTR(PtrArray) o = CAST(PtrArray,other);
		//
		//			if (this->count() != other->count()) {
		//			return FALSE;
		//			}
		//			for (Int32 i = 0; i < this->count(); i += 1) {
		//			SPTR(Heaper) a;
		//			SPTR(Heaper) b;
		//	
		//			if (!((a = this->unsafeFetch(i)) == (b = o->unsafeFetch(i))
		//				  || a != NULL && b != NULL && a->isEqual(b))) {
		//				  return FALSE;
		//			}
		//			}
		//			return TRUE;
		//		}
	}

	public int contentsHash() {
		int result = count() * 43;
		for (int i = 0; i < count(); i += 1) {
			Heaper p = fetch(i);

			if (p != null) {
				result ^= p.hashForEqual();
			}
		}
		return result;
		//		UInt32 PtrArray::contentsHash (){
		//			UInt32 result;
		//
		//			result = this->count() * 43;
		//			for (Int32 i = 0; i < this->count(); i += 1) {
		//			SPTR(Heaper) p;
		//
		//			if ((p = this->unsafeFetch(i)) != NULL) {
		//				result ^= p->hashForEqual();
		//			}
		//			}
		//			return result;
		//		}
	}

	public boolean elementsEQ(int here, PrimArray other, int there, int count) {
		PtrArray o = (PtrArray) other;

		int n = Math.min(other.count() - there, count() - here);
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		for (int i = 0; i < n; i += 1) {
			if (fetch(here + i) != o.fetch(there + i)) {
				return false;
			}
		}
		return true;
		//		BooleanVar PtrArray::elementsEQ (Int32 here,
		//						 PrimArray * other,
		//						 Int32 there/* = Int32Zero*/,
		//						 Int32 count/* = -1*/)
		//		{
		//			Int32 n;
		//			SPTR(PtrArray) o = CAST(PtrArray,other);
		//
		//			n = min(other->count() - there, this->count() - here);
		//			if (count > n) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//			if (count >= 0) {
		//			n = count;
		//			}
		//			for (Int32 i = 0; i < n; i += 1) {
		//			if (this->unsafeFetch(here + i) != o->unsafeFetch(there + i)) {
		//				return FALSE;
		//			}
		//			}
		//			return TRUE;
		//		}
		//
	}

	public boolean elementsEQ(int here, PrimArray other, int there) {
		return elementsEQ(here, other, there, -1);
	}

	public boolean elementsEQ(int here, PrimArray other) {
		return elementsEQ(here, other, 0);
	}

	public boolean elementsEqual(int here, PrimArray other, int there, int count) {
		PtrArray o = (PtrArray) other;

		int n = Math.min(other.count() - there, count() - here);
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		for (int i = 0; i < n; i += 1) {
			Heaper a = fetch(here + i);
			Heaper b = o.fetch(there + i);

			if (!(a == b || a != null && b != null && a.isEqual(b))) {
				return false;
			}
		}
		return true;
		//		BooleanVar PtrArray::elementsEqual (Int32 here,
		//							APTR(PrimArray) other,
		//							Int32 there/* = Int32Zero*/,
		//							Int32 count/* = -1*/)
		//		{
		//			Int32 n;
		//			SPTR(PtrArray) o = CAST(PtrArray,other);
		//
		//			n = min(other->count() - there, this->count() - here);
		//			if (count > n) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//			if (count >= 0) {
		//			n = count;
		//			}
		//			for (Int32 i = 0; i < n; i += 1) {
		//			SPTR(Heaper) a;
		//			SPTR(Heaper) b;
		//
		//			if (!((a = this->unsafeFetch(here + i)) ==
		//				  (b = o->unsafeFetch(there + i))
		//				  || a != NULL && b != NULL && a->isEqual(b))) {
		//				return FALSE;
		//			}
		//			}
		//			return TRUE;
		//		}
	}

	public int elementsHash(int count, int start) {
		int result = 0;
		int n = count == -1 ? count() - start : count;
		if (start < 0 || n + start > count()) {
			throw new IndexOutOfBoundsException();
		}
		for (int i = 0; i < n; i++) {
			result ^= fetch(i + start).hashForEqual();
		}
		return result;
		//		UInt32 PtrArray::elementsHash(Int32 count/* = -1*/,
		//						  Int32 start/* = Int32Zero*/)
		//		{
		//			UInt32 result = 0;
		//			Int32 n = count == -1 ? this->count() - start : count;
		//			if (start < 0 || n + start > this->count()) {
		//				BLAST(IndexOutOfBounds);
		//			}
		//			for (Int32 i = 0; i < n; i++) {
		//			result ^= this->unsafeFetch(i + start)->hashForEqual();
		//			}
		//			return result;
		//		}
	}


	//////////////////////////////////////////////
	// Finding

	public int indexOf(Heaper value, int start, int nth) {
		if (count() == 0 || nth == 0) {
			return -1;
		}
		if (start < 0) {
			start = count() + start;
		}
		if (start < 0 || start >= count()) {
			throw new IndexOutOfBoundsException();
		}

		if (value != null) {
			if (nth >= 0) {
				for (int idx = start; idx < count(); idx += 1) {
					Heaper ptr = fetch(idx);
					if (ptr == value || (ptr != null && value != null && ptr.isEqual(value))) {
						nth -= 1;
						if (nth == 0) {
							return idx;
						}
					}
				}
			} else {
				for (int idx = start; idx >= 0; idx -= 1) {
					Heaper ptr = fetch(idx);
					if (ptr == value || (ptr != null && value != null && ptr.isEqual(value))) {
						nth += 1;
						if (nth == 0) {
							return idx;
						}
					}
				}
			}
		} else {
			if (nth >= 0) {
				int count = count();
				for (int idx = start; idx < count; idx += 1) {
					Heaper ptr = fetch(idx);
					if (ptr == null) {
						nth -= 1;
						if (nth == 0) {
							return idx;
						}
					}
				}
			} else {
				for (int idx = start; idx >= 0; idx -= 1) {
					Heaper ptr = fetch(idx);
					if (ptr == null) {
						nth += 1;
						if (nth == 0) {
							return idx;
						}
					}
				}
			}
		}
		return -1;
		//		Int32 PtrArray::indexOf (APTR(Heaper) value, 
		//					 Int32 start/* = Int32Zero*/,
		//					 Int32 nth/* = 1*/)
		//		{
		//			if (this->count() == 0 || nth == 0) {
		//			return -1;
		//			}
		//			if (start < 0) {
		//			start = this->count () + start;
		//			}
		//			if (start < 0 || start >= this->count ()) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//
		//			if (value != NULL) {
		//			if (nth >= 0) {
		//				for (Int32 idx = start; idx < this->count(); idx++) {
		//				WPTR(Heaper) ptr = this->unsafeFetch(idx);
		//				if (ptr == value || (ptr && value && ptr->isEqual (value))) {
		//					nth--;
		//					if (nth == 0) {
		//					return idx;
		//					}
		//				}
		//				}
		//			} else {
		//				for (Int32 idx = start; idx >= 0; idx--) {
		//				WPTR(Heaper) ptr = this->unsafeFetch(idx);
		//				if (ptr == value || (ptr && value && ptr->isEqual (value))) {
		//					nth++;
		//					if (nth == 0) {
		//					return idx;
		//					}
		//				}
		//				}
		//			}
		//			} else {
		//			Heaper ** pp = (Heaper**) this->storage() + start;
		//			if (nth >= 0) {
		//				Int32 count = this->count();
		//				for (Int32 idx = start; idx < count; idx++) {
		//				WPTR(Heaper) ptr = *pp++;
		//				if (ptr == NULL) {
		//					nth--;
		//					if (nth == 0) {
		//					return idx;
		//					}
		//				}
		//				}
		//			} else {
		//				for (Int32 idx = start; idx >= 0; idx--) {
		//				WPTR(Heaper) ptr = *pp--;
		//				if (ptr == NULL) {
		//					nth++;
		//					if (nth == 0) {
		//					return idx;
		//					}
		//				}
		//				}
		//			}
		//			}
		//			return -1;
		//		}
	}

	public int indexPast(Heaper value, int start, int nth) {
		if (count() == 0 || nth == 0) {
			return -1;
		}
		if (start < 0) {
			start = count() + start;
		}
		if (start < 0 || start >= count()) {
			throw new IndexOutOfBoundsException();
		}

		if (value != null) {
			if (nth >= 0) {
				for (int idx = start; idx < count(); idx += 1) {
					Heaper ptr = fetch(idx);
					if (!(ptr == value || (ptr != null && ptr.isEqual(value)))) {
						nth -= 1;
						if (nth == 0) {
							return idx;
						}
					}
				}
			} else {
				for (int idx = start; idx >= 0; idx -= 1) {
					Heaper ptr = fetch(idx);
					if (!(ptr == value || (ptr != null && ptr.isEqual(value)))) {
						nth += 1;
						if (nth == 0) {
							return idx;
						}
					}
				}
			}
		} else {
			if (nth >= 0) {
				for (int idx = start; idx < count(); idx += 1) {
					Heaper ptr = fetch(idx);
					if (ptr != null) {
						nth -= 1;
						if (nth == 0) {
							return idx;
						}
					}
				}
			} else {
				for (int idx = start; idx >= 0; idx -= 1) {
					Heaper ptr = fetch(idx);
					if (ptr != null) {
						nth += 1;
						if (nth == 0) {
							return idx;
						}
					}
				}
			}
		}
		return -1;
		//		Int32 PtrArray::indexPast (APTR(Heaper) value, 
		//					   Int32 start/* = Int32Zero*/,
		//					   Int32 nth/* = 1*/)
		//		{
		//			if (this->count() == 0 || nth == 0) {
		//			return -1;
		//			}
		//			if (start < 0) {
		//			start = this->count () + start;
		//			}
		//			if (start < 0 || start >= this->count ()) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//
		//			if (value != NULL) {
		//			if (nth >= 0) {
		//				for (Int32 idx = start; idx < this->count(); idx++) {
		//				WPTR(Heaper) ptr = this->unsafeFetch(idx);
		//				if (! (ptr == value || (ptr && ptr->isEqual (value)))) {
		//					nth--;
		//					if (nth == 0) {
		//					return idx;
		//					}
		//				}
		//				}
		//			} else {
		//				for (Int32 idx = start; idx >= 0; idx--) {
		//				WPTR(Heaper) ptr = this->unsafeFetch(idx);
		//				if (! (ptr == value || (ptr && ptr->isEqual (value)))) {
		//					nth++;
		//					if (nth == 0) {
		//					return idx;
		//					}
		//				}
		//				}
		//			}
		//			} else {
		//			Heaper ** pp = (Heaper**) this->storage() + start;
		//			if (nth >= 0) {
		//				for (Int32 idx = start; idx < this->count(); idx++) {
		//				WPTR(Heaper) ptr = *pp++;
		//				if (ptr) {
		//					nth--;
		//					if (nth == 0) {
		//					return idx;
		//					}
		//				}
		//				}
		//			} else {
		//				for (Int32 idx = start; idx >= 0; idx--) {
		//				WPTR(Heaper) ptr = *pp--;
		//				if (ptr) {
		//					nth++;
		//					if (nth == 0) {
		//					return idx;
		//					}
		//				}
		//				}
		//			}
		//			}
		//			return -1;
		//		}
	}

	public int indexOfEQ(Heaper value, int start, int nth) {
		if (count() == 0 || nth == 0) {
			return -1;
		}
		if (start < 0) {
			start = count() + start;
		}
		if (start < 0 || start >= count()) {
			throw new IndexOutOfBoundsException();
		}

		if (nth >= 0) {
			for (int idx = start; idx < count(); idx += 1) {
				Heaper ptr = fetch(idx);
				if (ptr == value) {
					nth -= 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		} else {
			for (int idx = start; idx >= 0; idx -= 1) {
				Heaper ptr = fetch(idx);
				if (ptr == value) {
					nth += 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		}
		return -1;
		//		Int32 PtrArray::indexOfEQ (APTR(Heaper) value, 
		//					   Int32 start/* = Int32Zero*/,
		//					   Int32 nth/* = 1*/)
		//		{
		//			if (this->count() == 0 || nth == 0) {
		//			return -1;
		//			}
		//			if (start < 0) {
		//			start = this->count () + start;
		//			}
		//			if (start < 0 || start >= this->count ()) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//
		//			Heaper ** pp = (Heaper**) this->storage() + start;
		//			if (nth >= 0) {
		//			for (Int32 idx = start; idx < this->count(); idx++) {
		//				WPTR(Heaper) ptr = *pp++;
		//				if (ptr == value) {
		//				nth--;
		//				if (nth == 0) {
		//					return idx;
		//				}
		//				}
		//			}
		//			} else {
		//			for (Int32 idx = start; idx >= 0; idx--) {
		//				WPTR(Heaper) ptr = *pp--;
		//				if (ptr == value) {
		//				nth++;
		//				if (nth == 0) {
		//					return idx;
		//				}
		//				}
		//			}
		//			}
		//			return -1;
		//		}
	}

	public int indexOfEQ(Heaper value, int start) {
		return indexOfEQ(value, start, 1);
	}

	public int indexOfEQ(Heaper value) {
		return indexOfEQ(value, 0);
	}

	public int indexOfEQOrNull(Heaper value, int start, int nth) {
		if (count() == 0 || nth == 0) {
			return -1;
		}
		if (start < 0) {
			start = count() + start;
		}
		if (start < 0 || start >= count()) {
			throw new IndexOutOfBoundsException();
		}

		if (nth >= 0) {
			for (int idx = start; idx < count(); idx += 1) {
				Heaper ptr = fetch(idx);
				if (ptr == value || ptr == null) {
					nth -= 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		} else {
			for (int idx = start; idx >= 0; idx -= 1) {
				Heaper ptr = fetch(idx);
				if (ptr == value || ptr == null) {
					nth += 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		}
		return -1;
		//		Int32 PtrArray::indexOfEQOrNull (APTR(Heaper) value, 
		//						 Int32 start/* = Int32Zero*/,
		//						 Int32 nth/* = 1*/)
		//		{
		//			if (this->count() == 0 || nth == 0) {
		//			return -1;
		//			}
		//			if (start < 0) {
		//			start = this->count () + start;
		//			}
		//			if (start < 0 || start >= this->count ()) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//
		//			Heaper ** pp = (Heaper**) this->storage() + start;
		//			if (nth >= 0) {
		//			for (Int32 idx = start; idx < this->count(); idx++) {
		//				WPTR(Heaper) ptr = *pp++;
		//				if (ptr == value || ptr == NULL) {
		//				nth--;
		//				if (nth == 0) {
		//					return idx;
		//				}
		//				}
		//			}
		//			} else {
		//			for (Int32 idx = start; idx >= 0; idx--) {
		//				WPTR(Heaper) ptr = *pp--;
		//				if (ptr == value || ptr == NULL) {
		//				nth++;
		//				if (nth == 0) {
		//					return idx;
		//				}
		//				}
		//			}
		//			}
		//			return -1;
		//		}
	}

	public int indexOfEQOrNull(Heaper value, int start) {
		return indexOfEQOrNull(value, start, 1);
	}

	public int indexOfEQOrNull(Heaper value) {
		return indexOfEQOrNull(value, 0);
	}

	public int indexPastEQ(Heaper value, int start, int nth) {
		if (count() == 0 || nth == 0) {
			return -1;
		}
		if (start < 0) {
			start = count() + start;
		}
		if (start < 0 || start >= count()) {
			throw new IndexOutOfBoundsException();
		}

		if (nth >= 0) {
			for (int idx = start; idx < count(); idx += 1) {
				Heaper ptr = fetch(idx);
				if (ptr == value) {
					nth -= 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		} else {
			for (int idx = start; idx >= 0; idx -= 1) {
				Heaper ptr = fetch(idx);
				if (ptr == value) {
					nth += 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		}
		return -1;
		//		Int32 PtrArray::indexPastEQ (APTR(Heaper) value, 
		//						 Int32 start/* = Int32Zero*/,
		//						 Int32 nth/* = 1*/)
		//		{
		//			if (this->count() == 0 || nth == 0) {
		//			return -1;
		//			}
		//			if (start < 0) {
		//			start = this->count () + start;
		//			}
		//			if (start < 0 || start >= this->count ()) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//
		//			Heaper ** pp = (Heaper**) this->storage() + start;
		//			if (nth >= 0) {
		//			for (Int32 idx = start; idx < this->count(); idx++) {
		//				WPTR(Heaper) ptr = *pp++;
		//				if (ptr == value) {
		//				nth--;
		//				if (nth == 0) {
		//					return idx;
		//				}
		//				}
		//			}
		//			} else {
		//			for (Int32 idx = start; idx >= 0; idx--) {
		//				WPTR(Heaper) ptr = *pp--;
		//				if (ptr == value) {
		//				nth++;
		//				if (nth == 0) {
		//					return idx;
		//				}
		//				}
		//			}
		//			}
		//			return -1;
		//		}
	}

	public int indexPastEQ(Heaper value, int start) {
		return indexPastEQ(value, start, 1);
	}

	public int indexPastEQ(Heaper value) {
		return indexPastEQ(value, 0);
	}


	//////////////////////////////////////////////
	// Bulk Storage

	public void storeAll(Heaper value, int count, int start) {
		int n = count() - start;
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (value == null && start == 0) {
			for (int i = 0; i < count(); i++) {
				store(i, null);
			}
			//			this->zeroElements (0, count);
			return;
		}
		if (count >= 0) {
			n = count;
		}
		for (int i = 0; i < n; i += 1) {
			store(start + i, value);
		}
		//		void PtrArray::storeAll (APTR(Heaper) value/* = NULL*/,
		//					 Int32 count/* = -1*/,
		//					 Int32 start/* = Int32Zero*/)
		//		{
		//			Int32 n;
		//
		//			n = this->count() - start;
		//			if (count > n) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//			if (value == NULL && start == Int32Zero) {
		//			for (Int32 i = 0; i < this->count(); i++) {
		//				this->store(i, NULL);
		//			}
		////			this->zeroElements (0, count);
		//			return;
		//			}
		//			if (count >= 0) {
		//			n = count;
		//			}
		//			for (Int32 i = 0; i < n; i += 1) {
		//			this->store(start + i, value);
		//			}
		//		}
	}

	public void copyToBuffer(Heaper[] buffer, int count, int start) {
		int n;
		if (count >= 0) {
			n = count;
		} else {
			n = count() - start;
		}
		if (n > buffer.length) {
			n = buffer.length;
		}
		System.arraycopy(storage, start, buffer, 0, n);

		//		void PtrArray::copyToBuffer (void * buffer,
		//						 Int32 size,
		//						 Int32 count /*= -1*/,
		//						 Int32 start /* = Int32Zero*/)
		//		{
		//			Int32 bufSize;
		//			Int32 n;
		//
		//			bufSize = size / sizeof(Heaper*);
		//			if (count >= 0) {
		//			n = count;
		//			} else {
		//			n = this->count() - start;
		//			}
		//			if (n > bufSize) {
		//			n = bufSize;
		//			}
		//			MEMMOVE (buffer, (Heaper**)this->storage() + start,
		//				 (int)(n * sizeof(Heaper*)));
		//		}
	}



	protected Heaper zeroElement() {
		return null;
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(fetch(index));
	}

	public static PtrArray nulls(int i) {
		return PtrArray.make(i);
	}

	public Heaper at(int estateIndex) {
		throw new UnsupportedOperationException();
	}

	public void put(int slot, Heaper value) {
		throw new UnsupportedOperationException();
	}

	//	/** for bulk methods that need checking and for migration */
	//	public void unsafeStore(int index, Heaper ptr) {
	//		throw new UnsupportedOperationException();
	//	}
	//
	//	/** for bulk methods that need checking and for migration */
	//	public Heaper "*" unsafeFetch(int index) {
	//		throw new UnsupportedOperationException();
	//	}

	//public void migrate(int[] destination, boolean destinationIsOld) {
	//	throw new UnsupportedOperationException();
	//}

	//private void nullEntry() {
	//	throw new UnsupportedOperationException();
	//}
	
	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendUInt32(count());
		for (int i = 0; i < storage.length; i++) {
			Heaper value = storage[i];
			xmtr.sendHeaper(value);
		}
	}

}
