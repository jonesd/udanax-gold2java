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
package info.dgjones.abora.gold.java.missing.smalltalk;

import java.util.ArrayList;
import java.util.List;


public class Array {

	private final List contents;
	
	public Array() {
		this(10);
	}
	
	public Array(int size) {
		contents = new ArrayList(size);
	}

	//TODO sort out this Array vs IntArray madness
	public static IntArray newWithAll(int i, int i1) {
		throw new UnsupportedOperationException();
	}

	public int size() {
		return contents.size();
	}
	
	public static Array with(Object a) {
		Array array = new Array();
		array.put(1, a);
		return array;
	}

	public static Array with(Object a, Object b) {
		Array array = new Array();
		array.put(1, a);
		array.put(2, b);
		return array;
	}

	/**
	 * @param i1 one-based index
	 */
	public Object at(int i1) {
		return contents.get(i1-1);
	}
	
	/**
	 * @param i1 one-based index
	 */
	public void put(int i1, Object v) {
		int i0 = i1 -1;
		if (i0 == contents.size()) {
			contents.add(v);
		} else {
			contents.set(i0, v);
		}
	}

	public void basicAtPut(int i, Object arg1) {
		put(i, arg1);
	}

}
