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



public class OrderedCollection {
	
	private final List contents;

	public OrderedCollection() {
		this(10);
	}
	
	public OrderedCollection(int c) {
		super();
		contents = new ArrayList(c);
	}
	
	public static OrderedCollection with(Object a, Object b) {
		throw new UnsupportedOperationException();
	}

	public void add(Object heaper) {
		contents.add(heaper);
	}

	public int size() {
		return contents.size();
	}
	
	/**
	 * @param index one-based index
	 */
	public Object get(int index) {
		return contents.get(index);
	}

	public void addAll(OrderedCollection collection) {
		for (int i = 0; i < collection.size(); i++) {
			Object element = collection.get(i);
			add(element);
		}
	}

}
