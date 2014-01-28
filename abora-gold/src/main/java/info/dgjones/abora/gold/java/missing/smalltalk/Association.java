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

import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.xcvr.Recipe;


public class Association {

	private String key = null;
	private Object value = null;
	
	public Association() {
		super();
	}
	
	public Association(String key, Object value) {
		super();
		this.key = key;
		this.value = value;
	}

	public String key() {
		return key;
	}

	public Object value() {
		return value;
	}

	public Recipe refValue() {
		return (Recipe)value;
	}

	public void refAssign(Recipe recipe) {
		value = recipe;
	}

	public void setKey(String string) {
		if (key != null) {
			throw new AboraRuntimeException("Association key already set");
		}
		key = string;
	}
	
	public String toString() {
		return getClass().getName()+"("+key()+","+value()+")";
	}

}
