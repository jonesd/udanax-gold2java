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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class Fn extends Heaper {

	protected Method method;
	protected String selector;
	
	protected Fn() {
		throw new UnsupportedOperationException();
	}
	
	public Fn(Category category, String selector) {
		method = findMethod(category, selector);
		this.selector = selector;
	}
	
	protected Method findMethod(Category category, String selector) {
		return findMethod(category.brotherClass().getJavaClass(), selector);
	}
	
	protected Method findMethod(Class c, String selector) {
		Method foundMethod = null;
		for (int i = 0; i < c.getMethods().length; i++) {
			Method m = c.getMethods()[i];
			if (isMatchingMethod(m, selector)) {
				if (foundMethod == null) {
					foundMethod = m;
				} else {
					throw new IllegalArgumentException("Duplicate matching methods for selector: "+selector+" in class: "+c);
				}
			}
		}
		if (foundMethod != null) {
			return foundMethod;
		} else {
			throw new IllegalArgumentException("No matching methods for selector: "+selector+" in class: "+c);			
		}
	}

	protected boolean isMatchingMethod(Method m, String selector) {
		return m.getName().equals(selector);
	}
	
	public Fn(Rcvr rcvr) {
		throw new UnsupportedOperationException();
	}

	public String staticClass() {
		throw new UnsupportedOperationException();
	}

	public String selector() {
		return selector;
	}
	
	protected Object invokeStaticWith(Object a) {
		Object[] arguments = new Object[] {a};
		return invokeStaticWith(arguments);
	}

	protected Object invokeStaticWith(Object[] arguments) {
		try {
			return method.invoke(null, arguments);
		} catch (IllegalArgumentException e) {
			throw new AboraRuntimeException("Fn invoke with illegal argument: "+e);
		} catch (IllegalAccessException e) {
			throw new AboraRuntimeException("Fn invoke with illegal access: "+e);
		} catch (InvocationTargetException e) {
			if (e.getCause() != null && e.getCause() instanceof RuntimeException) {
				throw (RuntimeException)e.getCause();
			} else {
				throw new AboraRuntimeException("Fn invoke with illegal invocation target exception: "+e);
			}
		}
	}

}
