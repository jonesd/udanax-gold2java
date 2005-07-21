package org.abora.gold.java;

import java.lang.reflect.Method;

import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xpp.basic.Category;
import org.abora.gold.xpp.basic.Heaper;

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
			if (m.getName().equals(selector)) {
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
	
	public Fn(Rcvr rcvr) {
		throw new UnsupportedOperationException();
	}

	public String staticClass() {
		throw new UnsupportedOperationException();
	}

	public String selector() {
		return selector;
	}

}
