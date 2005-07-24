package org.abora.gold.java;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.abora.gold.java.exception.AboraRuntimeException;
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
