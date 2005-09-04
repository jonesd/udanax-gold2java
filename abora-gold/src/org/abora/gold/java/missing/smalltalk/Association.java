/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing.smalltalk;

import org.abora.gold.java.exception.AboraRuntimeException;
import org.abora.gold.xcvr.Recipe;

import com.sun.tools.doclets.standard.ClassSubWriter;


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
