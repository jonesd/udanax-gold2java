/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.javatoken;

import org.abora.ug2java.JavaWriter;

public class JavaToken {
	//FIXME:
	public String value;

	public JavaToken() {
		this("");
	}

	public JavaToken(String value) {
		super();

		if (value == null) {
			throw new IllegalArgumentException("Must set value of JavaToken");
		}
		this.value = value;
	}

	public String toString() {
		String className = getClass().getName();
		className = className.substring(className.lastIndexOf('.') + 1);
		return className + "[" + value + "]";
	}

	public void write(JavaWriter buffer) {
		buffer.append(value);
	}

}