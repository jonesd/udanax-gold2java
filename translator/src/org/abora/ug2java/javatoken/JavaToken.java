/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ug2java.javatoken;

public class JavaToken {
	//fixme:
	public String value = "";

	public JavaToken() {
		super();
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

	public void write(StringBuffer buffer) {
		buffer.append(value);
	}

	protected void writeLeadingSpaceIfRequired(StringBuffer buffer) {
		if (buffer.length() > 0 && !Character.isWhitespace(buffer.charAt(buffer.length() - 1))) {
			buffer.append(' ');
		}
	}
}