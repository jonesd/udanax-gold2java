/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ug2java;

public class JavaCallStart extends JavaToken {

	public JavaCallStart(String value) {
		super(value);
	}

	public void write(StringBuffer buffer) {
		// handling required after this identifires have been trimmed
		char c;
		if (buffer.length() > 0 && (Character.isJavaIdentifierPart(c = buffer.charAt(buffer.length() - 1)) || c == ')')) {
			buffer.append(".");
		}
		super.write(buffer);
		buffer.append("(");
	}
}