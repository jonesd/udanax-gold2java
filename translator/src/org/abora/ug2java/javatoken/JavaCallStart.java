/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.javatoken;

import org.abora.ug2java.writer.JavaWriter;

public class JavaCallStart extends JavaToken {

	public JavaCallStart(String value) {
		super(value);
	}

	public void write(JavaWriter buffer) {
		// handling required after this identifires have been trimmed
		char c = buffer.getLastCharacter();
		if (Character.isJavaIdentifierPart(c) || c == ')' || c == '"') {
			buffer.append(".");
		}
		super.write(buffer);
		buffer.append("(");
	}
}