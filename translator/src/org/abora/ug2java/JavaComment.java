/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ug2java;

public class JavaComment extends JavaToken {

	public JavaComment(String value) {
		super(value);
	}

	public void write(StringBuffer buffer) {
		if (buffer.length() > 0 && buffer.charAt(buffer.length() - 1) != '\n') {
			buffer.append(ClassWriter.lineSeparator());
		}
		buffer.append("/* ");
		super.write(buffer);
		buffer.append(" */" + ClassWriter.lineSeparator());
	}
}