/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ug2java;

public class JavaBlockStart extends JavaToken {

	public JavaBlockStart() {
		super();
	}

	public void write(StringBuffer buffer) {
		writeLeadingSpaceIfRequired(buffer);
		buffer.append("{" + ClassWriter.lineSeparator());
	}
}