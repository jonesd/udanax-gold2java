/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.javatoken;

import org.abora.ug2java.JavaWriter;

public class JavaKeyword extends JavaToken {

	public JavaKeyword(String value) {
		super(value);
	}

	public void write(JavaWriter buffer) {
		buffer.writeLeadingSpaceIfRequired();
		super.write(buffer);
		buffer.append(" ");
	}
}