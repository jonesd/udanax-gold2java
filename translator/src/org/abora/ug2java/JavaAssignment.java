/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ug2java;

public class JavaAssignment extends JavaToken {

	public JavaAssignment() {
		super();
	}

	public void write(StringBuffer buffer) {
		buffer.append(" = ");
	}
}