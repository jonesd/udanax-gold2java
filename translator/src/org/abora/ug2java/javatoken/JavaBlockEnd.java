/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ug2java.javatoken;

import org.abora.ug2java.ClassWriter;

public class JavaBlockEnd extends JavaToken {

	public JavaBlockEnd() {
		super();
	}

	public void write(StringBuffer buffer) {
		buffer.append("}" + ClassWriter.lineSeparator());
	}
}