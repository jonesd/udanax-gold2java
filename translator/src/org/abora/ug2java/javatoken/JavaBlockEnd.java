/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.javatoken;

import org.abora.ug2java.writer.JavaWriter;

public class JavaBlockEnd extends JavaToken {

	public JavaBlockEnd() {
		super();
	}

	public void write(JavaWriter buffer) {
		buffer.decrease();
		buffer.append("}");
		buffer.newLine();
	}
}