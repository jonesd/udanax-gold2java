/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.javatoken;

import org.abora.ug2java.JavaWriter;

public class JavaBlockStart extends JavaToken {

	public JavaBlockStart() {
		super();
	}

	public void write(JavaWriter buffer) {
		buffer.writeLeadingSpaceIfRequired();
		buffer.append("{");
		buffer.increase();
		buffer.newLine();
	}
}