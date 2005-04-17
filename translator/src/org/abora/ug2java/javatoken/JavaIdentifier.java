/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.javatoken;

import org.abora.ug2java.writer.JavaWriter;

public class JavaIdentifier extends JavaToken {

	public JavaIdentifier(String value) {
		super(value);
	}

	public void write(JavaWriter buffer) {
		//TODO dubious - might want to add variable reference token?
		char c = buffer.getLastCharacter();
		if (Character.isJavaIdentifierPart(c)) {
			buffer.append(".");
		}
		super.write(buffer);
	}

}