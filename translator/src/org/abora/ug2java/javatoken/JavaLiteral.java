/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.javatoken;

public abstract class JavaLiteral extends JavaToken {

	public JavaLiteral(String value) {
		super(value);
	}

	public boolean isCharacter() {
		// TODO what to really do here?
		return value.startsWith("\'") && value.endsWith("\'");
	}
}