/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java;

import org.abora.ug2java.util.ToStringGenerator;




public class JavaField {
	public String name;
	public String type;
	public String modifiers;

	public JavaField(String type, String name) {
		this("", type, name);
	}

	public JavaField(String modifiers, String type, String name) {
		this.name = name;
		this.type = type;
		this.modifiers = modifiers;
	}
	
	public String toString() {
		ToStringGenerator generator = new ToStringGenerator(this);
		generator.add(modifiers+" "+type+" "+name);
		return generator.end();
	}

	public JavaClass getJavaClass(JavaCodebase javaCodebase) {
		return javaCodebase.getJavaClass(type);
	}
}
