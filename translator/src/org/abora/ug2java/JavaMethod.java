/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java;



public class JavaMethod extends JavaClassElement {
	public String modifiers;
	public String returnType;
	public MethodBody methodBody;
	public String name;
	public String params;
	public SmalltalkSource smalltalkSource;
	public String comment;
	public JavaClass javaClass;
}
