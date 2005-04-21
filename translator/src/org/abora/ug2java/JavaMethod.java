/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;



public class JavaMethod extends JavaClassElement {
	public String modifiers = "";
	public String returnType;
	public MethodBody methodBody;
	public String name;
	public SmalltalkSource smalltalkSource;
	public String comment;
	public JavaClass javaClass;
	public List localVariables = new ArrayList();
	public List parameters = new ArrayList();
	public boolean shouldInclude = true;
	public boolean isDeprecated = false;
	
	public JavaMethod() {
		super();
	}
	
	public JavaMethod(String returnType, String name) {
		this.returnType = returnType;
		this.name = name;
	}
	
	public String findTypeOfVariable(String variableName) {
		String type = findTypeOfVariable(variableName, localVariables);
		if (type == null) {
			type = findTypeOfVariable(variableName, parameters);
		}
		if (type == null) {
			type = javaClass.findTypeOfVariable(variableName); 
		}
		return type;
	}

	private String findTypeOfVariable(String variableName, List javaFields) {
		for (Iterator iter = javaFields.iterator(); iter.hasNext();) {
			JavaField javaField = (JavaField) iter.next();
			if (javaField.name.equals(variableName)) {
				return javaField.type;
			}
		}
		return null;
	}

	public JavaCodebase getJavaCodebase() {
		return javaClass.getJavaCodebase();
	}

	public void addLocalVariable(JavaField field) {
		localVariables.add(field);
	}

	public boolean isStatic() {
		//TODO lame implementation...
		return modifiers.indexOf("static") != -1;
	}
	
}
