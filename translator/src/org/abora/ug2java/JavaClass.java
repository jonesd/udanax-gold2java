/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */

package org.abora.ug2java;

import java.io.File;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Vector;

import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.JavaType;

public class JavaClass {
	public String className;
	public String superclassName;
	public String classCategory;
	public String comment;
	public Vector classQuotes = new Vector();
	public Vector instanceMethods = new Vector();
	public Vector classMethods = new Vector();
	final List fields = new ArrayList();
	final List methodBodies = new ArrayList();

	SortedSet importedPackages = new TreeSet();
	public Hashtable packageLookup = new Hashtable();

	static final String PACKAGE_SEPARATOR = ".";

	/**
	 * ClassWriter constructor comment.
	 */
	public JavaClass(Hashtable packageLookup) {
		super();
		this.packageLookup = packageLookup;
	}

	protected String getPackage() {
		return classCategory;
	}

	protected String getPackageDirectory() {
		return classCategory.replaceAll("\\.", File.separator);
	}

	public void includeImportForType(String type) {
		String importPackage = (String) packageLookup.get(type);
		if (importPackage != null) {
			importedPackages.add(importPackage + "." + type);
		}
	}

	public static String lineSeparator() {
		return System.getProperty("line.separator");
	}




	protected void includeAnyReferencedTypes(MethodBody body) {
		List tokens = body.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.get(i);
			if ((token instanceof JavaIdentifier || token instanceof JavaType || token instanceof JavaCast)
				&& Character.isJavaIdentifierStart(token.value.charAt(0))) {
				includeImportForType(token.value);
			}
		}
	}


	public List getFields() {
		return fields;
	}
	
	public List getMethodBodies() {
		return methodBodies;
	}
}