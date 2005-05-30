/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */

package org.abora.ug2java;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Vector;

import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.util.ToStringGenerator;

public class JavaClass {
	public String className;
	public String superclassName;
	public String classCategory;
	public String comment;
	public Vector classQuotes = new Vector();
	public Vector instanceMethodChunks = new Vector();
	public Vector classMethodChunks = new Vector();
	public final List fields = new ArrayList();
	public final List methods = new ArrayList();
	public final JavaCodebase javaCodebase;
	public SortedSet importedPackages = new TreeSet();
	public final List staticBlocks = new ArrayList();

	static final String PACKAGE_SEPARATOR = ".";

	public JavaClass(String className, JavaCodebase javaCodebase) {
		this(className, null, javaCodebase);
	}
	
	public JavaClass(String className, String superclassName, JavaCodebase javaCodebase) {
		super();
		this.javaCodebase = javaCodebase;
		this.className = className;
		this.superclassName = superclassName;
		//TODO good form to add yourself directly to javaCodebase?
		javaCodebase.addJavaClass(this);
	}
	
	public JavaCodebase getJavaCodebase() {
		return javaCodebase;
	}

	public String findTypeOfVariable(String name) {
		for (Iterator iter = fields.iterator(); iter.hasNext();) {
			JavaField javaField = (JavaField) iter.next();
			if (javaField.name.equals(name)) {
				return javaField.type;
			}
		}
		return null;
	}
	
	public String getPackage() {
		return classCategory;
	}

	public String getPackageDirectory() {
		return classCategory.replace('.', File.separatorChar);
	}

	public void includeImportForType(String type) {
		String importPackage = (String) javaCodebase.packageLookup.get(type);
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
		return methods;
	}

	public boolean isSubclassAnyDepthOf(JavaClass aClass) {
		if (this == aClass) {
			return false;
		} else {
			return this.isClassOrSubclassAnyDepthOf(aClass);
		}
	}

	private boolean isClassOrSubclassAnyDepthOf(JavaClass aClass) {
		if (this == aClass) {
			return true;
		} else if (aClass == null) {
			return false;
		} else if (getSuperClass() != null) {
			return this.getSuperClass().isClassOrSubclassAnyDepthOf(aClass);
		} else {
			return false;
		}
	}
	
	public JavaClass getSuperClass() {
		return javaCodebase.getJavaClass(superclassName);
	}

	public void addMethod(JavaMethod method) {
		if (method.javaClass != null && method.javaClass != this) {
			throw new IllegalStateException("method already a member of a different type");
		}
		methods.add(method);
		method.javaClass = this;
	}

	public JavaMethod getMethod(String methodName) {
		JavaMethod found = null;
		for (Iterator iter = methods.iterator(); iter.hasNext();) {
			JavaMethod method = (JavaMethod) iter.next();
			if (method.name.equals(methodName)) {
				if (found != null) {
					throw new IllegalStateException("More than one match for: "+className+"."+methodName);
				}
				found = method;
			}
		}
		return found;
	}
	
	public String toString() {
		ToStringGenerator generator = new ToStringGenerator(this);
		generator.add("name", className);
		return generator.end();
	}

}