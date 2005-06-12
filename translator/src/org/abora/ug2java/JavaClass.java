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

import org.abora.ug2java.javatoken.JavaCallStart;
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

	private void includeImportForType(String type) {
		String importPackage = (String) javaCodebase.packageLookup.get(type);
		//TODO should be able to filter out imports for our package
		if (importPackage != null) {
			importedPackages.add(importPackage + "." + type);
		}
	}

	private void includeAnyReferencedTypes(MethodBody body) {
		List tokens = body.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.get(i);
			if ((token instanceof JavaIdentifier || token instanceof JavaType || token instanceof JavaCast || token instanceof JavaCallStart)
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

	public void generateImports() {
		includeImportForType(superclassName);
		for (Iterator iter = fields.iterator(); iter.hasNext();) {
			JavaField field = (JavaField) iter.next();
			includeImportForType(field.type);
		}
		includeAnyReferencedTypes(methods);
		includeAnyReferencedTypes(staticBlocks);
	}
	
	private void includeAnyReferencedTypes(List methodsList) {
		for (Iterator iter = methodsList.iterator(); iter.hasNext();) {
			JavaMethod method = (JavaMethod) iter.next();
			if (method.shouldInclude) {
				includeAnyReferencedTypes(method);
			}
		}
	}

	private void includeAnyReferencedTypes(JavaMethod method) {
		if (!method.shouldInclude) {
			return;
		}
		for (Iterator iter = method.parameters.iterator(); iter.hasNext();) {
			JavaField field = (JavaField) iter.next();
			includeImportForType(field.type);
		}
		includeImportForType(method.returnType);
		includeAnyReferencedTypes(method.methodBody);
	}

	public String findMatchingMethodReturnType(String callName, int numberOfArgs, boolean onlyStatic) {
		//TODO take into account full method signature
		String returnTypeName = null;
		JavaClass currentClass = this;
		do {
			for (Iterator iter = currentClass.methods.iterator(); iter.hasNext();) {
				JavaMethod javaMethod = (JavaMethod) iter.next();
				if ((!onlyStatic || javaMethod.isStatic()) && javaMethod.name.equals(callName) && javaMethod.parameters.size() == numberOfArgs) {
					String methodReturnType = javaMethod.returnType;
					if (returnTypeName == null) {
						returnTypeName = methodReturnType;
					} else if (!returnTypeName.equals(methodReturnType)) {
						return null;
					}
				}
			}
			currentClass = currentClass.getSuperClass();
		} while (currentClass != null);
		
		return returnTypeName;
	}

	public JavaMethod findMatchingMethod(String callName, int numberOfArgs, boolean onlyStatic) {
		//TODO take into account full method signature
		JavaMethod match = null;
		JavaClass currentClass = this;
		do {
			for (Iterator iter = currentClass.methods.iterator(); iter.hasNext();) {
				JavaMethod javaMethod = (JavaMethod) iter.next();
				if ((!onlyStatic || javaMethod.isStatic()) && javaMethod.name.equals(callName) && javaMethod.parameters.size() == numberOfArgs) {
					if (match == null) {
						match = javaMethod;
					} else {
						return null;
					}
				}
			}
			currentClass = currentClass.getSuperClass();
		} while (currentClass != null);
		
		return match;
	}

}