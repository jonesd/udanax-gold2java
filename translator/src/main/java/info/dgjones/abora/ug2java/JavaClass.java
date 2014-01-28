/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.ug2java;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Vector;

import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.javatoken.JavaType;
import info.dgjones.abora.ug2java.util.ToStringGenerator;

public class JavaClass {
	public String className;
	public String superclassName;
	public String classCategory;
	protected String comment;
	public Vector classQuotes = new Vector();
	public Vector instanceMethodChunks = new Vector();
	public Vector classMethodChunks = new Vector();
	public final List fields = new ArrayList();
	public final List methods = new ArrayList();
	public final JavaCodebase javaCodebase;
	protected final SortedSet imports = new TreeSet();
	protected final List staticBlocks = new ArrayList();

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
		//TODO should be able to filter out imports for our package
		if (importPackage != null) {
			imports.add(importPackage + "." + type);
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

	public JavaMethod getMethodOrInherited(String problemsName) {
		JavaMethod method = getMethod(problemsName);
		if (method == null && getSuperClass() != null) {
			return getSuperClass().getMethodOrInherited(problemsName);
		}
		return method;
		
	}

	public boolean isSubclassOf(JavaClass cast) {
		if (this == cast) {
			return true;
		} else if (getSuperClass() != null) {
			return getSuperClass().isSubclassOf(cast);
		} else {
			return false;
		}
	}

	
	public String getComment() {
		return comment;
	}

	
	public void setComment(String comment) {
		this.comment = comment;
	}
	
	public SortedSet getImports() {
		return Collections.unmodifiableSortedSet(imports);
	}

	public void addStaticBlock(JavaMethod staticBlock) {
		staticBlocks.add(staticBlock);
	}

	public void addStaticBlockFirst(JavaMethod staticBlock) {
		staticBlocks.add(0, staticBlock);
	}

	public List getStaticBlocks() {
		return Collections.unmodifiableList(staticBlocks);
	}
	
	/**
	 * Return the immediate subclasses
	 */
	public List subclasses() {
		List subclasses = new ArrayList();
		for (Iterator allClasses = javaCodebase.allClasses().iterator(); allClasses.hasNext();) {
			JavaClass element = (JavaClass) allClasses.next();
			if (className.equals(element.superclassName)) {
				subclasses.add(element);
			}
		}
		return subclasses;
	}
	
	public String getQualifiedName() {
		return getPackage()+"."+className;
	}
}