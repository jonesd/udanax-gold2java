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
package info.dgjones.abora.ug2java.transform.type;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import info.dgjones.abora.ug2java.Annotation;
import info.dgjones.abora.ug2java.JavaClass;
import info.dgjones.abora.ug2java.JavaCodebase;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.MethodBody;
import info.dgjones.abora.ug2java.SmalltalkSource;
import info.dgjones.abora.ug2java.javatoken.JavaArrayInitializerEnd;
import info.dgjones.abora.ug2java.javatoken.JavaArrayInitializerStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.StringLiteral;



public class RecordHeaperHierarchy implements ClassTransformer {

	
	public void transform(JavaClass javaClass) {
		if (!javaClass.className.equals("Heaper")) {
			return;
		}
		List allClasses = new ArrayList();
		addClass(javaClass, allClasses);
		//TODO manually add "missing" classes
		addMissingClass("info.dgjones.abora.gold.java.missing", "ShepherdStub", allClasses);

		addMissingClass("info.dgjones.abora.gold.collection.basic", "IEEE32Array", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "IEEE64Array", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "Int32Array", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "Int8Array", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "IntegerVarArray", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "PrimArray", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "PrimDataArray", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "PrimFloatArray", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "PrimIntArray", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "PrimIntegerArray", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "PtrArray", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "SharedPtrArray", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "UInt32Array", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "UInt8Array", allClasses);
		addMissingClass("info.dgjones.abora.gold.collection.basic", "WeakPtrArray", allClasses);
		
		createClassHierarchyMethod(javaClass, allClasses);
		createInitTimeNonInheritedDependencies(javaClass, allClasses);
	}
	
	protected void addMissingClass(String classPackage, String className, List allClasses) {
		JavaCodebase codebase = ((JavaClass)(allClasses.get(0))).getJavaCodebase();
		JavaClass c = new JavaClass(className, codebase);
		c.classCategory = classPackage;
		allClasses.add(c);
		
	}

	protected JavaMethod createClassHierarchyMethod(JavaClass javaClass, List allClasses) {
		JavaMethod method = new JavaMethod("String[]", "classHierarchy");
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaKeyword("new"));
		tokens.add(new JavaKeyword("String[]"));
		tokens.add(new JavaArrayInitializerStart());
		for (Iterator iter = allClasses.iterator(); iter.hasNext();) {
			JavaClass element = (JavaClass) iter.next();
			//TODO really checking to see if this is a generated/translated class or not
			if (element.getPackage() != null) {
				tokens.add(new StringLiteral(element.getPackage()+"."+element.className));
				if (iter.hasNext()) {
					tokens.add(new JavaCallArgumentSeparator());
				}
			}
		}
		tokens.add(new JavaArrayInitializerEnd());
		tokens.add(new JavaStatementTerminator());

		method.modifiers = "static ";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: RecordHeaperHierarchy";
		javaClass.addMethod(method);
		return method;
	}

	protected JavaMethod createInitTimeNonInheritedDependencies(JavaClass javaClass, List allClasses) {
		JavaMethod method = new JavaMethod("String[][]", "initTimeNonInheritedDependencies");
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaKeyword("new"));
		tokens.add(new JavaKeyword("String[][]"));
		tokens.add(new JavaArrayInitializerStart());
		for (Iterator iter = allClasses.iterator(); iter.hasNext();) {
			JavaClass element = (JavaClass) iter.next();
			JavaMethod createInitTimeNonInherited = element.getMethod("initTimeNonInherited");
			if (createInitTimeNonInherited != null) {
				Set dependencies = (Set)createInitTimeNonInherited.getAnnotations().get(Annotation.REQUIRES);
				if (dependencies != null) {
					tokens.add(new JavaArrayInitializerStart());
					tokens.add(new StringLiteral(element.getQualifiedName()));
					tokens.add(new JavaCallArgumentSeparator());
					for (Iterator iterator = dependencies.iterator(); iterator.hasNext();) {
						String dependenceName = (String) iterator.next();
						JavaClass dependencyClass = javaClass.getJavaCodebase().getJavaClass(dependenceName);
						if (dependencyClass != null) {
							tokens.add(new StringLiteral(dependencyClass.getQualifiedName()));
							tokens.add(new JavaCallArgumentSeparator());
						} else {
							System.out.println("--Warning: Failed to find initTimeNonInheritedDependencies class lookup for: "+dependenceName);
						}
					}
					tokens.add(new JavaArrayInitializerEnd());
					tokens.add(new JavaCallArgumentSeparator());
				}
			}
		}
		tokens.add(new JavaArrayInitializerEnd());
		tokens.add(new JavaStatementTerminator());

		method.modifiers = "static ";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: RecordHeaperHierarchy";
		javaClass.addMethod(method);
		return method;
	}

	
	protected void addClass(JavaClass start, List allClasses) {
		allClasses.add(start);
		List subclasses = start.subclasses();
		for (Iterator iter = subclasses.iterator(); iter.hasNext();) {
			JavaClass element = (JavaClass) iter.next();
			addClass(element, allClasses);
		}
	}
}
