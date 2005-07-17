package org.abora.ug2java.transform.type;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.Annotation;
import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.SmalltalkSource;
import org.abora.ug2java.javatoken.JavaArrayInitializerEnd;
import org.abora.ug2java.javatoken.JavaArrayInitializerStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.StringLiteral;



public class RecordHeaperHierarchy implements ClassTransformer {

	
	public void transform(JavaClass javaClass) {
		if (!javaClass.className.equals("Heaper")) {
			return;
		}
		List allClasses = new ArrayList();
		addClass(javaClass, allClasses);
		
		createClassHierarchyMethod(javaClass, allClasses);
		createInitTimeNonInheritedDependencies(javaClass, allClasses);
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
