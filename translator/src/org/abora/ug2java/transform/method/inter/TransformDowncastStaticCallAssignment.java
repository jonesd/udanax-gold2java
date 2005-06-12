/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.inter;

import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaCodebase;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformDowncastStaticCallAssignment extends AbstractMethodBodyTransformation {

	public TransformDowncastStaticCallAssignment() {
		super();
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
//				factory.token(JavaIdentifier.class),
//				factory.token(JavaAssignment.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		String tempTypeName = null;
		
		if (i > 1 && tokens.get(i-2) instanceof JavaIdentifier && tokens.get(i-1) instanceof JavaAssignment) {
			String tempName = ((JavaIdentifier)tokens.get(i-2)).value;
			tempTypeName = javaMethod.findTypeOfVariable(tempName);
		} else if (i > 0 && tokens.get(i-1) instanceof JavaKeyword && ((JavaKeyword)tokens.get(i-1)).value.equals("return")) {
			tempTypeName = javaMethod.returnType;
		}

		if (tempTypeName == null) {
			return i;
		}

		String className = ((JavaIdentifier)tokens.get(i)).value;
		String callName = ((JavaCallStart)tokens.get(i+1)).value;
		
		JavaCodebase javaCodebase = javaMethod.javaClass.javaCodebase;
		
		boolean onlyStatic;
		
		JavaClass callerClass = javaCodebase.getJavaClass(className);
		if (callerClass == null) {
			String varTypeName = javaMethod.findTypeOfVariable(className);
			callerClass = javaCodebase.getJavaClass(varTypeName);
			if (callerClass == null) {
				return i;
			} else {
				onlyStatic = false;
			}
		} else {
			onlyStatic = true;
		}
		
		JavaClass tempClass = javaCodebase.getJavaClass(tempTypeName);
		if (tempClass == null) {
			return i;
		}
		
		int args = javaMethod.methodBody.findNumberOfCallArgs(i+1);
		
		//TODO IMPLEMENT
		JavaClass returnClass = findReturnType(callerClass, callName, args, onlyStatic);
		if (returnClass == null) {
			//TODO void vs cant calculate returnClass
			return i;
		}
		if (tempClass.isSubclassAnyDepthOf(returnClass)) {
			tokens.add(i, new JavaCast(tempClass.className));
		}
		
		return i;
	}
	
	private JavaClass findReturnType(JavaClass callerClass, String callName, int numberOfArgs, boolean onlyStatic) {
		//TODO search parent classses for matching methods
		//TODO take into account full method signature
		String returnType = null;
		JavaClass currentClass = callerClass;
		do {
			for (Iterator iter = currentClass.methods.iterator(); iter.hasNext();) {
				JavaMethod javaMethod = (JavaMethod) iter.next();
				if ((!onlyStatic || javaMethod.isStatic()) && javaMethod.name.equals(callName) && javaMethod.parameters.size() == numberOfArgs) {
					String methodReturnType = javaMethod.returnType;
					if (returnType == null) {
						returnType = methodReturnType;
					} else if (!returnType.equals(methodReturnType)) {
						return null;
					}
				}
			}
			currentClass = currentClass.getSuperClass();
		} while (currentClass != null);
		
		if (returnType == null) {
			return null;
		} else {
			return callerClass.getJavaCodebase().getJavaClass(returnType);
		}
	}
}
