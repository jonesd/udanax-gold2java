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
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformDowncastStaticCallAssignment extends AbstractMethodBodyTransformation {

	public TransformDowncastStaticCallAssignment() {
		super();
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaAssignment.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		String tempName = ((JavaIdentifier)tokens.get(i)).value;
		String className = ((JavaIdentifier)tokens.get(i+2)).value;
		String callName = ((JavaCallStart)tokens.get(i+3)).value;
		
		JavaCodebase javaCodebase = javaMethod.javaClass.javaCodebase;
		
		JavaClass callerClass = javaCodebase.getJavaClass(className);
		if (callerClass == null) {
			return i;
		}
		String tempTypeName = javaMethod.findTypeOfVariable(tempName);
		if (tempTypeName == null) {
			return i;
		}
		JavaClass tempClass = javaCodebase.getJavaClass(tempTypeName);
		if (tempClass == null) {
			return i;
		}

		
		//TODO IMPLEMENT
		JavaClass returnClass = findReturnType(callerClass, callName);
		if (returnClass == null) {
			//TODO void vs cant calculate returnClass
			return i;
		}
		if (tempClass.isSubclassAnyDepthOf(returnClass)) {
			tokens.add(i+2, new JavaCast(tempClass.className));
		}
		
		return i;
	}

	private JavaClass findReturnType(JavaClass callerClass, String callName) {
		//TODO search parent classses for matching methods
		//TODO take into account full method signature
		String returnType = null;
		for (Iterator iter = callerClass.methods.iterator(); iter.hasNext();) {
			JavaMethod javaMethod = (JavaMethod) iter.next();
			if (javaMethod.name.equals(callName)) {
				String methodReturnType = javaMethod.returnType;
				if (returnType == null) {
					returnType = methodReturnType;
				} else if (!returnType.equals(methodReturnType)) {
					return null;
				}
			}
		}
		if (returnType == null) {
			return null;
		} else {
			return callerClass.getJavaCodebase().getJavaClass(returnType);
		}
	}
}
