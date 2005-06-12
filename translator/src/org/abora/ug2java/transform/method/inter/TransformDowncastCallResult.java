/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.inter;

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



public class TransformDowncastCallResult extends AbstractMethodBodyTransformation {

	public TransformDowncastCallResult() {
		super();
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCodebase javaCodebase = javaMethod.javaClass.javaCodebase;

		int base = i;
		boolean onlyStatic;

		JavaClass callerClass;
		if (i > 0 && tokens.get(i-1) instanceof JavaIdentifier) {
			String className = ((JavaIdentifier)tokens.get(i-1)).value;
			base = i - 1;
			callerClass = javaCodebase.getJavaClass(className);
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
		} else if (javaMethod.isStatic()){
			callerClass = javaMethod.javaClass;
			onlyStatic = true;
		} else {
			return i;
		}
		
		String tempTypeName = null;
		
		if (base > 1 && tokens.get(base-2) instanceof JavaIdentifier && tokens.get(base-1) instanceof JavaAssignment) {
			String tempName = ((JavaIdentifier)tokens.get(base-2)).value;
			tempTypeName = javaMethod.findTypeOfVariable(tempName);
		} else if (base > 0 && tokens.get(base-1) instanceof JavaKeyword && ((JavaKeyword)tokens.get(base-1)).value.equals("return")) {
			tempTypeName = javaMethod.returnType;
		}

		if (tempTypeName == null) {
			return i;
		}

		String callName = ((JavaCallStart)tokens.get(i)).value;
		
						
		JavaClass tempType = javaCodebase.getJavaClass(tempTypeName);
		if (tempType == null && !javaMethod.getJavaCodebase().isPrimitiveType(tempTypeName)) {
			return i;
		}
		
		int args = javaMethod.methodBody.findNumberOfCallArgs(i);
		
		String returnTypeName = callerClass.findMatchingMethodReturnType(callName, args, onlyStatic);
		//TODO would prefer this to the above special case method, but not good enough at the moment to find the
		// single best matching method, so need to rely on merging in methods with the same return code
//		JavaMethod method = callerClass.findMatchingMethod(callName, args, onlyStatic);
//		if (method == null) {
//			return i;
//		}
//		String returnTypeName = method.returnType; 

		if (javaMethod.getJavaCodebase().shouldDowncast(returnTypeName, tempTypeName)) {
			tokens.add(base, new JavaCast(tempTypeName));
		}
		
		return i;
	}
	
}
