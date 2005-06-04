/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.inter;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaCodebase;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformDowncastArgument extends AbstractMethodBodyTransformation {

	public TransformDowncastArgument() {
		super();
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				//TODO simple first case
				factory.token(JavaKeyword.class, "return"),
				factory.token(JavaCallKeywordStart.class)
//				factory.token(JavaIdentifier.class),
//				factory.token(JavaCallEnd.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.isStatic()) {
			return i;
		}

		String callName = ((JavaCallStart)tokens.get(i+1)).value;
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i+1);

		JavaCodebase javaCodebase = javaMethod.javaClass.javaCodebase;

		List argumentTypes = new ArrayList();
		List argumentStarts = new ArrayList();

		int j = i + 2;
		while (j < callEnd) {
			//TODO Simple find type of expression code - extract
			argumentStarts.add(new Integer(j));
			String argumentTypeName = null;
			
			boolean wrappedInParanthesis = false;
			JavaToken token = (JavaToken)tokens.get(j);
			if (token instanceof JavaParenthesisStart) {
				wrappedInParanthesis = true;
				j += 1;
				token = (JavaToken)tokens.get(j);
			}
			if (token instanceof JavaCast) {
				argumentTypeName = token.value;
				j += 1;
				token = (JavaToken)tokens.get(j);
			}
			if (!(token instanceof JavaIdentifier)) {
				return i;
			}

			j += 1;
			if (wrappedInParanthesis) {
				JavaToken token2 = (JavaToken)tokens.get(j);
				if (!(token2 instanceof JavaParenthesisEnd)) {
					return i;
				}
				j += 1;
			}
			if (j < callEnd - 1) {
				JavaToken token2 = (JavaToken)tokens.get(j);
				if (!(token2 instanceof JavaCallArgumentSeparator)) {
					return i;
				}
				j += 1;
			}
			if (argumentTypeName == null) {
				String argumentName = token.value;
				argumentTypeName = javaMethod.findTypeOfVariable(argumentName);
				if (argumentTypeName == null) {
					return i;
				}
			}
			JavaClass argumentType = javaCodebase.getJavaClass(argumentTypeName);
			if (argumentType == null) {
				return i;
			}
			argumentTypes.add(argumentType);
		}
		
		JavaMethod method = findOnlyMatchingMethod(javaMethod.javaClass, callName, argumentTypes.size());
		if (method == null) {
			return i;
		}
		for (int k = argumentTypes.size() - 1; k >= 0; k -= 1) {
			JavaClass expectedType = method.getParameter(k).getJavaClass(javaCodebase);
			if (expectedType == null) {
				continue;
				//TODOreturn i;
			}
			JavaClass actualType = (JavaClass)argumentTypes.get(k);
			if (expectedType.isSubclassAnyDepthOf(actualType)) {
				int p = ((Integer)argumentStarts.get(k)).intValue();
				tokens.add(p, new JavaCast(expectedType.className));
			}
			
		}
				
		return i;
	}

	private JavaClass findArgumentType(JavaClass callerClass, String callName) {
		JavaMethod method = findOnlyMatchingMethod(callerClass, callName, 1);
		if (method != null) {
			return method.getParameter(0).getJavaClass(callerClass.getJavaCodebase());
		} else {
			return null;
		}
	}

	private JavaMethod findOnlyMatchingMethod(JavaClass callerClass, String callName, int totalParameters) {
		//TODO search parent classses for matching methods
		//TODO take into account full method signature
		JavaMethod matchingMethod = null;
		for (Iterator iter = callerClass.methods.iterator(); iter.hasNext();) {
			JavaMethod javaMethod = (JavaMethod) iter.next();
			if (javaMethod.name.equals(callName)) {
				if (javaMethod.parameters.size() == totalParameters) {
					if (matchingMethod == null) {
						matchingMethod = javaMethod;
					} else {
						return null;
					}
				}
			}
		}
		return matchingMethod;
	}
}
