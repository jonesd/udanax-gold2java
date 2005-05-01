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
import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
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
				factory.token(JavaCallKeywordStart.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallEnd.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.isStatic()) {
			return i;
		}
		
		String argumentName = ((JavaIdentifier)tokens.get(i+2)).value;
		String callName = ((JavaCallStart)tokens.get(i+1)).value;
		
		JavaCodebase javaCodebase = javaMethod.javaClass.javaCodebase;
		
		String argumentTypeName = javaMethod.findTypeOfVariable(argumentName);
		if (argumentTypeName == null) {
			return i;
		}
		JavaClass argumentType = javaCodebase.getJavaClass(argumentTypeName);
		if (argumentType == null) {
			return i;
		}

		
		//TODO IMPLEMENT
		JavaClass expectedType = findArgumentType(javaMethod.javaClass, callName);
		if (expectedType == null) {
			//TODO void vs cant calculate returnClass
			return i;
		}
		if (expectedType.isSubclassAnyDepthOf(argumentType)) {
			tokens.add(i+2, new JavaCast(expectedType.className));
		}
		
		return i;
	}

	private JavaClass findArgumentType(JavaClass callerClass, String callName) {
		//TODO search parent classses for matching methods
		//TODO take into account full method signature
		String argumentType = null;
		for (Iterator iter = callerClass.methods.iterator(); iter.hasNext();) {
			JavaMethod javaMethod = (JavaMethod) iter.next();
			if (javaMethod.name.equals(callName)) {
				if (javaMethod.parameters.size() == 1) {
					String methodArgumentType = ((JavaField)javaMethod.parameters.get(0)).type;
					if (argumentType == null) {
						argumentType = methodArgumentType;
					} else if (!argumentType.equals(methodArgumentType)) {
						return null;
					}
				}
			}
		}
		if (argumentType == null) {
			return null;
		} else {
			return callerClass.getJavaCodebase().getJavaClass(argumentType);
		}
	}
}
