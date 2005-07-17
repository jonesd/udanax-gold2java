/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.Annotation;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformRequires extends AbstractMethodBodyTransformation {

	public TransformRequires() {
		super();
	}
	public TransformRequires(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallKeywordStart.class, "REQUIRES");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		Set required = (Set)javaMethod.getAnnotations().get(Annotation.REQUIRES);
		if (required == null) {
			required = new HashSet();
			javaMethod.getAnnotations().put(Annotation.REQUIRES, required);
		}
		
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i);
		if (callEnd + 1 < tokens.size() && (tokens.get(callEnd + 1) instanceof JavaStatementTerminator)) {
			tokens.remove(callEnd + 1);
		}
		for (int j = i+1; j < callEnd; j++) {
			if (tokens.get(j) instanceof JavaIdentifier) {
				JavaIdentifier classIdentifier = (JavaIdentifier)tokens.get(j);
				if (javaMethod.getJavaCodebase().getJavaClass(classIdentifier.value) != null) {
					required.add(classIdentifier.value);
				}
			}
		}
		for (int j = callEnd; j >= i; j--) {
			tokens.remove(j);
		}
		
		return i-1;
	}
}
