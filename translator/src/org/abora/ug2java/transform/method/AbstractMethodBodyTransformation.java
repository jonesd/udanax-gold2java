/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public abstract class AbstractMethodBodyTransformation implements MethodTransformation {

	private final TokenMatcher tokenMatcher;
	
	public AbstractMethodBodyTransformation() {
		this(new TokenMatcherFactory());
	}
	
	public AbstractMethodBodyTransformation(TokenMatcherFactory factory) {
		tokenMatcher = matchers(factory);
	}
	
	public void transform(JavaMethod javaMethod) {
		MethodBody methodBody = javaMethod.methodBody;
		List methodBodyTokens = methodBody.tokens;
		for (int i = 0; i < methodBodyTokens.size(); i++) {
			if (tokenMatcher.doesMatch(methodBodyTokens, i)) {
				int nextI = transform(javaMethod, methodBodyTokens, i);
				i = nextI;
			}
		}
	}
	
	protected abstract TokenMatcher matchers(TokenMatcherFactory factory);
	protected abstract int transform(JavaMethod javaMethod, List methodBodyTokens, int indexOfMatch);
}
