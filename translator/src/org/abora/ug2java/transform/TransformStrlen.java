/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformStrlen extends AbstractMethodBodyTransformation {

	public TransformStrlen() {
		super();
	}
	public TransformStrlen(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "String"), 
				factory.token(JavaCallStart.class, "strlen"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int endOfCall = javaMethod.methodBody.findClosingCallEnd(i+1);
		tokens.add(endOfCall, new JavaCallStart("length"));
		tokens.remove(i + 1);
		tokens.remove(i);
		
		return i;
	}
}
