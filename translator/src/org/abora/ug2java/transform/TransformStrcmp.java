/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformStrcmp extends AbstractMethodBodyTransformation {

	public TransformStrcmp() {
		super();
	}
	public TransformStrcmp(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "String"), 
				factory.token(JavaCallKeywordStart.class, "strcmp"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int endOfCall = javaMethod.methodBody.findClosingCallEnd(i+1);
		int nextArgument = javaMethod.methodBody.findNextTokenOfType(i+1, JavaCallArgumentSeparator.class);
		tokens.remove(nextArgument);
		tokens.add(nextArgument, new JavaCallKeywordStart("compareTo"));
		tokens.remove(i + 1);
		tokens.remove(i);
		
		return i;
	}
}
