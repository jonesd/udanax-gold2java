/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.transform.tokenmatcher.MatchAny;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformDOThashForEqual extends AbstractMethodBodyTransformation {

	public TransformDOThashForEqual() {
		super();
	}
	public TransformDOThashForEqual(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
			factory.token(JavaIdentifier.class),
			factory.token(JavaCallStart.class, "DOThashForEqual"),
			factory.token(JavaCallEnd.class));
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i+1);
		tokens.add(i, new JavaIdentifier("HashHelper"));
		tokens.add(i + 1, new JavaCallKeywordStart("hashForEqual"));
	}
}