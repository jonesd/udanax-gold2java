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
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformIntegerIntegerVar extends AbstractMethodBodyTransformation {


	public TransformIntegerIntegerVar() {
		super();
	}
	public TransformIntegerIntegerVar(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "Integer"), 
				factory.token(JavaCallKeywordStart.class, "IntegerVar"),
				factory.token(JavaLiteral.class),
				factory.token(JavaCallEnd.class));
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i+3);
		tokens.remove(i+1);
		tokens.remove(i);
	}
}
