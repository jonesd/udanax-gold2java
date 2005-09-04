/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.FloatingPointLiteral;
import org.abora.ug2java.javatoken.IntegerLiteral;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * Transformation due to incorrect original source code within HashSetTester.test2On 
 */
public class TransformOperatorPrecedence extends AbstractMethodBodyTransformation {

	public TransformOperatorPrecedence() {
		super();
	}
	public TransformOperatorPrecedence(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.any(factory.token(JavaIdentifier.class), factory.token(IntegerLiteral.class), factory.token(FloatingPointLiteral.class)),
				factory.token(JavaKeyword.class, "\\+|-"),
				factory.any(factory.token(JavaIdentifier.class), factory.token(IntegerLiteral.class), factory.token(FloatingPointLiteral.class)),
				factory.token(JavaKeyword.class, "\\*|/|%"),
				factory.any(factory.token(JavaIdentifier.class), factory.token(IntegerLiteral.class), factory.token(FloatingPointLiteral.class)));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.add(i+3, new JavaParenthesisEnd());
		tokens.add(i, new JavaParenthesisStart());
		return i;
	}
}
