/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformAssert extends AbstractMethodBodyTransformation {


	public TransformAssert() {
		super();
	}
	public TransformAssert(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class, "assert");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		int expressionStart = i;
		if (i > 0) {
			expressionStart = javaMethod.methodBody.findStartOfExpression(i - 1);
		}
		tokens.add(i, new JavaKeyword("throw"));
		tokens.add(i+1, new JavaKeyword("new"));
		call.value = "AboraAssertionException";
		javaMethod.javaClass.includeImportForType("AboraAssertionException");
		
		if (i > 0 && (!(tokens.get(expressionStart) instanceof JavaParenthesisStart) || !(tokens.get(i - 1) instanceof JavaParenthesisEnd))) {
			tokens.add(i, new JavaParenthesisEnd());
			tokens.add(expressionStart, new JavaParenthesisStart());
		}
		tokens.add(expressionStart, new JavaKeyword("if"));
		return i;

	}
}
