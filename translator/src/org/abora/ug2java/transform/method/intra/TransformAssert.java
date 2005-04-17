/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
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
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i);
		tokens.add(callEnd+2, new JavaBlockEnd());
		
		int expressionStart = i;
		if (i > 0) {
			expressionStart = javaMethod.methodBody.findStartOfExpression(i - 1);
		}
		tokens.add(i, new JavaBlockStart());
		tokens.add(i+1, new JavaKeyword("throw"));
		tokens.add(i+2, new JavaKeyword("new"));
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
