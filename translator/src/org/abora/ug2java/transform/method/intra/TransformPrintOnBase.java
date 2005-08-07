/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformPrintOnBase extends AbstractMethodBodyTransformation {

	
public TransformPrintOnBase() {
		super();
	}
	public TransformPrintOnBase(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallKeywordStart.class, "printOnBase"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallArgumentSeparator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallKeywordStart call = (JavaCallKeywordStart)tokens.get(i);
		JavaIdentifier stream = (JavaIdentifier)tokens.get(i+1);
		int expressionStart = javaMethod.methodBody.findStartOfExpression(i-1);
		int endCall = javaMethod.methodBody.findClosingCallEnd(i);
		tokens.add(endCall+1, new JavaCallEnd());
		tokens.remove(i+1);
		tokens.remove(i);
		tokens.add(expressionStart, new JavaIdentifier(stream.value));
		tokens.add(expressionStart+1, new JavaCallKeywordStart("print"));
		tokens.add(expressionStart+2, new JavaIdentifier("AboraSupport"));
		tokens.add(expressionStart+3, new JavaCallKeywordStart("toBaseString"));
		return i;
	}
}
