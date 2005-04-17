/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformRaisedTo extends AbstractMethodBodyTransformation {

	
public TransformRaisedTo() {
		super();
	}
	public TransformRaisedTo(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallKeywordStart.class, "raisedTo");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int expressionStart = javaMethod.methodBody.findStartOfExpression(i-1);
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i);
		tokens.remove(i);
		tokens.add(i, new JavaCallArgumentSeparator());
		tokens.add(expressionStart, new JavaIdentifier("Math"));
		tokens.add(expressionStart+1, new JavaCallKeywordStart("pow"));
		return i;
	}
}
