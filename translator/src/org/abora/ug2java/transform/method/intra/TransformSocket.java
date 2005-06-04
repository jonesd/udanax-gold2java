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
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;

public class TransformSocket extends AbstractMethodBodyTransformation {


	public TransformSocket() {
		super();
	}

	public TransformSocket(TokenMatcherFactory factory) {
		super(factory);
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class, "close|acceptNonBlock|listenFor");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		int expressionStart = javaMethod.methodBody.findStartOfExpressionMinimal(i-1);
		
		tokens.remove(i);
		if (call instanceof JavaCallKeywordStart) {
			tokens.add(i, new JavaCallArgumentSeparator());
		}
		
		tokens.add(expressionStart, new JavaIdentifier("AboraSocketSupport"));
		tokens.add(expressionStart+1, new JavaCallKeywordStart(call.value));
				
		return i;
	}
}