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
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformExcessParantheses extends AbstractMethodBodyTransformation {

	public TransformExcessParantheses() {
		super();
	}
	public TransformExcessParantheses(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaParenthesisEnd.class), 
				factory.any(
						factory.token(JavaStatementTerminator.class),
						factory.token(JavaBlockEnd.class))); 
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int start = javaMethod.methodBody.findOpeningTokenOfType(i, JavaParenthesisStart.class);
		if (start > 0) {
			JavaToken preStart = (JavaToken)tokens.get(start-1);
			if (! (preStart instanceof JavaStatementTerminator || preStart instanceof JavaBlockEnd || preStart instanceof JavaBlockStart)) {
				return i;
			}
		}
		tokens.remove(i);
		tokens.remove(start);
		return i;
	}
}
