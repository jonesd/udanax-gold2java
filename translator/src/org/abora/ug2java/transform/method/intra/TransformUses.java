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
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformUses extends AbstractMethodBodyTransformation {

	public TransformUses() {
		super();
	}
	public TransformUses(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaToken.class, "USES");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		
		if (tokens.get(i) instanceof JavaCallStart) {
			javaMethod.methodBody.removeShouldMatch(i+1, JavaCallEnd.class);
		}
		
		if (i + 1 < tokens.size() && (tokens.get(i + 1) instanceof JavaStatementTerminator)) {
			tokens.remove(i + 1);
		}
		
		int deleteTo;
		if (i > 0 && (tokens.get(i - 1) instanceof JavaBlockEnd)) {
			deleteTo = javaMethod.methodBody.findOpeningTokenOfType(i-1, JavaBlockStart.class);
		} else {
			deleteTo = javaMethod.methodBody.findStartOfExpression(i-1);
		}
		tokens.remove(i);

		int j = i - 1;
		for (; j >= deleteTo; j -= 1) {
			tokens.remove(j);
		}
		
		return j;
	}
}
