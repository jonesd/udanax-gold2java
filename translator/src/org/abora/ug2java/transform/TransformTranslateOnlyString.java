/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformTranslateOnlyString extends AbstractMethodBodyTransformation {

	public TransformTranslateOnlyString() {
		super();
	}
	public TransformTranslateOnlyString(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaLiteral.class), 
				factory.token(JavaCallStart.class, "translateOnly"), 
				factory.token(JavaCallEnd.class));
	}
	
	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaLiteral javaLiteral = (JavaLiteral)tokens.get(i);
		tokens.add(i, new JavaComment("translateOnly " + javaLiteral.value));
		tokens.remove(i + 1);
		tokens.remove(i + 1);
		tokens.remove(i + 1);
		if (i + 1 < tokens.size() && tokens.get(i + 1) instanceof JavaStatementTerminator) {
			tokens.remove(i + 1);
		}
	}
}
