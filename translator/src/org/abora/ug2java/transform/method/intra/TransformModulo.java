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
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformModulo extends AbstractMethodBodyTransformation {

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaKeyword.class, "%");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int start = javaMethod.methodBody.findStartOfExpression(i-1);
		tokens.remove(i);
		tokens.add(i, new JavaCallArgumentSeparator());
		int end = javaMethod.methodBody.findEndOfExpression(i+1);
		tokens.add(end+1, new JavaCallEnd());
		tokens.add(start, new JavaIdentifier("AboraSupport"));
		tokens.add(start+1, new JavaCallKeywordStart("modulo"));
		
		return i;
	}
}
