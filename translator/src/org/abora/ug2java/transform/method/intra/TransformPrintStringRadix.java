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



public class TransformPrintStringRadix extends AbstractMethodBodyTransformation {

	
public TransformPrintStringRadix() {
		super();
	}
	public TransformPrintStringRadix(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallKeywordStart.class, "printStringRadix");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int expressionStart = javaMethod.methodBody.findStartOfExpression(i-1);
		tokens.remove(i);
		tokens.add(i, new JavaCallArgumentSeparator());
		tokens.add(expressionStart, new JavaIdentifier("Integer"));
		tokens.add(expressionStart+1, new JavaCallKeywordStart("toString"));
		return i;
	}
}
