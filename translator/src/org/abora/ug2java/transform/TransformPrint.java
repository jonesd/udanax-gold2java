/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformPrint extends AbstractMethodBodyTransformation {


	public TransformPrint() {
		super();
	}
	public TransformPrint(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaKeyword.class, "<<");
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		int endOfBlock = javaMethod.methodBody.findEndOfExpression(i+1);
		tokens.add(endOfBlock+1, new JavaCallEnd());
		tokens.remove(i);
		tokens.add(i, new JavaCallKeywordStart("print"));
	}
}
