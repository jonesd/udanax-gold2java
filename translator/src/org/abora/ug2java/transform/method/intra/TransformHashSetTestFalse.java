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
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.StringLiteral;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * Transformation due to incorrect original source code within HashSetTester.test2On 
 */
public class TransformHashSetTestFalse extends AbstractMethodBodyTransformation {

	public TransformHashSetTestFalse() {
		super();
	}
	public TransformHashSetTestFalse(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaKeyword.class, "else"),
				factory.token(JavaBlockStart.class),
				factory.token(StringLiteral.class, "\"FALSE\""),
				factory.token(JavaStatementTerminator.class),
				factory.token(JavaBlockEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getQualifiedName().equals("HashSetTester.test2On")) {
			return i;
		}
		tokens.add(i+2, new JavaIdentifier("oo"));
		tokens.add(i+3, new JavaCallKeywordStart("print"));
		
		tokens.add(i+5, new JavaCallEnd());
		return i;
	}
}
