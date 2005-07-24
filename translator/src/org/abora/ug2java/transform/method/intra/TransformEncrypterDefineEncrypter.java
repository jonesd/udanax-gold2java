/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.IntegerLiteral;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * TODO this class only exists because of limitations of choosing transformOnly with a String
 */
public class TransformEncrypterDefineEncrypter extends AbstractMethodBodyTransformation {

	public TransformEncrypterDefineEncrypter() {
		super();
	}
	public TransformEncrypterDefineEncrypter(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaParenthesisStart.class),
				factory.token(JavaIdentifier.class, "Smalltalk"),
				factory.token(JavaCallKeywordStart.class, "at"),
				factory.token(JavaIdentifier.class, "className"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaParenthesisEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getQualifiedName().equals("Encrypter.DEFINEUENCRYPTER")) {
			return i;
		}
		
		tokens.add(i+6, new JavaCallEnd());
		tokens.add(i, new JavaIdentifier("EncrypterConstructor"));
		tokens.add(i+1, new JavaCallKeywordStart("make"));
		
		return i+2;
	}
}
