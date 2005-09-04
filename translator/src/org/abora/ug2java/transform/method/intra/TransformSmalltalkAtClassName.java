/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSmalltalkAtClassName extends AbstractMethodBodyTransformation {

	public TransformSmalltalkAtClassName() {
		super();
	}
	public TransformSmalltalkAtClassName(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "Smalltalk"), 
				factory.token(JavaCallKeywordStart.class, "at"),
				factory.token(JavaIdentifier.class, "className"),
				factory.token(JavaCallEnd.class)
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i+1);
		tokens.remove(i);
		
		tokens.add(i, new JavaIdentifier("AboraSupport"));
		tokens.add(i+1, new JavaCallKeywordStart("findCategory"));
		
		return i;
	}
}
