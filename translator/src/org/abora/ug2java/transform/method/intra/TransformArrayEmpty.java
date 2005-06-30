/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaArrayInitializerEnd;
import org.abora.ug2java.javatoken.JavaArrayInitializerStart;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformArrayEmpty extends AbstractMethodBodyTransformation {

	public TransformArrayEmpty() {
		super();
	}
	public TransformArrayEmpty(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaArrayInitializerStart.class),
				factory.token(JavaArrayInitializerEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i+1);
		tokens.remove(i);
		
		tokens.add(i, new JavaKeyword("new"));
		tokens.add(i+1, new JavaCallStart("Array"));
		tokens.add(i+2, new JavaCallEnd());
		return i;
	}
}
