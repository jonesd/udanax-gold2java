/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCompilerFodder extends AbstractMethodBodyTransformation {


	public TransformCompilerFodder() {
		super();
	}
	public TransformCompilerFodder(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaKeyword.class, "return"),
				factory.token(JavaToken.class),
				factory.token(JavaStatementTerminator.class),
				factory.any(
						factory.token(JavaComment.class, "compiler fodder"),
						factory.token(JavaComment.class, "fodder")
				));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i + 3);
		tokens.remove(i + 2);
		tokens.remove(i + 1);
		tokens.remove(i);
		return i;
	}
}
