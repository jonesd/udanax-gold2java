/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockEnd;
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
				factory.token(JavaBlockEnd.class),
				factory.token(JavaKeyword.class, "return"),
				factory.token(JavaToken.class),
				factory.token(JavaStatementTerminator.class),
				factory.any(
						factory.token(JavaComment.class, "compiler fodder"),
						factory.token(JavaComment.class, "fodder")
				));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int blockStart = javaMethod.methodBody.findStartOfBlock(i);
		JavaToken preBlock = (JavaToken)tokens.get(blockStart - 1);
		if (preBlock instanceof JavaKeyword && "else".equals(preBlock.value)) {
			// Can only trim out the return if this return wont be called.
			// This is just a quick approximate check
			tokens.remove(i+4);
			tokens.remove(i+3);
			tokens.remove(i+2);
			tokens.remove(i+1);
		}
		return i;
	}
}
