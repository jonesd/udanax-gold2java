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
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSmalltalkOnly extends AbstractMethodBodyTransformation {

	public TransformSmalltalkOnly() {
		super();
	}
	public TransformSmalltalkOnly(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaBlockEnd.class), 
				factory.token(JavaIdentifier.class, "smalltalkOnly"));
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaBlockEnd blockEnd = (JavaBlockEnd)tokens.get(i);
		int blockStart = javaMethod.methodBody.findStartOfBlock(i);
		tokens.remove(blockStart);
		tokens.add(blockStart, new JavaComment(">>> smalltalkOnly"));
		tokens.remove(blockEnd);
		tokens.add(i, new JavaComment("<<< smalltalkOnly"));
		tokens.remove(i + 1);
		if (i + 1 < tokens.size() && tokens.get(i + 1) instanceof JavaStatementTerminator) {
			tokens.remove(i + 1);
		}
	}

}
