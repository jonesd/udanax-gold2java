/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformAtIfAbsent extends AbstractMethodBodyTransformation {

	public TransformAtIfAbsent() {
		super();
	}
	public TransformAtIfAbsent(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallKeywordStart.class, "ifAbsent");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i);
		int blockStart = javaMethod.methodBody.findNextTokenOfTypeQuietFail(i, JavaBlockStart.class);
		if (blockStart == -1 || blockStart > callEnd) {
			return i;
		}
		int blockEnd = javaMethod.methodBody.findEndOfBlock(blockStart);
		
		tokens.remove(blockEnd);
		if (blockStart + 1 == blockEnd) {
			// Empty block
			tokens.add(blockEnd, new JavaIdentifier("null"));
		} else {
			javaMethod.methodBody.removeShouldMatch(blockEnd-1, JavaStatementTerminator.class);
		}
		tokens.remove(blockStart);
		return i;
	}
}
