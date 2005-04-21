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
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformNewBecome extends AbstractMethodBodyTransformation {


	public TransformNewBecome() {
		super();
	}
	public TransformNewBecome(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
//				factory.token(JavaParenthesisStart.class),
//				factory.token(JavaIdentifier.class), 
				factory.token(JavaCallKeywordStart.class, "newBecome"),
				factory.token(JavaToken.class),
				factory.token(JavaCallEnd.class),
				factory.token(JavaParenthesisEnd.class),
				factory.token(JavaKeyword.class, "new"), 
				factory.token(JavaCallStart.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i+5);
		
		tokens.remove(i+3);
		tokens.remove(i+2);
		tokens.remove(i+1);
		tokens.remove(i);
		tokens.add(i, new JavaComment("TODO newBecome"));
		
		if (i > 0) {
			if (tokens.get(i-1) instanceof JavaIdentifier) {
				JavaIdentifier type = (JavaIdentifier)tokens.get(i-1);
				call.value = type.value;
				tokens.remove(i-1);
				tokens.remove(i-2);
			} else {
				tokens.remove(i-1);
			}
		}

		return i;
	}
}
