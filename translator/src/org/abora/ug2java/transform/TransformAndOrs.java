/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformAndOrs extends AbstractMethodBodyTransformation {

	public TransformAndOrs() {
		super();
	}
	public TransformAndOrs(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.any(
						factory.token(JavaCallKeywordStart.class, "and"), 
						factory.token(JavaCallKeywordStart.class, "or")), 
				factory.token(JavaBlockStart.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallKeywordStart call = (JavaCallKeywordStart)tokens.get(i);
		int closingIndex = javaMethod.methodBody.findEndOfBlock(i + 1);
		if (!(tokens.get(closingIndex + 1) instanceof JavaCallEnd)
			&& (!(tokens.get(closingIndex + 1) instanceof JavaComment) && !(tokens.get(closingIndex + 2) instanceof JavaCallEnd))) {
			throw new IllegalStateException("short circuit not properly terminated with )");
		}
		tokens.remove(closingIndex + 1);
		tokens.remove(closingIndex);
		tokens.add(closingIndex, new JavaParenthesisEnd());
		tokens.remove(i + 1);
		tokens.remove(i);
		String value;
		if (call.value.equals("and")) {
			value = "&&";
		} else {
			value = "||";
		}
		tokens.add(i, new JavaKeyword(value));
		tokens.add(i + 1, new JavaParenthesisStart());
		return i;

	}
}
