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
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
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
		//TODO automatically handle comments, rather than working around them here...
		int postClosingIndex = closingIndex+1;
		JavaToken postClosing = (JavaToken)tokens.get(closingIndex+1);
		if (postClosing instanceof JavaComment) {
			postClosingIndex += 1;
		}
		javaMethod.methodBody.removeShouldMatch(postClosingIndex, JavaCallEnd.class);
		javaMethod.methodBody.removeShouldMatch(closingIndex, JavaBlockEnd.class);
		tokens.add(closingIndex, new JavaParenthesisEnd());
		javaMethod.methodBody.removeShouldMatch(i + 1, JavaBlockStart.class);
		javaMethod.methodBody.removeShouldMatch(i, JavaCallKeywordStart.class);
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
