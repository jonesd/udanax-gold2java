/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCritical extends MethodBodyTransformation {

	public TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallKeywordStart.class, "critical"), 
				factory.token(JavaBlockStart.class));
	}

	public void transform(JavaMethod javaMethod, List tokens, int i) {
		int start = javaMethod.methodBody.findStartOfExpression(i - 1);
		int postCallEnd = javaMethod.methodBody.findClosingCallEnd(i);
		if (postCallEnd + 1 < tokens.size() && (tokens.get(postCallEnd + 1) instanceof JavaStatementTerminator)) {
			tokens.remove(postCallEnd + 1);
		}
		tokens.remove(postCallEnd);
		tokens.remove(i);
		tokens.add(i, new JavaParenthesisEnd());
		tokens.add(start, new JavaKeyword("synchronized"));
		tokens.add(start + 1, new JavaParenthesisStart());
	}
}
