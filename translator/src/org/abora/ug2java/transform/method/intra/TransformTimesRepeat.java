/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaLoopTerminator;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformTimesRepeat extends AbstractMethodBodyTransformation {

	public TransformTimesRepeat() {
		super();
	}
	public TransformTimesRepeat(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallKeywordStart.class, "timesRepeat"), 
				factory.token(JavaBlockStart.class));
	}
	
	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		final String incrementVariable = "i";

		int start = javaMethod.methodBody.findStartOfExpression(i - 1);
		int postCallEnd = javaMethod.methodBody.findClosingCallEnd(i);
		if (postCallEnd + 1 < tokens.size() && (tokens.get(postCallEnd + 1) instanceof JavaStatementTerminator)) {
			tokens.remove(postCallEnd + 1);
		}
		tokens.remove(postCallEnd);
		tokens.remove(i);
		tokens.add(i, new JavaLoopTerminator());
		tokens.add(i + 1, new JavaIdentifier(incrementVariable));
		tokens.add(i + 2, new JavaKeyword("++"));
		tokens.add(i + 3, new JavaParenthesisEnd());
		tokens.add(start, new JavaKeyword("for"));
		tokens.add(start + 1, new JavaParenthesisStart());
		tokens.add(start + 2, new JavaType("int"));
		tokens.add(start + 3, new JavaIdentifier(incrementVariable));
		tokens.add(start + 4, new JavaAssignment());
		tokens.add(start + 5, new JavaLiteral("0"));
		tokens.add(start + 6, new JavaLoopTerminator());
		tokens.add(start + 7, new JavaIdentifier(incrementVariable));
		tokens.add(start + 8, new JavaKeyword("<"));
		
		return i;
	}
}
