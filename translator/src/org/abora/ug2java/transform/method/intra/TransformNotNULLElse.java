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
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformNotNULLElse extends AbstractMethodBodyTransformation {


	public TransformNotNULLElse() {
		super();
	}
	public TransformNotNULLElse(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq( 
			factory.token(JavaCallKeywordStart.class, "not(NULL|Nil)(Else)?"),
			factory.token(JavaBlockStart.class),
			factory.token(JavaType.class),
			factory.token(JavaIdentifier.class),
			factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		String callName = ((JavaCallKeywordStart)tokens.get(i)).value;
		String tempType = ((JavaType)tokens.get(i+2)).value;
		String tempName = ((JavaIdentifier)tokens.get(i+3)).value;
		
		int expressionStart = javaMethod.methodBody.findStartOfExpression(i-1);
		int ifBlockEnd = javaMethod.methodBody.findEndOfBlock(i+1);
		int callEnd = ifBlockEnd+1;
		if (callName.endsWith("Else")) {
			javaMethod.methodBody.shouldMatch(ifBlockEnd+1, JavaCallArgumentSeparator.class);
			int elseBlockStart = ifBlockEnd+2;
			javaMethod.methodBody.shouldMatch(elseBlockStart, JavaBlockStart.class);
			int elseBlockEnd = javaMethod.methodBody.findEndOfBlock(elseBlockStart);
			callEnd = elseBlockEnd+1;
		}
		javaMethod.methodBody.shouldMatch(callEnd, JavaCallEnd.class);
		javaMethod.methodBody.shouldMatch(callEnd+1, JavaStatementTerminator.class);
		
		tokens.remove(callEnd+1);
		tokens.remove(callEnd);
		
		if (callName.endsWith("Else")) {
			tokens.remove(ifBlockEnd+1);
			tokens.add(ifBlockEnd+1, new JavaKeyword("else"));
		}
		
		tokens.remove(i+4);
		tokens.remove(i+3);
		tokens.remove(i+2);
		
		tokens.remove(i);
		tokens.add(i, new JavaStatementTerminator());
		tokens.add(i+1, new JavaKeyword("if"));
		tokens.add(i+2, new JavaParenthesisStart());
		tokens.add(i+3, new JavaIdentifier(tempName));
		tokens.add(i+4, new JavaKeyword("!="));
		tokens.add(i+5, new JavaKeyword("null"));
		tokens.add(i+6, new JavaParenthesisEnd());
		
		tokens.add(expressionStart, new JavaType(tempType));
		tokens.add(expressionStart+1, new JavaIdentifier(tempName));
		tokens.add(expressionStart+2, new JavaAssignment());
		tokens.add(expressionStart+3, new JavaCast(tempType));
		return i;
	}
}
