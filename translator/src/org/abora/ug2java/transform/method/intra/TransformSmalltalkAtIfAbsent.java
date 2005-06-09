/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSmalltalkAtIfAbsent extends AbstractMethodBodyTransformation {


	public TransformSmalltalkAtIfAbsent() {
		super();
	}
	public TransformSmalltalkAtIfAbsent(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaAssignment.class),
				factory.token(JavaIdentifier.class, "Smalltalk"),
				factory.token(JavaCallKeywordStart.class, "ifAbsent"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallArgumentSeparator.class)
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier varAssign = (JavaIdentifier)tokens.get(i);
		JavaIdentifier varSymbol = (JavaIdentifier)tokens.get(i+4);
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i+3);

		javaMethod.methodBody.removeShouldMatch(callEnd+1, JavaStatementTerminator.class);
		javaMethod.methodBody.removeShouldMatch(callEnd, JavaCallEnd.class);

		for (int j = i+ 5; j >= i; j--) {
			tokens.remove(j);
		}
		int j = i;
		tokens.add(j++, new JavaType("Category"));
		tokens.add(j++, new JavaIdentifier("ifAbsent"));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaIdentifier("Smalltalk"));
		tokens.add(j++, new JavaCallKeywordStart("at"));
		tokens.add(j++, new JavaIdentifier(varSymbol.value));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaStatementTerminator());
		tokens.add(j++, new JavaKeyword("if"));
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaIdentifier("ifAbsent"));
		tokens.add(j++, new JavaKeyword("!="));
		tokens.add(j++, new JavaIdentifier("null"));
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.add(j++, new JavaBlockStart());
		tokens.add(j++, new JavaIdentifier(varAssign.value));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaIdentifier("ifAbsent"));
		tokens.add(j++, new JavaStatementTerminator());
		tokens.add(j++, new JavaBlockEnd());
		tokens.add(j++, new JavaKeyword("else"));
		
		return i;

	}
}
