/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

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
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;

import sun.tools.tree.AssignMultiplyExpression;



public class TransformCastIntoOthers extends AbstractMethodBodyTransformation {


	public TransformCastIntoOthers() {
		super();
	}
	public TransformCastIntoOthers(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
			factory.token(JavaIdentifier.class),
			factory.token(JavaCallKeywordStart.class, "castIntoOthers"),
			factory.token(JavaIdentifier.class),
			factory.token(JavaCallArgumentSeparator.class),
			factory.token(JavaBlockStart.class)
			);
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		String sourceName = ((JavaIdentifier)tokens.get(i)).value;
		String tempType = ((JavaIdentifier)tokens.get(i + 2)).value;
		String tempName = ((JavaIdentifier)tokens.get(i + 6)).value;
		tokens.remove(i+1);
		tokens.add(i+1, new JavaKeyword("instanceof"));
		tokens.remove(i + 3);
		tokens.add(i + 3, new JavaParenthesisEnd());
		tokens.remove(i + 6);
		tokens.remove(i + 5);
		tokens.add(i + 5, new JavaType(tempType));
		tokens.add(i + 6, new JavaIdentifier(tempName));
		tokens.add(i + 7, new JavaAssignment());
		tokens.add(i + 8, new JavaCast(tempType));
		tokens.add(i + 9, new JavaIdentifier(sourceName));

		int endOfIntoBlock = javaMethod.methodBody.findEndOfBlock(i + 4);
		JavaCallArgumentSeparator separator = (JavaCallArgumentSeparator)tokens.get(endOfIntoBlock + 1);
		tokens.remove(endOfIntoBlock + 1);
		tokens.add(endOfIntoBlock + 1, new JavaKeyword("else"));
		int endOfOthers = javaMethod.methodBody.findEndOfBlock(endOfIntoBlock + 2);
		JavaCallEnd callEnd = (JavaCallEnd)tokens.get(endOfOthers + 1);
		JavaStatementTerminator terminator = (JavaStatementTerminator)tokens.get(endOfOthers + 2);
		tokens.remove(endOfOthers + 2);
		tokens.remove(endOfOthers + 1);
		
		tokens.add(i, new JavaKeyword("if"));
		tokens.add(i + 1, new JavaParenthesisStart());

	}
}
