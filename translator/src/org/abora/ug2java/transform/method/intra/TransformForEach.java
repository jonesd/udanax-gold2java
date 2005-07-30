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
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLoopTerminator;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformForEach extends AbstractMethodBodyTransformation {

	private String lastMatchingMethodSignature = "";
	private int lastMatchingMethodOccurrences = 0;

	public TransformForEach() {
		super();
	}
	public TransformForEach(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallKeywordStart.class, "for(Each|Indices|Positions)"),
				factory.token(JavaBlockStart.class),
				factory.token(JavaType.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallKeywordStart callToken = (JavaCallKeywordStart)tokens.get(i);
		boolean forIndices = callToken.value.equals("forIndices");
		boolean forPositions = callToken.value.equals("forPositions");
		
		
		int expressionStart = javaMethod.methodBody.findStartOfExpression(i-1);

		JavaType elementTypeToken = (JavaType)tokens.get(i+2);
		JavaIdentifier elementNameToken = (JavaIdentifier)tokens.get(i+3);
		
		String stepName = "stomper";
		String firstMethodName = forIndices ? "index" : (forPositions ? "position": "fetch");
		String secondMethodName = (forIndices | forPositions) ? "fetch" : null;
		String stepperClassName = (forIndices | forPositions) ? "TableStepper" : "Stepper";
		
		String methodSignature = javaMethod.getQualifiedSignature();
		if (methodSignature.equals(lastMatchingMethodSignature)) {
			lastMatchingMethodOccurrences += 1;
			stepName += lastMatchingMethodOccurrences;
		} else {
			lastMatchingMethodSignature = methodSignature;
			lastMatchingMethodOccurrences = 1;
		}

		int blockEnd = javaMethod.methodBody.findEndOfBlock(i+1);
		if (blockEnd+2 < tokens.size() && tokens.get(blockEnd+2) instanceof JavaStatementTerminator) {
			javaMethod.methodBody.removeShouldMatch(blockEnd+2, JavaStatementTerminator.class);
		}
		javaMethod.methodBody.removeShouldMatch(blockEnd+1, JavaCallEnd.class);
		int j = blockEnd+1;
		tokens.add(j++, new JavaIdentifier(stepName));
		tokens.add(j++, new JavaCallStart("destroy"));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaStatementTerminator());
		
		
		if (secondMethodName != null) {
			assign(tokens, i + 5, stepName, secondMethodName);
		}
		assign(tokens, i + 2, stepName, firstMethodName);
		
		tokens.remove(i);
		j = i;
		tokens.add(j++, new JavaStatementTerminator());
		tokens.add(j++, new JavaKeyword("for"));
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaLoopTerminator());
		tokens.add(j++, new JavaIdentifier(stepName));
		tokens.add(j++, new JavaCallStart("hasValue"));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaLoopTerminator());
		tokens.add(j++, new JavaIdentifier(stepName));
		tokens.add(j++, new JavaCallStart("step"));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaParenthesisEnd());
		
		j = expressionStart;
		tokens.add(j++, new JavaType(stepperClassName));
		tokens.add(j++, new JavaIdentifier(stepName));
		tokens.add(j++, new JavaAssignment());
				
		return i;
	}
	
	protected void assign(List tokens, int i, String stepName, String methodName) {
		JavaType elementTypeToken = (JavaType)tokens.get(i);
		
		int j = i + 2;
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaCast(elementTypeToken.value));
		tokens.add(j++, new JavaIdentifier(stepName));
		tokens.add(j++, new JavaCallStart(methodName));
		tokens.add(j++, new JavaCallEnd());

	}
}
