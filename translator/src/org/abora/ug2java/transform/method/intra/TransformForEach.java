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
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLoopTerminator;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;


/**
 * udanax-top.st:52876:Stepper methodsFor: 'smalltalk: operations'!
 * {void} forEach: fn {BlockClosure} 
 * 	[| elem {Heaper} |
 * 	[(elem _ self fetch) ~~ NULL]
 * 		whileTrue:
 * 			[fn value: elem.
 * 			self step]]
 * 		valueNowOrOnUnwindDo: [self destroy]!
 * 
 * udanax-top.st:55277:TableStepper methodsFor: 'smalltalk: operations'!
 * {void} forIndices: fn {BlockClosure of: IntegerVar with: Heaper} 
 * 	[| result {Heaper} |
 * 	[(result _ self fetch) ~~ NULL]
 * 		whileTrue:
 * 			[fn of: self index and: result.
 * 			self step]]
 * 		valueNowOrOnUnwindDo: [self destroy]!
 * 
 * udanax-top.st:55285:TableStepper methodsFor: 'smalltalk: operations'!
 * {void} forKeyValues: fn {BlockClosure of: Position with: Heaper} 
 * 	[| result {Heaper} |
 * 	[(result _ self fetch) ~~ NULL]
 * 		whileTrue:
 * 			[fn of: self position and: result.
 * 			self step]]
 * 		valueNowOrOnUnwindDo: [self destroy]!
 * 
 * udanax-top.st:55293:TableStepper methodsFor: 'smalltalk: operations'!
 * {void} forPositions: fn {BlockClosure of: Position with: Heaper} 
 * 	[| result {Heaper} |
 * 	[(result _ self fetch) ~~ NULL]
 * 		whileTrue:
 * 			[fn of: self position and: result.
 * 			self step]]
 * 		valueNowOrOnUnwindDo: [self destroy]!
 * 
 */
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

		String firstElementTypeName = ((JavaType)tokens.get(i+2)).value;
		String firstElementName = ((JavaIdentifier)tokens.get(i+3)).value;
		
		String secondElementTypeName = forIndices || forPositions ? ((JavaType)tokens.get(i+5)).value : null;
		String secondElementName = forIndices || forPositions ? ((JavaIdentifier)tokens.get(i+6)).value : null;
		
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
			j = assign(tokens, i + 5, stepName, secondMethodName);
			continueIfNull(tokens, j, secondElementName);
		}
		j = assign(tokens, i + 2, stepName, firstMethodName);
		if (secondMethodName == null) {
			continueIfNull(tokens, j, firstElementName);
		}
		
//		if (!elementTypeToken.value.equals("int")) {
//			//TODO perhaps we should check for 0 here, rather than skipping the block for int case?
//			continueIfNull(tokens, j, elementNameToken);
//		}
		
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
	private void continueIfNull(List tokens, int j, String elementName) {
		tokens.add(j++, new JavaStatementTerminator());
		tokens.add(j++, new JavaKeyword("if"));
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaIdentifier(elementName));
		tokens.add(j++, new JavaKeyword("=="));
		tokens.add(j++, new JavaIdentifier("null"));
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.add(j++, new JavaBlockStart());
		tokens.add(j++, new JavaKeyword("continue"));
		tokens.add(j++, new JavaStatementTerminator());
		tokens.add(j++, new JavaBlockEnd());
	}
	
	protected int assign(List tokens, int i, String stepName, String methodName) {
		JavaType elementTypeToken = (JavaType)tokens.get(i);
		
		int j = i + 2;
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaCast(elementTypeToken.value));
		tokens.add(j++, new JavaIdentifier(stepName));
		tokens.add(j++, new JavaCallStart(methodName));
		tokens.add(j++, new JavaCallEnd());
		return j;

	}
}
