/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Iterator;
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
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class CopyOfTransformCastIntoOthers2 extends AbstractMethodBodyTransformation {


	public CopyOfTransformCastIntoOthers2() {
		super();
	}
	public CopyOfTransformCastIntoOthers2(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
			factory.token(JavaToken.class),
			factory.token(JavaCallKeywordStart.class, "([cC]astInto)+(Others)?")
//			factory.token(JavaIdentifier.class),
//			factory.token(JavaCallArgumentSeparator.class),
//			factory.token(JavaBlockStart.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		String sourceName;
		boolean sourceIsIdentifier;
		if (tokens.get(i) instanceof JavaIdentifier) {
			sourceName = ((JavaIdentifier)tokens.get(i)).value;
			sourceIsIdentifier = true;
		} else {
			sourceName = "cast1";
			sourceIsIdentifier = false;
		}
		String call = ((JavaCallKeywordStart)tokens.get(i+1)).value.toLowerCase();
		int base = i;
		int endOfIntoBlock = Integer.MAX_VALUE;
		boolean firstIntoBlock = true;
		while (call.startsWith("castinto")) {
			int typeStart = base+2;
			List types = new ArrayList();
			int typeEnd = readTypes(tokens, typeStart, types);
			for (int k = typeEnd; k >= typeStart; k--) {
				tokens.remove(k);
			}
			for (Iterator iter = types.iterator(); iter.hasNext();) {
				String tempType = (String) iter.next();
				
//				String tempType = ((JavaIdentifier)tokens.get(base + 2)).value;
				String tempName = ((JavaIdentifier)tokens.get(base + 6-3)).value;
//				tokens.remove(base+1-4);
				tokens.add(base+1-4, new JavaKeyword("instanceof"));
				tokens.remove(base + 3-4);
				tokens.add(base + 3-4, new JavaParenthesisEnd());
				tokens.remove(base + 6-4);
				tokens.remove(base + 5-4);
				tokens.add(base + 5-4, new JavaType(tempType));
				tokens.add(base + 6-4, new JavaIdentifier(tempName));
				tokens.add(base + 7-4, new JavaAssignment());
				tokens.add(base + 8-4, new JavaCast(tempType));
				tokens.add(base + 9-4, new JavaIdentifier(sourceName));

			}

			endOfIntoBlock = javaMethod.methodBody.findEndOfBlock(base + 4-4);
	
			if (!firstIntoBlock) {
				// 2nd or later castInto
				tokens.add(base+1-4, new JavaKeyword("else"));
				tokens.add(base+2-4, new JavaKeyword("if"));
				tokens.add(base+3-4, new JavaParenthesisStart());
				tokens.add(base+4-4, new JavaIdentifier(sourceName));
				endOfIntoBlock += 4;
			}
			firstIntoBlock = false;
			
			call = call.substring("castinto".length());
			base = endOfIntoBlock;
		}
		if (call.equals("others")) {
			JavaCallArgumentSeparator separator = (JavaCallArgumentSeparator)tokens.get(endOfIntoBlock + 1);
			tokens.remove(endOfIntoBlock + 1);
			tokens.add(endOfIntoBlock + 1, new JavaKeyword("else"));
			int endOfOthers = javaMethod.methodBody.findEndOfBlock(endOfIntoBlock + 2);
			JavaCallEnd callEnd = (JavaCallEnd)tokens.get(endOfOthers + 1);
			JavaStatementTerminator terminator = (JavaStatementTerminator)tokens.get(endOfOthers + 2);
			tokens.remove(endOfOthers + 2);
			tokens.remove(endOfOthers + 1);
		} else {
			tokens.remove(endOfIntoBlock + 2);
			tokens.remove(endOfIntoBlock + 1);
		}
		
		if (sourceIsIdentifier) {
			tokens.add(i, new JavaKeyword("if"));
			tokens.add(i + 1, new JavaParenthesisStart());
		} else {
			int statementStart = javaMethod.methodBody.findStartOfExpression(i);
			tokens.add(i+1, new JavaKeyword("if"));
			tokens.add(i+2, new JavaParenthesisStart());
			tokens.add(i+3, new JavaIdentifier(sourceName));
			
			tokens.add(i+1, new JavaStatementTerminator());
			tokens.add(statementStart, new JavaType("Heaper"));
			tokens.add(statementStart+1, new JavaIdentifier(sourceName));
			tokens.add(statementStart+2, new JavaAssignment());
		}
		return i;
	}
	
	private int readTypes(List tokens, int typeStart, List types) {
			int typeEnd;
			if (tokens.get(typeStart) instanceof JavaIdentifier) {
				String tempType = ((JavaIdentifier)tokens.get(typeStart)).value;
				types.add(tempType);
				typeEnd = typeStart;
			} else if (tokens.get(typeStart) instanceof JavaParenthesisStart) {
				int j = typeStart+1;
				while (!(tokens.get(j) instanceof JavaParenthesisEnd)) {
					if (tokens.get(j) instanceof JavaIdentifier) {
						String tempType = ((JavaIdentifier)tokens.get(j)).value;
						types.add(tempType);
					} else if (((JavaToken)tokens.get(j)).value.equals("|")) {
						// ignore
					} else {
						throw new IllegalStateException("Failed to parse types of castinto");
					}
				}
				typeEnd = j + 1;
			} else {
				throw new IllegalStateException("Failed to recognize start of types for castinto");
			}
			return typeEnd;
		}

}
