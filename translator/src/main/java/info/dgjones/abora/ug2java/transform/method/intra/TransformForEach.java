/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.ug2java.transform.method.intra;

import java.util.List;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaAssignment;
import info.dgjones.abora.ug2java.javatoken.JavaBlockEnd;
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaLoopTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaType;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;


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
