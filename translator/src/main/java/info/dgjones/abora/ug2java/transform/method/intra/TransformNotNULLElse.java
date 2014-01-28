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
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaType;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



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
		
		int j = i;
		tokens.add(j++, new JavaStatementTerminator());

		boolean replacedAssignment = false;
		if (expressionStart > 1 && tokens.get(expressionStart-2) instanceof JavaIdentifier && tokens.get(expressionStart-1) instanceof JavaAssignment) {
			replacedAssignment = true;
			String variableName = ((JavaIdentifier)tokens.get(expressionStart-2)).value;
			tokens.add(j++, new JavaIdentifier(variableName));
			tokens.add(j++, new JavaAssignment());
		}

		tokens.add(j++, new JavaKeyword("if"));
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaIdentifier(tempName));
		tokens.add(j++, new JavaKeyword("!="));
		tokens.add(j++, new JavaKeyword("null"));
		tokens.add(j++, new JavaParenthesisEnd());
		
		tokens.add(expressionStart, new JavaType(tempType));
		tokens.add(expressionStart+1, new JavaIdentifier(tempName));
		tokens.add(expressionStart+2, new JavaAssignment());
		tokens.add(expressionStart+3, new JavaCast(tempType));
		
		if (replacedAssignment) {
			tokens.remove(expressionStart - 1);
			tokens.remove(expressionStart - 2);
		}
		return i;
	}
}
