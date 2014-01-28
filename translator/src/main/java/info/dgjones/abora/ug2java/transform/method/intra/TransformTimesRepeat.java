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
import info.dgjones.abora.ug2java.javatoken.IntegerLiteral;
import info.dgjones.abora.ug2java.javatoken.JavaAssignment;
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
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
		tokens.add(start + 5, new IntegerLiteral(0));
		tokens.add(start + 6, new JavaLoopTerminator());
		tokens.add(start + 7, new JavaIdentifier(incrementVariable));
		tokens.add(start + 8, new JavaKeyword("<"));
		
		return i;
	}
}
