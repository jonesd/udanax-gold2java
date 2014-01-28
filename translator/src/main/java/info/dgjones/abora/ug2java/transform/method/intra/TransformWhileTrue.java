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
import info.dgjones.abora.ug2java.javatoken.JavaBlockEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformWhileTrue extends AbstractMethodBodyTransformation {

	public TransformWhileTrue() {
		super();
	}
	public TransformWhileTrue(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaStatementTerminator.class), 
				factory.token(JavaBlockEnd.class),
				factory.any(
						factory.token(JavaCallStart.class, "whileTrue|whileFalse"),
						factory.token(JavaIdentifier.class, "whileTrue|whileFalse")
						));
	}
	
	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaToken call = (JavaToken)tokens.get(i+2);
		boolean isCall = call instanceof JavaCallStart;
		boolean isWhileTrue = call.value.equals("whileTrue");
		int preBlockStart = javaMethod.methodBody.findStartOfBlock(i + 1);
		tokens.add(preBlockStart, new JavaKeyword("while"));
		tokens.remove(preBlockStart + 1);
		tokens.add(preBlockStart + 1, new JavaParenthesisStart());
		tokens.remove(i + 1); // ;
		tokens.remove(i + 1); // }					
		tokens.add(i + 1, new JavaParenthesisEnd());
		if (isCall) {
			int postCallEnd = javaMethod.methodBody.findClosingCallEnd(i + 2);
			if (postCallEnd + 1 < tokens.size() && (tokens.get(postCallEnd + 1) instanceof JavaStatementTerminator)) {
				tokens.remove(postCallEnd + 1);
			}
			tokens.remove(postCallEnd);
		}
		tokens.remove(i+2);
		
		if (!isWhileTrue) {
			tokens.add(i+2, new JavaParenthesisEnd());
			tokens.add(preBlockStart+1, new JavaParenthesisStart());
			tokens.add(preBlockStart+2, new JavaKeyword("!"));
		}
		
		return i;
	}
}
