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
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformExcessParantheses extends AbstractMethodBodyTransformation {

	public TransformExcessParantheses() {
		super();
	}
	public TransformExcessParantheses(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaParenthesisEnd.class), 
				factory.any(
						factory.token(JavaStatementTerminator.class),
						factory.token(JavaBlockEnd.class))); 
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int start = javaMethod.methodBody.findOpeningTokenOfType(i, JavaParenthesisStart.class);
		if (start > 0) {
			JavaToken preStart = (JavaToken)tokens.get(start-1);
			if (! (preStart instanceof JavaStatementTerminator || preStart instanceof JavaBlockEnd || preStart instanceof JavaBlockStart)) {
				return i;
			}
		}
		tokens.remove(i);
		tokens.remove(start);
		return i;
	}
}
