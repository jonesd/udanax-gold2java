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
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaComment;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformConditionalOperator extends AbstractMethodBodyTransformation {

	public TransformConditionalOperator() {
		super();
	}
	public TransformConditionalOperator(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
						factory.token(JavaKeyword.class, "if"),
						factory.token(JavaParenthesisStart.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (i == 0) {
			return i;
		}
		for (int j = i -1; j >= 0; j--) {
			JavaToken preToken = (JavaToken)tokens.get(j);
			if (preToken instanceof JavaStatementTerminator || preToken instanceof JavaBlockEnd || preToken instanceof JavaBlockStart || (preToken instanceof JavaKeyword && preToken.value.equals("else"))) {
				// leave as normal if statement
				return i;
			} else if (!(preToken instanceof JavaComment)) {
				break;
			}
			if (j == 0) {
				return i;
			}
		}
		int ifBlockStart = javaMethod.methodBody.findNextTokenOfTypeQuietFail(i, JavaBlockStart.class);
		if (ifBlockStart == -1) {
			return i;
		}
		int ifBlockEnd = javaMethod.methodBody.findEndOfBlockQuietFail(ifBlockStart);
		if (ifBlockEnd != -1 && ifBlockEnd < tokens.size() - 1) {
			JavaToken elseKeyword = (JavaToken)tokens.get(ifBlockEnd+1);
			if (elseKeyword instanceof JavaKeyword && elseKeyword.value.equals("else")) {
				int elseBlockStart = javaMethod.methodBody.findNextTokenOfTypeQuietFail(ifBlockEnd, JavaBlockStart.class);
				if (elseBlockStart == -1) {
					return i;
				}
				int elseBlockEnd = javaMethod.methodBody.findEndOfBlockQuietFail(elseBlockStart);
				if (elseBlockEnd == -1) {
					return i;
				}
				javaMethod.methodBody.removeShouldMatch(elseBlockEnd, JavaBlockEnd.class);
				if (elseBlockEnd < tokens.size() && tokens.get(elseBlockEnd-1) instanceof JavaStatementTerminator) {
					JavaToken following = (JavaToken)tokens.get(elseBlockEnd);
					if (following instanceof JavaParenthesisEnd || following instanceof JavaCallEnd || following instanceof JavaCallArgumentSeparator) {
						tokens.remove(elseBlockEnd-1);
					}
				}
				javaMethod.methodBody.removeShouldMatch(elseBlockStart, JavaBlockStart.class);
				javaMethod.methodBody.removeShouldMatch(elseBlockStart-1, JavaKeyword.class, "else");
				javaMethod.methodBody.removeShouldMatch(elseBlockStart-2, JavaBlockEnd.class);
				javaMethod.methodBody.remove(elseBlockStart-3);//TODOelseShouldMatch(elseBlockStart-3, JavaStatementTerminator.class|JavaBlockEnd.class);
				tokens.add(elseBlockStart-3, new JavaKeyword(":"));
				javaMethod.methodBody.removeShouldMatch(ifBlockStart, JavaBlockStart.class);
				tokens.add(ifBlockStart, new JavaKeyword("?"));
				javaMethod.methodBody.removeShouldMatch(i, JavaKeyword.class, "if");
			}
		}
		return i;
	}
}
