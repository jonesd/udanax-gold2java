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
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;

public class TransformUnreachableCode extends AbstractMethodBodyTransformation {
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.any(
				factory.token(JavaKeyword.class, "throw"),
				factory.token(JavaKeyword.class, "return"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		
		int end = javaMethod.methodBody.findNextTokenOfTypeQuietFail(i, JavaStatementTerminator.class);
		if (end == -1) {
			return i;
		}
		int nextBlockStart = javaMethod.methodBody.findNextTokenOfTypeQuietFail(i, JavaBlockStart.class);
		if (nextBlockStart != -1 && nextBlockStart < end) {
			end = javaMethod.methodBody.findEndOfBlock(nextBlockStart);
		}
		int endOfBlock;
		if (javaMethod.methodBody.findPreviousTokenOfTypeQuietFail(i, JavaBlockStart.class) == -1) {
			// top level - so can remove the rest of the method
			endOfBlock = tokens.size();
		} else {
			endOfBlock = javaMethod.methodBody.findEndOfBlockQuietFail(i);
			if (endOfBlock == -1) {
				endOfBlock = tokens.size();
			}
		}
		for (int j = endOfBlock - 1; j > end; --j) {
			tokens.remove(j);
		}
		return i;
	}

}