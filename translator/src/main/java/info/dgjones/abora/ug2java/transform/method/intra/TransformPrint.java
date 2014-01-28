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
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaComment;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformPrint extends AbstractMethodBodyTransformation {


	public TransformPrint() {
		super();
	}
	public TransformPrint(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaKeyword.class, "<<|;");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaKeyword call = (JavaKeyword)tokens.get(i);
		int endOfBlock = javaMethod.methodBody.findEndOfExpression(i+1);
		for (int j = i + 2; j < endOfBlock; j ++) {
			JavaToken token = (JavaToken)tokens.get(j);
			if ((token instanceof JavaKeyword) && (token.value.equals("<<") || token.value.equals(";"))) {
				endOfBlock = j - 1;
			}
		}
		if (call.value.equals("<<")) {
			tokens.add(endOfBlock+1, new JavaCallEnd());
		}
			tokens.remove(i);
		if (call.value.equals("<<")) {
			tokens.add(i, new JavaCallKeywordStart("print"));
		}
		//Watch out for immediately proceeding comment TODO would prefer to keep it
		if (tokens.get(i-1) instanceof JavaComment) {
			tokens.remove(i-1);
			i -= 1;
		}
		int startOfBlock = javaMethod.methodBody.findStartOfExpressionMinimal(i-1);
		//TODO special comment handling...
		if (startOfBlock + 2 < i && tokens.get(startOfBlock) instanceof JavaComment) {
				startOfBlock += 1;
		}
		if (startOfBlock + 2 < i && tokens.get(startOfBlock) instanceof JavaIdentifier) {
		
			JavaIdentifier identifier = (JavaIdentifier)tokens.get(startOfBlock);
			tokens.add(i, new JavaStatementTerminator());
			tokens.add(i + 1, new JavaIdentifier(identifier.value));
			// watch out for "AboraSupport.logger." case
			if (tokens.get(startOfBlock+1) instanceof JavaIdentifier) {
				JavaIdentifier identifier2 = (JavaIdentifier)tokens.get(startOfBlock+1);
				tokens.add(i+2, new JavaIdentifier(identifier2.value));
			}
		}
		
		return i;
	}
}
