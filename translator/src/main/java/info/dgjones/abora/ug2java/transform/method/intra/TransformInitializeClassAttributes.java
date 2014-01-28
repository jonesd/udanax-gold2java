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
import info.dgjones.abora.ug2java.javatoken.JavaArrayInitializerStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.javatoken.StringLiteral;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformInitializeClassAttributes extends AbstractMethodBodyTransformation {

	public TransformInitializeClassAttributes() {
		super();
	}
	public TransformInitializeClassAttributes(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaParenthesisStart.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "getOrMakeCxxClassDescription"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaParenthesisEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getName().equals("initializeClassAttributes")) {
			return i;
		}
		
		javaMethod.methodBody.remove(i, i+5/*12*/);
		
		if (((JavaToken)tokens.get(i)).value.equals("friends")) {
			int end = javaMethod.methodBody.findClosingCallEnd(i);
			javaMethod.methodBody.remove(i, end+1);
		}
		javaMethod.methodBody.remove(i, i+7);
		
		
		int j = i;
		tokens.add(j++, new JavaIdentifier("AboraSupport"));
		tokens.add(j++, new JavaCallKeywordStart("findAboraClass"));
		tokens.add(j++, new JavaIdentifier(javaMethod.javaClass.className));
		tokens.add(j++, new JavaIdentifier("class"));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaCallKeywordStart("setAttributes"));
		tokens.add(j++, new JavaKeyword("new"));
		tokens.add(j++, new JavaCallStart("Set"));
		tokens.add(j++, new JavaCallEnd());
		
		while (j < tokens.size()) {
			JavaToken token = (JavaToken)tokens.get(j);
			if (token instanceof JavaIdentifier) {
				tokens.remove(j);
				tokens.add(j, new StringLiteral(token.value));
			} else if (token instanceof JavaKeyword && token.value.equals(";")) {
				tokens.remove(j);
				j -= 1;
			} else if (token.value.equals("yourself")) {
				tokens.remove(j+1);
				tokens.remove(j);
				j -= 1;
			} else if (token instanceof JavaParenthesisEnd) {
				tokens.remove(j);
				j -= 1;
			} else if (token instanceof JavaArrayInitializerStart) {
				//TODO shouldn't need to do this, as other transforms should set up string arrays properly...
				tokens.add(j, new JavaKeyword("new"));
				tokens.add(j+1, new JavaIdentifier("String[]"));
				j += 2;
			}
			j += 1;
		}
		
		return i;
	}
}
