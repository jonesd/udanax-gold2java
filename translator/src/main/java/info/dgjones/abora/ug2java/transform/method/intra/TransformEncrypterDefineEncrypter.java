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
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * TODO this class only exists because of limitations of choosing transformOnly with a String
 */
public class TransformEncrypterDefineEncrypter extends AbstractMethodBodyTransformation {

	public TransformEncrypterDefineEncrypter() {
		super();
	}
	public TransformEncrypterDefineEncrypter(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaParenthesisStart.class),
				factory.token(JavaIdentifier.class, "Smalltalk"),
				factory.token(JavaCallKeywordStart.class, "at"),
				factory.token(JavaIdentifier.class, "className"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaParenthesisEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getQualifiedName().equals("Encrypter.DEFINEUENCRYPTER")) {
			return i;
		}
		
		tokens.add(i+6, new JavaCallEnd());
		tokens.add(i, new JavaIdentifier("EncrypterConstructor"));
		tokens.add(i+1, new JavaCallKeywordStart("make"));
		
		return i+2;
	}
}
