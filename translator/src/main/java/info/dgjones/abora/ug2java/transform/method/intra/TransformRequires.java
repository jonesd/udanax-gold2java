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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import info.dgjones.abora.ug2java.Annotation;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformRequires extends AbstractMethodBodyTransformation {

	public TransformRequires() {
		super();
	}
	public TransformRequires(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallKeywordStart.class, "REQUIRES");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		Set required = (Set)javaMethod.getAnnotations().get(Annotation.REQUIRES);
		if (required == null) {
			required = new HashSet();
			javaMethod.getAnnotations().put(Annotation.REQUIRES, required);
		}
		
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i);
		if (callEnd + 1 < tokens.size() && (tokens.get(callEnd + 1) instanceof JavaStatementTerminator)) {
			tokens.remove(callEnd + 1);
		}
		for (int j = i+1; j < callEnd; j++) {
			if (tokens.get(j) instanceof JavaIdentifier) {
				JavaIdentifier classIdentifier = (JavaIdentifier)tokens.get(j);
				if (javaMethod.getJavaCodebase().getJavaClass(classIdentifier.value) != null) {
					required.add(classIdentifier.value);
				}
			}
		}
		for (int j = callEnd; j >= i; j--) {
			tokens.remove(j);
		}
		
		return i-1;
	}
}
