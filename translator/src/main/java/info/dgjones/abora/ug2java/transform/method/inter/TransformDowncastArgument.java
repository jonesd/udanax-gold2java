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
package info.dgjones.abora.ug2java.transform.method.inter;

import java.util.ArrayList;
import java.util.List;

import info.dgjones.abora.ug2java.JavaClass;
import info.dgjones.abora.ug2java.JavaCodebase;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformDowncastArgument extends AbstractMethodBodyTransformation {

	public TransformDowncastArgument() {
		super();
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
//		return factory.seq(
				//TODO simple first case
//				factory.token(JavaKeyword.class, "return"),
return				factory.token(JavaCallKeywordStart.class);
//				factory.token(JavaIdentifier.class),
//				factory.token(JavaCallEnd.class)
			//);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaClass receiverClass = javaMethod.javaClass;
		
		if (i > 0 && tokens.get(i-1) instanceof JavaIdentifier) {
			JavaIdentifier callSource = (JavaIdentifier)tokens.get(i-1);
			receiverClass = javaMethod.getJavaCodebase().getJavaClass(callSource.value);
			if (receiverClass == null) {
				return i;
			}
		}

		String callName = ((JavaCallStart)tokens.get(i)).value;
		int callEnd = javaMethod.methodBody.findClosingCallEndQuietFail(i);
		//TODO why do we need to use the failQuiet version?
		if (callEnd == -1) {
			return i;
		}

		JavaCodebase javaCodebase = javaMethod.javaClass.javaCodebase;

		List argumentTypes = new ArrayList();
		List argumentStarts = new ArrayList();

		int j = i + 1;
		while (j < callEnd) {
			//TODO Simple find type of expression code - extract
			argumentStarts.add(new Integer(j));
			String argumentTypeName = null;
			
			boolean wrappedInParanthesis = false;
			JavaToken token = (JavaToken)tokens.get(j);
			if (token instanceof JavaParenthesisStart) {
				wrappedInParanthesis = true;
				j += 1;
				token = (JavaToken)tokens.get(j);
			}
			if (token instanceof JavaCast) {
				argumentTypeName = token.value;
				j += 1;
				token = (JavaToken)tokens.get(j);
			}
			if (!(token instanceof JavaIdentifier)) {
				return i;
			}

			j += 1;
			if (wrappedInParanthesis) {
				JavaToken token2 = (JavaToken)tokens.get(j);
				if (!(token2 instanceof JavaParenthesisEnd)) {
					return i;
				}
				j += 1;
			}
			if (j < callEnd - 1) {
				JavaToken token2 = (JavaToken)tokens.get(j);
				if (!(token2 instanceof JavaCallArgumentSeparator)) {
					return i;
				}
				j += 1;
			}
			if (argumentTypeName == null) {
				String argumentName = token.value;
				argumentTypeName = javaMethod.findTypeOfVariable(argumentName);
				if (argumentTypeName == null) {
					return i;
				}
			}
			if (!javaCodebase.isPrimitiveType(argumentTypeName)) {
				JavaClass argumentType = javaCodebase.getJavaClass(argumentTypeName);
				if (argumentType == null) {
					return i;
				}
			}
			argumentTypes.add(argumentTypeName);
		}
		
		JavaMethod method = receiverClass.findMatchingMethod(callName, argumentTypes.size(), false); 
		if (method == null) {
			return i;
		}

		for (int k = argumentTypes.size() - 1; k >= 0; k -= 1) {
			String expectedTypeName = method.getParameter(k).type;
			String actualTypeName = (String)argumentTypes.get(k);
			if (javaCodebase.shouldDowncast(actualTypeName, expectedTypeName)) {
				int p = ((Integer)argumentStarts.get(k)).intValue();
				tokens.add(p, new JavaCast(expectedTypeName));
			}
		}
				
		return i;
	}
}
