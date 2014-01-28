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

import java.util.List;

import info.dgjones.abora.ug2java.JavaClass;
import info.dgjones.abora.ug2java.JavaCodebase;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaAssignment;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformDowncastCallResult extends AbstractMethodBodyTransformation {

	public TransformDowncastCallResult() {
		super();
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCodebase javaCodebase = javaMethod.javaClass.javaCodebase;

		int base = i;
		boolean onlyStatic;

		JavaClass callerClass;
		if (i > 0 && tokens.get(i-1) instanceof JavaIdentifier) {
			String className = ((JavaIdentifier)tokens.get(i-1)).value;
			base = i - 1;
			callerClass = javaCodebase.getJavaClass(className);
			if (callerClass == null) {
				String varTypeName = javaMethod.findTypeOfVariable(className);
				callerClass = javaCodebase.getJavaClass(varTypeName);
				if (callerClass == null) {
					return i;
				} else {
					onlyStatic = false;
				}
			} else {
				onlyStatic = true;
			}
		} else if (javaMethod.isStatic()){
			callerClass = javaMethod.javaClass;
			onlyStatic = true;
		} else {
			return i;
		}
		
		String tempTypeName = null;
		
		if (base > 1 && tokens.get(base-2) instanceof JavaIdentifier && tokens.get(base-1) instanceof JavaAssignment) {
			String tempName = ((JavaIdentifier)tokens.get(base-2)).value;
			tempTypeName = javaMethod.findTypeOfVariable(tempName);
		} else if (base > 0 && tokens.get(base-1) instanceof JavaKeyword && ((JavaKeyword)tokens.get(base-1)).value.equals("return")) {
			tempTypeName = javaMethod.returnType;
		}

		if (tempTypeName == null) {
			return i;
		}

		String callName = ((JavaCallStart)tokens.get(i)).value;
		
						
		JavaClass tempType = javaCodebase.getJavaClass(tempTypeName);
		if (tempType == null && !javaMethod.getJavaCodebase().isPrimitiveType(tempTypeName)) {
			return i;
		}
		
		int args = javaMethod.methodBody.findNumberOfCallArgs(i);
		
		String returnTypeName = callerClass.findMatchingMethodReturnType(callName, args, onlyStatic);
		//TODO would prefer this to the above special case method, but not good enough at the moment to find the
		// single best matching method, so need to rely on merging in methods with the same return code
//		JavaMethod method = callerClass.findMatchingMethod(callName, args, onlyStatic);
//		if (method == null) {
//			return i;
//		}
//		String returnTypeName = method.returnType; 

		if (javaMethod.getJavaCodebase().shouldDowncast(returnTypeName, tempTypeName)) {
			tokens.add(base, new JavaCast(tempTypeName));
		}
		
		return i;
	}
	
}
