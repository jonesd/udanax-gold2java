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

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import info.dgjones.abora.ug2java.Annotation;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformRaise extends AbstractMethodBodyTransformation {

	private static final Map HANDLES;
	static {
		Map map = new HashMap();
		map.put("TextyRcvr.blastIdentifierTooLong", "IDENTIFIER_TOO_LONG");
		map.put("Binary2Rcvr.blastInvalidCharacter", "INVALID_CHARACTER");
		map.put("TextyXmtr.blastInvalidCharacter", "INVALID_CHARACTER");
		HANDLES = Collections.unmodifiableMap(map);
	}
	
	public TransformRaise() {
		super();
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class),
				factory.token(JavaCallEnd.class),
				factory.token(JavaCallStart.class, "raise"),
				factory.token(JavaCallEnd.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {

		String problemsClassName = ((JavaIdentifier)tokens.get(i)).value;
		String problemsName = ((JavaCallStart)tokens.get(i+1)).value;

		String signal = null;
		
		//TODO use class inheritance to find these as well
		if (HANDLES.containsKey(problemsClassName+"."+problemsName)) {
			signal = (String)HANDLES.get(problemsClassName+"."+problemsName);
		} else {
			JavaMethod problemsMethod = javaMethod.getJavaCodebase().getJavaClass(problemsClassName).getMethodOrInherited(problemsName);
			if (problemsMethod == null) {
				System.out.println("--Failed to find signals match for raise: "+problemsClassName+"."+problemsName);
				return i;
			}
			
			Set allSignals = (Set)problemsMethod.getAnnotations().get(Annotation.PROBLEM_SIGNALS);
			if (allSignals == null || allSignals.isEmpty()) {
				System.out.println("--No registered signals found for raise: "+problemsClassName+"."+problemsName);
				return i;
			} else if (allSignals.size() > 1) {
				System.out.println("--More than one signal found for raise: "+problemsClassName+"."+problemsName);
				return i;
			}
			signal = (String)allSignals.iterator().next();
		}
		
		javaMethod.methodBody.remove(i, i+5);
		
		int j = i;
		tokens.add(j++, new JavaKeyword("throw"));
		tokens.add(j++, new JavaKeyword("new"));
		tokens.add(j++, new JavaCallKeywordStart("AboraRuntimeException"));
		tokens.add(j++, new JavaIdentifier("AboraRuntimeException"));
		tokens.add(j++, new JavaIdentifier(signal));
		tokens.add(j++, new JavaCallEnd());

		return i;
	}

}
