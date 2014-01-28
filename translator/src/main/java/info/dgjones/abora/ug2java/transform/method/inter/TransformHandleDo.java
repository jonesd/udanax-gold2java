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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import info.dgjones.abora.ug2java.Annotation;
import info.dgjones.abora.ug2java.ClassParser;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaBlockEnd;
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaType;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformHandleDo extends AbstractMethodBodyTransformation {

	private static final Map HANDLES;
	static {
		Map map = new HashMap();
		map.put("TextyRcvr.blastIdentifierTooLong", "IDENTIFIER_TOO_LONG");
		HANDLES = Collections.unmodifiableMap(map);
	}
	
	public TransformHandleDo() {
		super();
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallKeywordStart.class, "handleDo"),
				factory.token(JavaBlockStart.class),
				factory.token(JavaType.class, "Object"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaStatementTerminator.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {

//		factory.token(JavaIdentifier.class),
//		factory.token(JavaCallStart.class, "problems.*"),
//		factory.token(JavaCallEnd.class),
		
		int ii = i - 3;
		if (tokens.get(i - 1) instanceof JavaParenthesisEnd) {
			ii -= 1;
		}

		String problemsClassName = ((JavaIdentifier)tokens.get(ii)).value;
		String problemsName = ((JavaCallStart)tokens.get(ii+1)).value;

		if (tokens.get(i - 1) instanceof JavaParenthesisEnd) {
			ii -= 1;
		}

		boolean allBlasts = false;
		Set signals = null;
		
		if (problemsClassName.equals("Heaper") && problemsName.equals("problemsAllBlasts")) {
			allBlasts = true;
		} else if (HANDLES.containsKey(problemsClassName+"."+problemsName)) {
			signals = new HashSet();
			signals.add(HANDLES.get(problemsClassName+"."+problemsName));
		} else {
			JavaMethod problemsMethod = javaMethod.getJavaCodebase().getJavaClass(problemsClassName).getMethodOrInherited(problemsName);
			if (problemsMethod == null) {
				System.out.println("--Failed to find signals match for handleDo: "+problemsClassName+"."+problemsName);
				return i;
			}
			
			signals = (Set)problemsMethod.getAnnotations().get(Annotation.PROBLEM_SIGNALS);
			if (signals == null || signals.isEmpty()) {
				System.out.println("--No registered signals found for handleDo: "+problemsClassName+"."+problemsName);
				return i;
			}
		}
		
		int handleBlockStart = i + 1;
		int handleBlockEnd = javaMethod.methodBody.findEndOfBlock(handleBlockStart);
		javaMethod.methodBody.shouldMatch(handleBlockEnd+1, JavaCallArgumentSeparator.class);
		int bodyBlockStart = handleBlockEnd+2;
		javaMethod.methodBody.shouldMatch(handleBlockStart, JavaBlockStart.class);
		int bodyBlockEnd = javaMethod.methodBody.findEndOfBlock(bodyBlockStart);
		javaMethod.methodBody.shouldMatch(bodyBlockEnd+1, JavaCallEnd.class);
		javaMethod.methodBody.shouldMatch(bodyBlockEnd+2, JavaStatementTerminator.class);
		
		tokens.remove(bodyBlockEnd+2);
		tokens.remove(bodyBlockEnd+1);
		
		int j = bodyBlockEnd+1;
		tokens.add(j++, new JavaKeyword("catch"));
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaType(ClassParser.ABORA_RUNTIME_EXCEPTION_CLASS));
		tokens.add(j++, new JavaIdentifier("ex"));
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.add(j++, new JavaBlockStart());
		
		if (!allBlasts) {
			tokens.add(j++, new JavaKeyword("if"));
			tokens.add(j++, new JavaParenthesisStart());
	
			for (Iterator iter = signals.iterator(); iter.hasNext();) {
				String problem = (String) iter.next();
				tokens.add(j++, new JavaIdentifier(ClassParser.ABORA_RUNTIME_EXCEPTION_CLASS));
				tokens.add(j++, new JavaIdentifier(problem));
				tokens.add(j++, new JavaCallKeywordStart("equals"));;
				tokens.add(j++, new JavaIdentifier("ex"));
				tokens.add(j++, new JavaCallStart("getMessage"));
				tokens.add(j++, new JavaCallEnd());
				tokens.add(j++, new JavaCallEnd());
				if (iter.hasNext()) {
					tokens.add(j++, new JavaKeyword("||"));
				}
			}
			tokens.add(j++, new JavaCallEnd());
			tokens.add(j++, new JavaBlockStart());
		}
		
		for (int k = i + 5; k < handleBlockEnd; ++k) {
			tokens.add(j++, tokens.get(k));
		}
		
		if (!allBlasts) {
			tokens.add(j++, new JavaBlockEnd());
			tokens.add(j++, new JavaKeyword("else"));
			tokens.add(j++, new JavaBlockStart());
			tokens.add(j++, new JavaKeyword("throw"));
			tokens.add(j++, new JavaIdentifier("ex"));
			tokens.add(j++, new JavaStatementTerminator());
			tokens.add(j++, new JavaBlockEnd());
		}
		tokens.add(j++, new JavaBlockEnd());
		
		for (int k = bodyBlockStart - 1; k >= ii; --k) {
			tokens.remove(k);
		}
		
		tokens.add(ii, new JavaKeyword("try"));

		return i;
	}

}
