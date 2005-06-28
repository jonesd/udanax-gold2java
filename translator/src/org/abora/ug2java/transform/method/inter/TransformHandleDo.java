/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.inter;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.abora.ug2java.Annotation;
import org.abora.ug2java.ClassParser;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



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
