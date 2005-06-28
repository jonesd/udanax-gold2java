/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.inter;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.abora.ug2java.Annotation;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



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
