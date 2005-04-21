/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;

public class TransformUnaryOperator extends AbstractMethodBodyTransformation {

	private static final Map operators;
	static {
		Map map = new HashMap();
		map.put("not", "!");
		map.put("bitInvert", "~");
		map.put("negated", "-");
		operators = Collections.unmodifiableMap(map);
	}

	private static final Map operatorCasts;
	static {
		Map map = new HashMap();
		map.put("asDouble", "double");
		map.put("asFloat", "float");
		map.put("asInteger", "int");
		operatorCasts = Collections.unmodifiableMap(map);
	}

	public TransformUnaryOperator() {
		super();
	}

	public TransformUnaryOperator(TokenMatcherFactory factory) {
		super(factory);
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallStart.class, regularExpressionOr(operators.keySet(), operatorCasts.keySet())),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		String smalltalkCall = ((JavaCallStart)tokens.get(i)).value;
		int expressionStart = javaMethod.methodBody.findStartOfExpressionMinimal(i-1);
		tokens.remove(i + 1);
		tokens.remove(i);
		JavaToken operator;
		String match = (String)operators.get(smalltalkCall);
		if (match != null) {
			operator = new JavaKeyword(match);
		} else {
			match = (String)operatorCasts.get(smalltalkCall);
			operator = new JavaCast(match);
		}
		tokens.add(expressionStart, operator);
		
		return i;
	}
}