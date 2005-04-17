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
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformBinaryOperator extends AbstractMethodBodyTransformation {

	private static final Map operators;
	static {
		Map map = new HashMap();
		map.put("bitAnd", "&");
		map.put("bitOr", "|");
		map.put("bitXor", "^");
		map.put("bitShift", "<<");
		map.put("bitShiftRight", ">>"); //TODO or >>>
		operators = Collections.unmodifiableMap(map);
	}
	
	public TransformBinaryOperator() {
		super();
	}
	public TransformBinaryOperator(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallKeywordStart.class, regularExpressionOr(operators.keySet()));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallKeywordStart token = (JavaCallKeywordStart)tokens.get(i);
		int closingIndex = javaMethod.methodBody.findClosingCallEnd(i);
		tokens.remove(closingIndex);
		tokens.remove(i);
		String operator = (String)operators.get(token.value);
		if (operator == null) {
			throw new IllegalStateException("Unknown key?");
		}
		tokens.add(i, new JavaKeyword(operator));
		return i;
	}
}
