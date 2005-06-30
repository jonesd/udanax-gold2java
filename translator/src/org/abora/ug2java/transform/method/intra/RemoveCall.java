/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class RemoveCall extends AbstractMethodBodyTransformation {

	static final Set REMOVE_CALLS;
	static {
		Set set = new HashSet();
		set.add("asSymbol");
		set.add("asString");
		REMOVE_CALLS = Collections.unmodifiableSet(set);
	}
	
public RemoveCall() {
		super();
	}
	public RemoveCall(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class, regularExpressionOrTrailing(REMOVE_CALLS));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		String shortCall = call.value;
		String methodCall = javaMethod.name+"."+shortCall;
		String fullCall = javaMethod.javaClass.className+"."+methodCall;
		if (REMOVE_CALLS.contains(fullCall) || REMOVE_CALLS.contains(methodCall) || REMOVE_CALLS.contains(shortCall)) {
			int callEnd = javaMethod.methodBody.findClosingCallEnd(i);
			for (int j = callEnd; j >= i; j--) {
				tokens.remove(j);
			}
		}
		return i;
	}
}
