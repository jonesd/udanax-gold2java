/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.ClassParser;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.transform.tokenmatcher.MatchAny;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformOverrideCalls extends AbstractMethodBodyTransformation {

	public TransformOverrideCalls() {
		super();
	}
	public TransformOverrideCalls(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		MatchAny matchAny = new MatchAny();
		//TODO could be more efficient by matching just once on type, then go through all names
		for (Iterator iter = ClassParser.OVERRIDE_CALLS.keySet().iterator(); iter.hasNext();) {
			String call = (String) iter.next();
			matchAny.add(factory.token(JavaCallStart.class, call));
		}	
		return matchAny;
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		call.value = (String) ClassParser.OVERRIDE_CALLS.get(call.value);
	}
}
