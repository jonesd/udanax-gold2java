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
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class RenameCall extends AbstractMethodBodyTransformation {

	static final Map RENAME_CALLS;
	static {
		Map map = new HashMap();
		map.put("show", "print");
		map.put("cr", "println");
		RENAME_CALLS = Collections.unmodifiableMap(map);
	}
	
public RenameCall() {
		super();
	}
	public RenameCall(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class, regularExpressionOr(RENAME_CALLS.keySet()));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		String newCall = (String)RENAME_CALLS.get(call.value);
		if (newCall == null) {
			throw new IllegalStateException("Failed to find expected match for rename:"+this);
		}
		call.value = newCall;
		return i;
	}
}
