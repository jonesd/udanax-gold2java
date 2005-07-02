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
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCharacterCalls extends AbstractMethodBodyTransformation {

	private static final Set CALLS;
	static {
		Set set = new HashSet();
		set.add("isSeparator");
		CALLS = Collections.unmodifiableSet(set);
	}
	
	public TransformCharacterCalls() {
		super();
	}
	public TransformCharacterCalls(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
			factory.token(JavaCallStart.class, this.regularExpressionOr(CALLS)),
			factory.token(JavaCallEnd.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier var = (JavaIdentifier)tokens.get(i);
		String varName = var.value;
		if (!"char".equals(javaMethod.findTypeOfVariable(varName))) {
			return i;
		}
		
		var.value = "AboraCharacterSupport";
		tokens.add(i+2, new JavaIdentifier(varName));
		return i;
	}
}
