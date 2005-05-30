/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSendHeaper extends AbstractMethodBodyTransformation {

	public TransformSendHeaper() {
		super();
	}
	public TransformSendHeaper(TokenMatcherFactory factory) {
		super(factory);
	}

	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "sendHeaper"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier variable = (JavaIdentifier)tokens.get(i+2);
		JavaCallStart call = (JavaCallStart)tokens.get(i+1);
		String type = javaMethod.findTypeOfVariable(variable.value);
		if (type == null) {
			return i;
		}
		if (type.equals("float") || type.equals("double")) {
			call.value = "sendIEEEDoubleVar";
			tokens.add(i+4, new JavaComment("TODO was sendHeaper"));
		}
		
		return i;
	}
}
