/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.ClassParser;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSignals extends AbstractMethodBodyTransformation {

	public TransformSignals() {
		super();
	}
	public TransformSignals(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaKeyword.class, "return"),
				factory.token(JavaCallKeywordStart.class, "signals"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallEnd.class)
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i);
		JavaCallKeywordStart signals = (JavaCallKeywordStart)tokens.get(i);
		JavaIdentifier name = (JavaIdentifier)tokens.get(i+1);
		tokens.add(i, new JavaKeyword("throw"));
		tokens.add(i+1, new JavaKeyword("new"));
		signals.value = ClassParser.ABORA_RUNTIME_EXCEPTION_CLASS;
		//TODO parsing problem...
		if (name.value.startsWith("(")) {
			name.value = name.value.substring(1);
		}
		name.value = ClassParser.ABORA_RUNTIME_EXCEPTION_CLASS+"."+name.value;
		javaMethod.javaClass.includeImportForType(ClassParser.ABORA_RUNTIME_EXCEPTION_CLASS);
		
		return i;
	}
}
