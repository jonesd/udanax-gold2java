/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.ClassParser;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformBlast extends AbstractMethodBodyTransformation {

	
public TransformBlast() {
		super();
	}
	public TransformBlast(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "Heaper"), 
				factory.token(JavaCallKeywordStart.class, "BLAST"),
				factory.token(JavaIdentifier.class), 
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallKeywordStart call = (JavaCallKeywordStart)tokens.get(i + 1);
		JavaIdentifier message = (JavaIdentifier)tokens.get(i + 2);
		tokens.remove(i);
		tokens.add(i, new JavaKeyword("throw"));
		tokens.add(i + 1, new JavaKeyword("new"));
		call.value = ClassParser.ABORA_RUNTIME_EXCEPTION_CLASS;
		javaMethod.javaClass.includeImportForType(call.value);
		message.value = ClassParser.ABORA_RUNTIME_EXCEPTION_CLASS+"." + message.value;
		return i;
	}
}
