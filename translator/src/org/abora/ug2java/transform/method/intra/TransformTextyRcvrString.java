/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * Transformation due to incorrect original source code within HashSetTester.test2On 
 */
public class TransformTextyRcvrString extends AbstractMethodBodyTransformation {

	public TransformTextyRcvrString() {
		super();
	}
	public TransformTextyRcvrString(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaKeyword.class, "new"),
				factory.token(JavaCallKeywordStart.class, "String"),
				factory.token(JavaIdentifier.class, "ReceiveStringBufferSize"),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getQualifiedName().equals("TextyRcvr.linkTimeNonInherited")) {
			return i;
		}
		JavaCallKeywordStart call = (JavaCallKeywordStart)tokens.get(i+1);
		call.value = "UInt8Array";
		return i;
	}
}
