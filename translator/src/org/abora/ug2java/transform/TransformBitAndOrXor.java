/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformBitAndOrXor extends AbstractMethodBodyTransformation {

	public TransformBitAndOrXor() {
		super();
	}
	public TransformBitAndOrXor(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.any(
				factory.token(JavaCallKeywordStart.class, "bitAnd"), 
				factory.token(JavaCallKeywordStart.class, "bitOr"), 
				factory.token(JavaCallKeywordStart.class, "bitXor"),
				factory.token(JavaCallKeywordStart.class, "bitShift"),
				factory.token(JavaCallKeywordStart.class, "bitShiftRight"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallKeywordStart token = (JavaCallKeywordStart)tokens.get(i);
		int closingIndex = javaMethod.methodBody.findClosingCallEnd(i);
		tokens.remove(closingIndex);
		tokens.remove(i);
		if (token.value.equals("bitAnd")) {
			tokens.add(i, new JavaKeyword("&"));
		} else if (token.value.equals("bitOr")) {
			tokens.add(i, new JavaKeyword("|"));
		} else if (token.value.equals("bitXor")){
			tokens.add(i, new JavaKeyword("^"));
		} else if (token.value.equals("bitShift")){
			tokens.add(i, new JavaKeyword("<<"));
		} else if (token.value.equals("bitShiftRight")){
			tokens.add(i, new JavaKeyword(">>")); //TODO or >>> 
		} else {
			throw new IllegalStateException("Unknown key?");
		}
		return i;
	}
}
