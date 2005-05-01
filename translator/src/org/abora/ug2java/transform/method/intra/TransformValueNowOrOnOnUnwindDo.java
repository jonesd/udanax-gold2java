/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformValueNowOrOnOnUnwindDo extends AbstractMethodBodyTransformation {

	public TransformValueNowOrOnOnUnwindDo() {
		super();
	}
	public TransformValueNowOrOnOnUnwindDo(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaBlockEnd.class), 
				factory.token(JavaCallKeywordStart.class, "valueNowOrOnUnwindDo"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		boolean hasBlockDo = tokens.get(i+2) instanceof JavaBlockStart; 
		int start = javaMethod.methodBody.findStartOfBlock(i);
		int postCallEnd = javaMethod.methodBody.findClosingCallEnd(i + 1);
		if (postCallEnd + 1 < tokens.size() && (tokens.get(postCallEnd + 1) instanceof JavaStatementTerminator)) {
			tokens.remove(postCallEnd + 1);
		}
		tokens.remove(postCallEnd);
		if (!hasBlockDo) {
			tokens.add(postCallEnd, new JavaStatementTerminator());
			tokens.add(postCallEnd+1, new JavaBlockEnd());
			tokens.add(i+2, new JavaBlockStart());
		}
		tokens.remove(i + 1);
		tokens.add(i + 1, new JavaKeyword("finally"));
		tokens.add(start, new JavaKeyword("try"));
		
		return i;
	}
}
