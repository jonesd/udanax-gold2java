/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformDiskManagerConsistent extends AbstractMethodBodyTransformation {

	public TransformDiskManagerConsistent() {
		super();
	}
	public TransformDiskManagerConsistent(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "DiskManager"), 
				factory.token(JavaCallKeywordStart.class, "consistent"),
				factory.token(JavaLiteral.class),
				factory.token(JavaCallArgumentSeparator.class),
				factory.token(JavaBlockStart.class)
			);
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier diskManager = (JavaIdentifier)tokens.get(i);
		JavaCallKeywordStart call = (JavaCallKeywordStart)tokens.get(i+1);
		JavaLiteral dirty = (JavaLiteral)tokens.get(i+2);
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i+1);
		if (tokens.get(callEnd - 1) instanceof JavaBlockEnd) {
			tokens.remove(callEnd+1);
			tokens.remove(callEnd);
			tokens.add(callEnd, new JavaKeyword("finally"));
			tokens.add(callEnd+1, new JavaBlockStart());
			tokens.add(callEnd+2, new JavaIdentifier("AboraBlockSupport"));
			tokens.add(callEnd+3, new JavaCallStart("exitConsistent"));
			tokens.add(callEnd+4, new JavaCallEnd());
			tokens.add(callEnd+5, new JavaStatementTerminator());
			tokens.add(callEnd+6, new JavaBlockEnd());
			
			diskManager.value="AboraBlockSupport";
			call.value="enterConsistent";
			tokens.remove(i+3);
			tokens.add(i+3, new JavaCallEnd());
			tokens.add(i+4, new JavaStatementTerminator());
			tokens.add(i+5, new JavaKeyword("try"));
			
			javaMethod.javaClass.includeImportForType("AboraBlockSupport");
		}
	}
}
