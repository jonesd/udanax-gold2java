/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformWriteStreamOnString extends AbstractMethodBodyTransformation {


	public TransformWriteStreamOnString() {
		super();
	}
	public TransformWriteStreamOnString(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaAssignment.class),
				factory.token(JavaIdentifier.class, "WriteStream"),
				factory.token(JavaCallKeywordStart.class, "on"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i+3);
		javaMethod.methodBody.remove(i+2, callEnd+1);

		String stringWriterVar = generateStringWriterName(identifier.value);
		
		tokens.add(i, new JavaType("StringWriter"));
		tokens.add(i+1, new JavaIdentifier(stringWriterVar));
		tokens.add(i+2, new JavaAssignment());
		tokens.add(i+3, new JavaKeyword("new"));
		tokens.add(i+4, new JavaCallStart("StringWriter"));
		tokens.add(i+5, new JavaCallEnd());
		tokens.add(i+6, new JavaStatementTerminator());
		tokens.add(i+9, new JavaKeyword("new"));
		tokens.add(i+10, new JavaCallKeywordStart("PrintWriter"));
		tokens.add(i+11, new JavaIdentifier(stringWriterVar));
		tokens.add(i+12, new JavaCallEnd());
		
		return i;
	}
	
	public static String generateStringWriterName(String printWriterVarName) {
		return printWriterVarName+"String";
	}
}
