/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformTimeMillisecondsToRun extends AbstractMethodBodyTransformation {

	public TransformTimeMillisecondsToRun() {
		super();
	}
	public TransformTimeMillisecondsToRun(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaAssignment.class),
				factory.token(JavaIdentifier.class, "Time"),
				factory.token(JavaCallKeywordStart.class, "millisecondsToRun"), 
				factory.token(JavaBlockStart.class));
	}
	
	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier var = (JavaIdentifier)tokens.get(i);
		
		int start = javaMethod.methodBody.findStartOfExpression(i);
		int postCallEnd = javaMethod.methodBody.findClosingCallEnd(i+3);
		if (postCallEnd + 1 < tokens.size() && (tokens.get(postCallEnd + 1) instanceof JavaStatementTerminator)) {
			tokens.remove(postCallEnd + 1);
		}
		tokens.remove(postCallEnd);
		tokens.remove(postCallEnd-1);
		
		int j = postCallEnd-1;
		tokens.add(j++, new JavaIdentifier(var.value));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaIdentifier("System"));
		tokens.add(j++, new JavaCallStart("currentTimeMillis"));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaKeyword("-"));
		tokens.add(j++, new JavaIdentifier(var.value+"Start"));
		tokens.add(j++, new JavaStatementTerminator());
		
		javaMethod.methodBody.remove(i, i+5);

		j = i;
		tokens.add(j++, new JavaType("long"));
		tokens.add(j++, new JavaIdentifier(var.value+"Start"));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaIdentifier("System"));
		tokens.add(j++, new JavaCallStart("currentTimeMillis"));
		tokens.add(j++, new JavaCallEnd());		
		tokens.add(j++, new JavaStatementTerminator());		
		
		return i;
	}
}
