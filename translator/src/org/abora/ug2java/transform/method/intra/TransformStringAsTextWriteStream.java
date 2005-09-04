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
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.javatoken.StringLiteral;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformStringAsTextWriteStream extends AbstractMethodBodyTransformation {


	public TransformStringAsTextWriteStream() {
		super();
	}
	public TransformStringAsTextWriteStream(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaAssignment.class),
				factory.token(StringLiteral.class, "\"\""),
				factory.token(JavaCallStart.class, "asText"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaCallStart.class, "writeStream"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		
		for (int j = i + 6; j >= i +2; j--) {
			tokens.remove(j);
		}
		
		tokens.add(i, new JavaType("StringWriter"));
		tokens.add(i+1, new JavaIdentifier("stringWriter"));
		tokens.add(i+2, new JavaAssignment());
		tokens.add(i+3, new JavaKeyword("new"));
		tokens.add(i+4, new JavaCallStart("StringWriter"));
		tokens.add(i+5, new JavaCallEnd());
		tokens.add(i+6, new JavaStatementTerminator());
		tokens.add(i+9, new JavaKeyword("new"));
		tokens.add(i+10, new JavaCallKeywordStart("PrintWriter"));
		tokens.add(i+11, new JavaIdentifier("stringWriter"));
		tokens.add(i+12, new JavaCallEnd());
		
		return i;
	}
}
