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
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * TODO this class only exists because of limitations of choosing transformOnly with a String
 */
public class TransformGenericCrossRegionIntersects extends AbstractMethodBodyTransformation {

	public TransformGenericCrossRegionIntersects() {
		super();
	}
	public TransformGenericCrossRegionIntersects(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "others"),
				factory.token(JavaCallStart.class, "destroy"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getQualifiedName().equals("GenericCrossRegion.intersects")) {
			return i;
		}
		tokens.add(i+4, new JavaBlockEnd());
		
		int j = i;
		tokens.add(j++, new JavaKeyword("if"));
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaIdentifier("others"));
		tokens.add(j++, new JavaKeyword("!="));
		tokens.add(j++, new JavaKeyword("null"));
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.add(j++, new JavaBlockStart());
		
		return j+1;
	}
}
