/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class RecordLocalVars extends AbstractMethodBodyTransformation {

	public RecordLocalVars() {
		super();
	}
	public RecordLocalVars(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaType.class),
				factory.token(JavaIdentifier.class),
				factory.any(
						factory.token(JavaAssignment.class),
						factory.token(JavaStatementTerminator.class)
						)
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		String tempType = ((JavaType)tokens.get(i)).value;
		String tempName = ((JavaIdentifier)tokens.get(i+1)).value;
		javaMethod.addLocalVariable(new JavaField(tempType, tempName));
		return i;
	}
}
