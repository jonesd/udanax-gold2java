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
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformShouldImplement extends AbstractMethodBodyTransformation {

	public TransformShouldImplement() {
		super();
	}
	public TransformShouldImplement(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "shouldImplement"),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier type = (JavaIdentifier)tokens.get(i);
		tokens.remove(i + 1);
		tokens.remove(i);
		
		tokens.add(i, new JavaKeyword("throw"));
		tokens.add(i + 1, new JavaKeyword("new"));
		tokens.add(i + 2, new JavaCallKeywordStart("ShouldImplementException"));
		tokens.add(i + 3, new JavaLiteral("\""+type.value+"\""));
		
		javaMethod.javaClass.includeImportForType("ShouldImplementException");
		
		if (i > 0) {
			JavaToken preToken = (JavaToken)tokens.get(i - 1);
			if ((preToken instanceof JavaKeyword) && preToken.value.equals("return")) {
				tokens.remove(i - 1);
			}
		}
		
		return i;
	}
}
