/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformClassReference extends AbstractMethodBodyTransformation {

	public TransformClassReference() {
		super();
	}
	public TransformClassReference(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaIdentifier.class);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier token = (JavaIdentifier)tokens.get(i);
		if (i < tokens.size() - 1 && (tokens.get(i + 1) instanceof JavaCallStart)) {
			return i;
		}
		if (i > 0) {
			JavaToken pre = (JavaToken) tokens.get(i - 1);
			if ((pre instanceof JavaKeyword) && pre.value.equals("instanceof")) {
				return i;
			}
		}
		if (javaMethod.javaClass.packageLookup.get(token.value) != null && (i >= tokens.size() || !((JavaToken)tokens.get(i+1)).value.equals("class"))) {
			tokens.add(i, new JavaIdentifier("AboraSupport"));
			tokens.add(i+1, new JavaCallKeywordStart("findCategory"));
			// classname
			tokens.add(i + 3, new JavaIdentifier("class"));
			tokens.add(i+4, new JavaCallEnd());
			javaMethod.javaClass.includeImportForType("AboraSupport");
		}
		return i;
	}
}
