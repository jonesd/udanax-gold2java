/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
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

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier token = (JavaIdentifier)tokens.get(i);
		if (i < tokens.size() - 1 && (tokens.get(i + 1) instanceof JavaCallStart)) {
			return;
		}
		if (i > 0) {
			JavaToken pre = (JavaToken) tokens.get(i - 1);
			if ((pre instanceof JavaKeyword) && pre.value.equals("instanceof")) {
				return;
			}
		}
		if (javaMethod.javaClass.packageLookup.get(token.value) != null) {
			tokens.add(i + 1, new JavaCallStart("getCategory"));
			tokens.add(i + 2, new JavaCallEnd());
		}
	}

//	protected void transformClassReference(MethodBody body) {
//	List tokens = body.tokens;
//	for (int i = 0; i < tokens.size(); i++) {
//		JavaToken token = (JavaToken) tokens.get(i);
//		if (token instanceof JavaIdentifier) {
//			if (i < tokens.size() - 1 && (tokens.get(i + 1) instanceof JavaCallStart)) {
//				continue;
//			}
//			if (i > 0) {
//				JavaToken pre = (JavaToken) tokens.get(i - 1);
//				if ((pre instanceof JavaKeyword) && pre.value.equals("instanceof")) {
//					continue;
//				}
//			}
//			if (javaClass.packageLookup.get(token.value) != null) {
//				tokens.add(i + 1, new JavaCallStart("getCategory"));
//				tokens.add(i + 2, new JavaCallEnd());
//			}
//		}
//	}
//}

}
