/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformBlockReturn extends AbstractMethodBodyTransformation {

	
public TransformBlockReturn() {
		super();
	}
	public TransformBlockReturn(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq( 
				factory.token(JavaKeyword.class, "return"),
				factory.token(JavaBlockStart.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (i != 0) {
			return i;
		}
		int endBlock = javaMethod.methodBody.findEndOfBlock(i+1);
		if (endBlock != tokens.size() - 1) {
			return i;
		}
		tokens.remove(endBlock);
		tokens.remove(i+1);
		tokens.remove(i);
		return i;
	}
}
