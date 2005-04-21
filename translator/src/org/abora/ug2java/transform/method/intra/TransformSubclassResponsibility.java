/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSubclassResponsibility extends AbstractMethodBodyTransformation {

	public TransformSubclassResponsibility() {
		super();
	}
	public TransformSubclassResponsibility(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class, "subclassResponsibility");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		tokens.add(i, new JavaKeyword("throw"));
		tokens.add(i + 1, new JavaKeyword("new"));
		call.value = "SubclassResponsibilityException";
		javaMethod.javaClass.includeImportForType("SubclassResponsibilityException");
		
		//handle ^self subclassResponsibility
		if (i > 0 && tokens.get(i-1) instanceof JavaKeyword) {
			JavaKeyword prev = (JavaKeyword)tokens.get(i-1);
			if (prev.value.equals("return")) {
				tokens.remove(i-1);
			}
		}
		
		return i;
	}
}
