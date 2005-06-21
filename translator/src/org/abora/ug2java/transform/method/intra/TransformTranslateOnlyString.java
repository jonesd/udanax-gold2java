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
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.StringLiteral;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformTranslateOnlyString extends AbstractMethodBodyTransformation {

	public TransformTranslateOnlyString() {
		super();
	}
	public TransformTranslateOnlyString(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(StringLiteral.class), 
				factory.token(JavaCallStart.class, "translateOnly"), 
				factory.token(JavaCallEnd.class));
	}
	
	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		StringLiteral javaLiteral = (StringLiteral)tokens.get(i);
		//TODO general insanity here. Why didn't we just remember the original value?
		String cleanSource = stipStringWrapping(javaLiteral.value);
		
		tokens.remove(i+2);
		tokens.remove(i+1);
		tokens.remove(i);

		tokens.add(i, new JavaBlockStart());
		tokens.add(i+1, new JavaComment(cleanSource));
		tokens.add(i+2, new JavaBlockEnd());
		tokens.add(i+3, new JavaIdentifier("translateOnly"));
		
		return i;
	}
	
	private String stipStringWrapping(String value) {
		String cleaner = value.replaceAll("\\\\n\"\\+\n\"", "\n");
//		cleaner = cleaner.replaceAll("\n\"", "\n");
		return cleaner.substring(1, cleaner.length()-1);
	}
}
