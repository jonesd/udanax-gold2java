/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformAtCalls extends AbstractMethodBodyTransformation {

	public TransformAtCalls() {
		super();
	}
	public TransformAtCalls(TokenMatcherFactory factory) {
		super(factory);
	}

	
	public void transform(JavaMethod javaMethod) {
		javaMethod.name = transformName(javaMethod.name);

		super.transform(javaMethod);
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		call.value = transformName(call.value); 
		return i;
	}
	
	private String transformName(String name) {
		if (name.startsWith("at") && name.length() >= 3 && Character.isUpperCase(name.charAt(2))) {
			String reducedName = Character.toString(Character.toLowerCase(name.charAt(2))) + name.substring(3);
			return reducedName;
		}
		return name;
	}
}
