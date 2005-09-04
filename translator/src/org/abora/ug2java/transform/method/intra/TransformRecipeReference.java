/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.ClassParser;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformRecipeReference extends AbstractMethodBodyTransformation {

	//TODO review this deference stuff...
	private static final Set DEREFERENCE_METHODS;
	static {
		Set set = new HashSet();
		set.add("BootPlan.initTimeNonInherited");
		set.add("BackendBootMaker.initTimeNonInherited");
		set.add("CalcCreator.initTimeNonInherited");
		
		//TODO test case
		set.add("Test.recipeDereference");
		DEREFERENCE_METHODS = Collections.unmodifiableSet(set);
	}

	public TransformRecipeReference() {
		super();
	}
	public TransformRecipeReference(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaIdentifier.class, "BootCuisine|CalcCuisine|DiskCuisine|FebeCuisine|XppCuisine");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		boolean shouldDeference = DEREFERENCE_METHODS.contains(javaMethod.getName()) || DEREFERENCE_METHODS.contains(javaMethod.getQualifiedName());
		
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		tokens.remove(i);
		
		boolean isAssignment = tokens.get(i) instanceof JavaAssignment; 
				
		int j = i;
		tokens.add(j++, new JavaIdentifier("Smalltalk"));
		tokens.add(j++, new JavaCallKeywordStart(isAssignment ? "atPut" : "associationAt"));
		tokens.add(j++, new JavaIdentifier(ClassParser.transformSmalltalkSymbolToJava(identifier.value)));
		
		if (isAssignment) {
			tokens.remove(j);
			tokens.add(j++, new JavaCallArgumentSeparator());
			j = javaMethod.methodBody.findEndOfExpression(j);
			j++;
		} 
		tokens.add(j++, new JavaCallEnd());
		if (shouldDeference) {
			tokens.add(j++, new JavaCallStart("refValue"));
			tokens.add(j++, new JavaCallEnd());
		}
		
		return i-1;
	}
}
