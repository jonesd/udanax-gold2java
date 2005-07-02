/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.MethodTransformation;

public class RemoveTrailingReturn implements MethodTransformation {

	private static final Set METHODS;
	static {
		Set set = new HashSet();
		set.add("Recipe.isEqual");
		set.add("SpecialistRcvr.basicReceive");

		METHODS = Collections.unmodifiableSet(set);
	}

	
	public void transform(JavaMethod javaMethod) {
		String shortName = javaMethod.name;
		String fullName = javaMethod.javaClass.className+"."+shortName;
		String parameterName = javaMethod.getQualifiedSignature();
		if (!javaMethod.parameters.isEmpty() && (METHODS.contains(shortName) || METHODS.contains(fullName) || METHODS.contains(parameterName))) {
			int size = javaMethod.methodBody.tokens.size();
			javaMethod.methodBody.removeShouldMatch(size-1, JavaStatementTerminator.class);
			javaMethod.methodBody.removeShouldMatch(size-2, JavaIdentifier.class);
			javaMethod.methodBody.removeShouldMatch(size-3, JavaKeyword.class, "return");
		}
	}

}