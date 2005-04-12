/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.JavaType;

/**
 * Sometimes the block level declaration of our variables differs enough from Smalltalk,
 * so that can end up with conflicting variable declarations of the same variable name.
 */
public class EnsureUniqueLocalVarNames implements MethodTransformation {

	public void transform(JavaMethod javaMethod) {
		Map nameOccurrence = new HashMap();
		//TODO populate nameOccurrences with all instance variables?
		//TODO handle declarations within blocks only lasting to the end of the block
		
		List tokens = javaMethod.methodBody.tokens;
		for (int i = 0; i < tokens.size(); ++i) {
			JavaToken token = (JavaToken)tokens.get(i);
			if (token instanceof JavaIdentifier && couldBeLocalVariableName(token.value)) {
				Integer occurrences = (Integer)nameOccurrence.get(token.value);
				if (isVariableDeclaration(tokens, i)) {
					if (occurrences == null) {
						occurrences = new Integer(0);
					} else {
						occurrences = new Integer(occurrences.intValue() + 1);
					}
					nameOccurrence.put(token.value, occurrences);
				}
				if (occurrences != null && occurrences.intValue() > 0) {
					token.value = token.value+occurrences;
				}
			}
		}
	}

	private boolean isVariableDeclaration(List tokens, int i) {
		return i > 0 && tokens.get(i-1) instanceof JavaType;
	}

	private boolean couldBeLocalVariableName(String name) {
		return Character.isLowerCase(name.charAt(0));
	}
	
	

}