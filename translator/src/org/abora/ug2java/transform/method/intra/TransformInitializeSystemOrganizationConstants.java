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
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.StringLiteral;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;
import org.abora.ug2java.util.NameSupport;



public class TransformInitializeSystemOrganizationConstants extends AbstractMethodBodyTransformation {
	private static final Set IGNORE_CONSTANTS;
	static {
		Set set = new HashSet();
		set.add("PUBLIC");
		set.add("PRIVATE");
		set.add("PROTECTED");
		set.add("TEST");
		set.add("FILE");
		set.add("DIR");
		
		set.add("ID");
		set.add("SHTO");
		IGNORE_CONSTANTS = Collections.unmodifiableSet(set);
	}
	
	public TransformInitializeSystemOrganizationConstants() {
		super();
	}
	public TransformInitializeSystemOrganizationConstants(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaIdentifier.class);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getName().equals("initializeSystemOrganization")) {
			return i;
		}
		
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		if (identifier.isConstant() && !IGNORE_CONSTANTS.contains(identifier.value)) {
			tokens.remove(i);
			tokens.add(i, new StringLiteral(NameSupport.idToLowerString(identifier.value)));
		}
		
		return i;
	}
}
