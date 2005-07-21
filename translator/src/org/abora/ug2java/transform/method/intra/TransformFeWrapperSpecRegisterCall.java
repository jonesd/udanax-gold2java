/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * TODO this class only exists because of limitations inferring type for downcasting
 * arguments within a call
 */
public class TransformFeWrapperSpecRegisterCall extends AbstractMethodBodyTransformation {

	public TransformFeWrapperSpecRegisterCall() {
		super();
	}
	public TransformFeWrapperSpecRegisterCall(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "FeWrapperSpec"),
				factory.token(JavaCallKeywordStart.class, "register(Abstract|Direct|Indirect)"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallKeywordStart call = (JavaCallKeywordStart)tokens.get(i+1);
		String[] casts;
		if (call.value.equals("registerAbstract")) {
			casts = new String[] {"", "", "FeWrapperSpecHolder"};
		} else if (call.value.equals("registerDirect")) {
			casts = new String[] {"", "", "FeDirectWrapperMaker", "FeDirectWrapperChecker", "FeWrapperSpecHolder"};
		} else {
			casts = new String[] {"", "", "", "FeIndirectWrapperMaker", "FeIndirectWrapperChecker", "FeWrapperSpecHolder"};
		}
		
		
		List argStarts = javaMethod.methodBody.extractCallArgStarts(i+1);
		if (argStarts.size() != casts.length) {
			System.out.println("--Warning: Mismatch of FeWrapperSpec register casts: "+casts+" but found: "+argStarts);
			return i;
		}
		
		for (int j = casts.length-1; j >= 0; j--) {
			String cast = casts[j];
			if (!cast.equals("")) {
				int insert = ((Integer)argStarts.get(j)).intValue();
				tokens.add(insert, new JavaCast(cast));
			}
		}
		
		return i;
	}
}
