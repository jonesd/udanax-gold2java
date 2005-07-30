/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;
import org.abora.ug2java.util.ClassHelper;



/**
 * Transformation due to apparently a missing shareLess on a SharedPtrArry. I'm assuming
 * I am missing some finalize or unreference code around SharedPtrArray - which may
 * suggest a lot of other unfound problems like this....
 */
public class TransformBucketArrayStepperVerifyEntry extends AbstractMethodBodyTransformation {

	public TransformBucketArrayStepperVerifyEntry() {
		super();
	}
	public TransformBucketArrayStepperVerifyEntry(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaBlockEnd.class),
				factory.token(JavaIdentifier.class, "myEntries"),
				factory.token(JavaAssignment.class),
				factory.token(JavaIdentifier.class, "null"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getQualifiedName().equals("BucketArrayStepper.verifyEntry")) {
			return i;
		}
		int j = i+1;
		tokens.add(j++, new JavaComment("Tranformed - include a shareLess here ["+ClassHelper.getShortName(getClass())+"]"));
		tokens.add(j++, new JavaIdentifier("myEntries"));
		tokens.add(j++, new JavaCallStart("shareLess"));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaStatementTerminator());

		return j;
	}
}
