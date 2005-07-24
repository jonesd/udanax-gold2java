/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.IntegerLiteral;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * TODO this class only exists because of limitations of choosing transformOnly with a String
 */
public class TransformPrimIntegerSpecConstructorUnsigned extends AbstractMethodBodyTransformation {

	public TransformPrimIntegerSpecConstructorUnsigned() {
		super();
	}
	public TransformPrimIntegerSpecConstructorUnsigned(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaKeyword.class, "else"),
				factory.token(JavaBlockStart.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getName().equals("PrimIntegerSpec")) {
			return i;
		}
		int j = i + 2;
		//myMin = Int32Zero;
		tokens.add(j++, new JavaIdentifier("myMin"));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new IntegerLiteral(0));
		tokens.add(j++, new JavaStatementTerminator());
		//myMax = ~(((~Int32Zero) << (myBitCount - 1)) << 1);
		tokens.add(j++, new JavaIdentifier("myMax"));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaKeyword("~"));
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaKeyword("~"));
		tokens.add(j++, new IntegerLiteral(0));
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.add(j++, new JavaKeyword("<<"));
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaIdentifier("myBitCount"));
		tokens.add(j++, new JavaKeyword("-"));
		tokens.add(j++, new IntegerLiteral(1));
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.add(j++, new JavaKeyword("<<"));
		tokens.add(j++, new IntegerLiteral(1));
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.add(j++, new JavaStatementTerminator());
		
		return i;
	}
}
