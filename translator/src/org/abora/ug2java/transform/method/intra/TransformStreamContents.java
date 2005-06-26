/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;
import org.abora.ug2java.util.ClassHelper;



public class TransformStreamContents extends AbstractMethodBodyTransformation {


	public TransformStreamContents() {
		super();
	}
	public TransformStreamContents(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
//				factory.token(JavaKeyword.class, "return"),
				factory.token(JavaIdentifier.class, "String"),
				factory.token(JavaCallKeywordStart.class, "streamContents"),
				factory.token(JavaBlockStart.class),
				factory.token(JavaType.class, "Object"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int statementStart = javaMethod.methodBody.findStartOfStatement(i-1);
		int blockEnd = javaMethod.methodBody.findEndOfBlockQuietFail(i+2);
		if (blockEnd == -1) {
			System.out.println("--Failed to find end of block for:"+ClassHelper.getShortName(this.getClass())+" method:"+javaMethod.getQualifiedSignature());
			return i;
		}
		tokens.remove(blockEnd+1);
		tokens.remove(blockEnd);
		tokens.remove(i+3);
		tokens.remove(i+2);
		tokens.remove(i+1);
		tokens.remove(i+0);
		tokens.add(i, new JavaType("StringWriter"));
		tokens.add(i+1, new JavaIdentifier("stringWriter"));
		tokens.add(i+2, new JavaAssignment());
		tokens.add(i+3, new JavaKeyword("new"));
		tokens.add(i+4, new JavaCallStart("StringWriter"));
		tokens.add(i+5, new JavaCallEnd());
		tokens.add(i+6, new JavaStatementTerminator());
		tokens.add(i+7, new JavaType("PrintWriter"));
		tokens.add(i+9, new JavaAssignment());
		tokens.add(i+10, new JavaKeyword("new"));
		tokens.add(i+11, new JavaCallKeywordStart("PrintWriter"));
		tokens.add(i+12, new JavaIdentifier("stringWriter"));
		tokens.add(i+13, new JavaCallEnd());
		int newEnd = blockEnd+13-4;
		tokens.add(newEnd+0, new JavaIdentifier("stringWriter"));
		tokens.add(newEnd+1, new JavaCallStart("toString"));
		tokens.add(newEnd+2, new JavaCallEnd());
		
		javaMethod.methodBody.copy(statementStart, i, newEnd);
		javaMethod.methodBody.remove(statementStart, i);
		
		return i;
	}
}
