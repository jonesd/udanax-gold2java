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



public class TransformReanimate extends AbstractMethodBodyTransformation {

	public TransformReanimate() {
		super();
	}
	public TransformReanimate(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallKeywordStart.class, "reanimate"), 
				factory.token(JavaBlockStart.class),
				factory.token(JavaType.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaStatementTerminator.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaType tempType = (JavaType)tokens.get(i+2);
		JavaIdentifier tempName = (JavaIdentifier)tokens.get(i+3);
		
		int expressionStart = javaMethod.methodBody.findStartOfExpression(i-1);
		int blockEnd = javaMethod.methodBody.findEndOfBlock(i+1);
		//TODO should check to ensure only one statement within this block
		
		javaMethod.methodBody.shouldMatch(blockEnd+1, JavaCallEnd.class);
		javaMethod.methodBody.shouldMatch(blockEnd+2, JavaStatementTerminator.class);
		
		tokens.remove(blockEnd+2);
		tokens.remove(blockEnd+1);
		
		tokens.add(blockEnd+1, new JavaKeyword("finally"));
		tokens.add(blockEnd+2, new JavaBlockStart());
		tokens.add(blockEnd+3, new JavaIdentifier("AboraBlockSupport"));
		tokens.add(blockEnd+4, new JavaCallStart("exitRecorderFossilReanimate"));
		tokens.add(blockEnd+5, new JavaCallEnd());
		tokens.add(blockEnd+6, new JavaStatementTerminator());
		tokens.add(blockEnd+7, new JavaBlockEnd());

		javaMethod.methodBody.removeShouldMatch(i+4, JavaStatementTerminator.class);
		javaMethod.methodBody.removeShouldMatch(i+3, JavaIdentifier.class);
		javaMethod.methodBody.removeShouldMatch(i+2, JavaType.class);
		javaMethod.methodBody.removeShouldMatch(i, JavaCallStart.class, "reanimate");
		
		tokens.add(i, new JavaCallEnd());
		tokens.add(i+1, new JavaStatementTerminator());
		tokens.add(i+2, new JavaKeyword("try"));
		
		tokens.add(expressionStart, new JavaType(tempType.value));
		tokens.add(expressionStart+1, new JavaIdentifier(tempName.value));
		tokens.add(expressionStart+2, new JavaAssignment());
		tokens.add(expressionStart+3, new JavaIdentifier("AboraBlockSupport"));
		tokens.add(expressionStart+4, new JavaCallKeywordStart("enterRecorderFossilReanimate"));
		
		return i;
	}
}
