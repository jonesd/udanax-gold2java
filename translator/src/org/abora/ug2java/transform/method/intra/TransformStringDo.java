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
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaLoopTerminator;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformStringDo extends AbstractMethodBodyTransformation {


	public TransformStringDo() {
		super();
	}
	public TransformStringDo(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallKeywordStart.class, "dox"),
				factory.token(JavaBlockStart.class),
				factory.token(JavaType.class, "Character"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier sourceVar = (JavaIdentifier)tokens.get(i);
		
		int blockEnd = javaMethod.methodBody.findEndOfBlock(i+2);
		javaMethod.methodBody.shouldMatch(blockEnd+1, JavaCallEnd.class);
		javaMethod.methodBody.shouldMatch(blockEnd+2, JavaStatementTerminator.class);
		
		tokens.remove(blockEnd+2);
		tokens.remove(blockEnd+1);
		
		int j = i;
		tokens.add(j++, new JavaKeyword("for"));
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaType("int"));
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaLiteral("0"));
		tokens.add(j++, new JavaLoopTerminator());
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaKeyword("<"));
		tokens.add(j++, new JavaLiteral(sourceVar.value));
		tokens.add(j++, new JavaCallStart("length"));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaLoopTerminator());
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaKeyword("++"));
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.remove(j);
		tokens.remove(j);
		j++;
		tokens.remove(j);
		tokens.add(j++, new JavaType("char"));
		j++;
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaIdentifier(sourceVar.value));
		tokens.add(j++, new JavaCallKeywordStart("charAt"));
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaCallEnd());
		
		return i;
	}
}
