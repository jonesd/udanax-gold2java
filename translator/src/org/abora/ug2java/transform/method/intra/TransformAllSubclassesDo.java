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
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLoopTerminator;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformAllSubclassesDo extends AbstractMethodBodyTransformation {

	public TransformAllSubclassesDo() {
		super();
	}
	public TransformAllSubclassesDo(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallKeywordStart.class, "allSubclassesDo|subclassesDo"),
				factory.token(JavaBlockStart.class)
//				factory.token(JavaType.class, "Character"),
//				factory.token(JavaIdentifier.class),
//				factory.token(JavaStatementTerminator.class)
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier sourceVar = (JavaIdentifier)tokens.get(i);
		JavaCallKeywordStart call = (JavaCallKeywordStart)tokens.get(i+1);
		
		String sourceVarName = call.value.substring(0, call.value.length() - 2);
				
		int blockEnd = javaMethod.methodBody.findEndOfBlock(i+2);
		javaMethod.methodBody.shouldMatch(blockEnd+1, JavaCallEnd.class);
		javaMethod.methodBody.shouldMatch(blockEnd+2, JavaStatementTerminator.class);
		
		String sizeCallName = "size";
		String elementTypeName = "AboraClass";
		String elementAccessorName = "get";
		
		tokens.remove(blockEnd+2);
		tokens.remove(blockEnd+1);
		
		int j = i;
		tokens.add(j++, new JavaType("OrderedCollection"));
		tokens.add(j++, new JavaIdentifier(sourceVarName));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaIdentifier("AboraSupport"));
		tokens.add(j++, new JavaCallKeywordStart(sourceVarName));
		tokens.add(j++, new JavaIdentifier(sourceVar.value));
		if (javaMethod.getJavaCodebase().getJavaClass(sourceVar.value) != null) {
			tokens.add(j++, new JavaIdentifier("class"));
		}
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaStatementTerminator());

		tokens.add(j++, new JavaKeyword("for"));
		tokens.add(j++, new JavaParenthesisStart());
		tokens.add(j++, new JavaType("int"));
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new IntegerLiteral(0));
		tokens.add(j++, new JavaLoopTerminator());
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaKeyword("<"));
		tokens.add(j++, new JavaIdentifier(sourceVarName));
		tokens.add(j++, new JavaCallStart(sizeCallName));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaLoopTerminator());
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaKeyword("++"));
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.remove(j);
		tokens.remove(j);
		j++;
		tokens.remove(j);
		tokens.add(j++, new JavaType(elementTypeName));
		j++;
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaCast(elementTypeName));
		tokens.add(j++, new JavaIdentifier(sourceVarName));
		tokens.add(j++, new JavaCallKeywordStart(elementAccessorName));
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaCallEnd());
		
		return i;
	}
}
