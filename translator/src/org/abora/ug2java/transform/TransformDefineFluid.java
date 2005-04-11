/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;
import org.abora.ug2java.util.NameSupport;



public class TransformDefineFluid extends AbstractMethodBodyTransformation {
	

	public TransformDefineFluid() {
		super();
	}
	public TransformDefineFluid(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "defineFluid"),
				factory.token(JavaIdentifier.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		JavaIdentifier name = (JavaIdentifier)tokens.get(i+2);
		int blockStart = javaMethod.methodBody.findNextTokenOfType(i+2, JavaBlockStart.class);
		int blockEnd = javaMethod.methodBody.findEndOfBlock(blockStart);
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i+1);
		if (blockEnd > callEnd) {
			System.out.println("--Failed JavaBlock match for defineFluid");
			return i;
		}
		
		tokens.remove(blockEnd);
		if (tokens.get(blockEnd-1) instanceof JavaStatementTerminator) {
			tokens.remove(blockEnd - 1);
		}
		tokens.remove(blockStart);
		tokens.add(i+2, new JavaIdentifier(identifier.value));
		//TODO why this embedded class
		tokens.add(i+3, new JavaIdentifier("class"));
		tokens.add(i+4, new JavaCallArgumentSeparator());
		identifier.value = "AboraSupport";
		
		name.value = "\"" + NameSupport.idToString(name.value) + "\"";
			
		return i;
	}
}
