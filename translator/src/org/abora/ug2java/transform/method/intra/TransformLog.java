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
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformLog extends AbstractMethodBodyTransformation {


	public TransformLog() {
		super();
	}
	public TransformLog(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallKeywordStart.class, "LOG"),
				factory.token(JavaBlockStart.class),
				factory.token(JavaType.class, "Object"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier log = (JavaIdentifier)tokens.get(i);
		int blockEnd = javaMethod.methodBody.findEndOfBlock(i+2);
		if (blockEnd + 2 < tokens.size() && tokens.get(blockEnd+2) instanceof JavaStatementTerminator) {
			tokens.remove(blockEnd+2);
		}
		tokens.remove(blockEnd+1);
		tokens.remove(blockEnd);
		tokens.remove(i+3);
		tokens.remove(i+2);
		tokens.remove(i+1);
		tokens.remove(i+0);
		tokens.add(i+0, new JavaType("PrintWriter"));
		tokens.add(i+2, new JavaAssignment());
		tokens.add(i+3, new JavaIdentifier(log.value));
		
		return i;
	}
}
