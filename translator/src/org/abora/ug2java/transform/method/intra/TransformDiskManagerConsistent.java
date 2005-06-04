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
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;
import org.abora.ug2java.util.NameSupport;



public class TransformDiskManagerConsistent extends AbstractMethodBodyTransformation {

	public TransformDiskManagerConsistent() {
		super();
	}
	public TransformDiskManagerConsistent(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaToken.class), 
				factory.any(
						factory.token(JavaCallKeywordStart.class, "consistent"),
						factory.token(JavaCallKeywordStart.class, "insistent"))
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaToken diskManager = (JavaToken)tokens.get(i);
		String diskManagerName = null;
		if (!(diskManager instanceof JavaIdentifier) && !"DiskManager".equals(diskManager.value)) {
			diskManagerName = "diskManager1";
		}
		JavaCallKeywordStart call = (JavaCallKeywordStart)tokens.get(i+1);
		int blockStart;
		// Optional dirty first parameter
		if (tokens.get(i + 2) instanceof JavaBlockStart) {
			blockStart = i + 2;
		} else if (tokens.get(i+2) instanceof JavaComment && tokens.get(i+3) instanceof JavaBlockStart) {
			// TODO should use more generic parsing that will skip comments...
			blockStart = i + 3;	
		} else {
			blockStart = javaMethod.methodBody.findNextTokenOfTypeQuietFail(i+2, JavaBlockStart.class);
			if (blockStart == -1) {
				return i;
			}
			if (!(tokens.get(blockStart-1) instanceof JavaCallArgumentSeparator)) {
				System.out.println("--Failed to match block context in DiskManager consistent call");
				return i;
			}
			tokens.remove(blockStart - 1);
			blockStart -= 1;
		}
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i+1);
		String baseName = NameSupport.capatilize(call.value);
		if (tokens.get(callEnd - 1) instanceof JavaBlockEnd) {
			tokens.remove(callEnd+1);
			tokens.remove(callEnd);
			tokens.add(callEnd, new JavaKeyword("finally"));
			tokens.add(callEnd+1, new JavaBlockStart());
			tokens.add(callEnd+2, new JavaIdentifier("AboraBlockSupport"));
			tokens.add(callEnd+3, new JavaCallKeywordStart("exit"+baseName));
			tokens.add(callEnd+4, new JavaCallEnd());
			tokens.add(callEnd+5, new JavaStatementTerminator());
			tokens.add(callEnd+6, new JavaBlockEnd());
			if (diskManagerName != null) {
				tokens.add(callEnd+4, new JavaIdentifier(diskManagerName));
			}
			
			tokens.add(blockStart, new JavaCallEnd());
			tokens.add(blockStart+1, new JavaStatementTerminator());
			tokens.add(blockStart+2, new JavaKeyword("try"));

			tokens.add(i+1, new JavaIdentifier("AboraBlockSupport"));
			call.value="enter"+baseName;

			if (diskManagerName != null) {
				tokens.add(blockStart+1, new JavaCallArgumentSeparator());
				tokens.add(blockStart+2, new JavaIdentifier(diskManagerName));
				int expressionStart = javaMethod.methodBody.findStartOfExpression(i);
				tokens.add(i+1, new JavaStatementTerminator());
				tokens.add(expressionStart, new JavaType("DiskManager"));
				tokens.add(expressionStart+1, new JavaIdentifier(diskManagerName));
				tokens.add(expressionStart+2, new JavaAssignment());
			} else {
				tokens.remove(i);
			}
			
		}
		return i;
	}
}
