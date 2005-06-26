/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformDo extends AbstractMethodBodyTransformation {

	static class DoDetails {
		String sizeCallName = "size";
		String elementTypeName = "Object";
		String elementAccessorName = "get";
		public DoDetails() {
			super();
		}
		public DoDetails(String elementTypeName) {
			this.elementTypeName = elementTypeName;
		}
		public DoDetails(String sizeCallName, String elementTypeName, String elementAccessorName) {
			this.sizeCallName = sizeCallName;
			this.elementTypeName = elementTypeName;
			this.elementAccessorName = elementAccessorName;
		}
	};
	
	private final static Map LOOKUP;
	static {
		Map map = new HashMap();
		map.put("Emulsion.fluidsSpace", new DoDetails("FluidVar"));
		map.put("Emulsion.destructAll", new DoDetails("FluidVar"));
		LOOKUP = Collections.unmodifiableMap(map);
	}

	public TransformDo() {
		super();
	}
	public TransformDo(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaToken.class),
				factory.token(JavaCallKeywordStart.class, "dox"),
				factory.token(JavaBlockStart.class)
//				factory.token(JavaType.class, "Character"),
//				factory.token(JavaIdentifier.class),
//				factory.token(JavaStatementTerminator.class)
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!(tokens.get(i) instanceof JavaIdentifier)) {
			return i;
		}
		JavaIdentifier sourceVar = (JavaIdentifier)tokens.get(i);
		
		String sourceVarName = sourceVar.value;
		
		DoDetails details = (DoDetails)LOOKUP.get(javaMethod.getQualifiedName());
		if (details == null) {
			String sourceVarTypeName = javaMethod.findTypeOfVariable(sourceVarName);
			if ("String".equals(sourceVarTypeName)) {
				details = new DoDetails("length", "char", "charAt");
			} else {
				details = new DoDetails();
			}
		}
		
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
		tokens.add(j++, new IntegerLiteral(0));
		tokens.add(j++, new JavaLoopTerminator());
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaKeyword("<"));
		tokens.add(j++, new JavaIdentifier(sourceVarName));
		tokens.add(j++, new JavaCallStart(details.sizeCallName));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaLoopTerminator());
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaKeyword("++"));
		tokens.add(j++, new JavaParenthesisEnd());
		tokens.remove(j);
		tokens.remove(j);
		j++;
		tokens.remove(j);
		tokens.add(j++, new JavaType(details.elementTypeName));
		j++;
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaCast(details.elementTypeName));
		tokens.add(j++, new JavaIdentifier(sourceVarName));
		tokens.add(j++, new JavaCallKeywordStart(details.elementAccessorName));
		tokens.add(j++, new JavaIdentifier("doIndex"));
		tokens.add(j++, new JavaCallEnd());
		
		return i;
	}
}
