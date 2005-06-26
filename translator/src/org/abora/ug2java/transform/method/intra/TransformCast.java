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

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCast extends AbstractMethodBodyTransformation {

	private static final Map OVERRIDE_CAST;
	static {
		Map map = new HashMap();
		map.put("XnBufferedReadStream.getBytes.String", "");
		map.put("XnReadStream.getBytes.String", "");
		OVERRIDE_CAST = Collections.unmodifiableMap(map);
	}
	
	public TransformCast() {
		super();
	}
	public TransformCast(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.any(
						factory.token(JavaCallKeywordStart.class, "cast"),
						factory.token(JavaCallKeywordStart.class, "basicCast"),
						factory.token(JavaCallKeywordStart.class, "quickCast")), 
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier type = (JavaIdentifier)tokens.get(i + 1);
		String castTypeName = findCastType(javaMethod, type);
		int start = javaMethod.methodBody.findStartOfExpression(i - 1);
		tokens.remove(i + 2);
		tokens.remove(i + 1);
		tokens.remove(i);
		if (i > 0 && tokens.get(i-1) instanceof JavaIdentifier) {
			JavaIdentifier var = (JavaIdentifier)tokens.get(i-1);
			String varTypeName = javaMethod.findTypeOfVariable(var.value);
			if (varTypeName != null && javaMethod.methodBody.findStartOfExpressionMinimal(i-1) == i-1) {
				if (varTypeName.equals(castTypeName)) {
//				if (!javaMethod.getJavaCodebase().shouldDowncast(varType, castType)) {
					castTypeName = "";
				} else {
					JavaClass castType = javaMethod.getJavaCodebase().getJavaClass(castTypeName);
					JavaClass varType = javaMethod.getJavaCodebase().getJavaClass(varTypeName);
					if (castType != null && varType != null && varType.isSubclassOf(castType)) {
						castTypeName = "";
					}
				}
			}
		}
		if (!castTypeName.equals("")) {
			tokens.add(start, new JavaCast(castTypeName));
		}
		return i;
	}
	
	private String findCastType(JavaMethod javaMethod, JavaIdentifier type) {
		String name = javaMethod.javaClass.className+"."+javaMethod.name+"."+type.value;
		String castType = (String)OVERRIDE_CAST.get(name);
		if (castType == null) {
			castType = type.value;
		}
		return castType;
	}
}
