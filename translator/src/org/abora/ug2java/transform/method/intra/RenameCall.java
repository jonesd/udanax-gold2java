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
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class RenameCall extends AbstractMethodBodyTransformation {

	static final Map RENAME_CALLS;
	static {
		Map map = new HashMap();
		map.put("show", "print");
		map.put("SnarfPacker.purgeClean.cr", "println");
		map.put("TestPacker.purgeClean.cr", "println");
		//map.put("cr", "println");
		map.put("nextPut", "print");
		map.put("ActualHashSet.linkTimeNonInherited.Array", "IntArray");
		map.put("GrandHashTable.subTable.makeCoordinateSpace", "make");
		map.put("IEEE8Pos.sendSelfTo.sendInt32", "sendIEEEDoubleVar");
		map.put("FeWorkSet.unionWith.notSharedWIth", "notSharedWith");
		map.put("isdigit", "isDigit");
		map.put("isspace", "isWhitespace");
		map.put("isalnum", "isLetterOrDigit");
		
		//TODO only for tests
		map.put("Test.testRename.Array", "IntArray");
		map.put("Test.testCascade.cr", "println");
		RENAME_CALLS = Collections.unmodifiableMap(map);
	}
	
public RenameCall() {
		super();
	}
	public RenameCall(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class, regularExpressionOrTrailing(RENAME_CALLS.keySet()));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		String shortCall = call.value;
		String methodCall = javaMethod.name+"."+shortCall;
		String fullCall = javaMethod.javaClass.className+"."+methodCall;
		String lookup = null;
		if (RENAME_CALLS.containsKey(fullCall)) {
			lookup = fullCall;
		} else if (RENAME_CALLS.containsKey(methodCall)) {
			lookup = methodCall;
		} else if (RENAME_CALLS.containsKey(shortCall)) {
			lookup = shortCall;
		}
		if (lookup != null) {
			String newCallName = (String)RENAME_CALLS.get(lookup);
			call.value = newCallName;
		}
		return i;
	}
}
