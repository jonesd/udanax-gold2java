/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformFluidAccess extends AbstractMethodBodyTransformation {

	private static final Map FLUID_MAPPINGS;
	static {
		Map map = new HashMap();
		map.put("ActiveClubs", "MuSet"); //GUESS
		map.put("CurrentAuthor", "ID");
		map.put("CurrentBertCanopyCache", "CanopyCache");
		map.put("CurrentBertCrum", "BertCrum");
		map.put("CurrentPacker", "DiskManager");
		map.put("CurrentGrandMap", "BeGrandMap");
		map.put("CurrentKeyMaster", "FeKeyMaster");
		map.put("CurrentSensorCanopyCache", "CanopyCache");
		map.put("CurrentServer", "FeServer");
		map.put("CurrentSession", "FeSession");
		map.put("CurrentSessions", "FePromiseSession");
		map.put("CurrentTrace", "TracePosition");
		map.put("GrandConnection", "Connection");
		map.put("InitialOwner", "ID");
		map.put("InitialEditClub", "ID");
		map.put("InitialReadClub", "ID");
		map.put("InitialSponsor", "ID");
		FLUID_MAPPINGS = Collections.unmodifiableMap(map);
	}
	

	public TransformFluidAccess() {
		super();
	}
	public TransformFluidAccess(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.any(
						factory.token(JavaCallStart.class, "fluidGet"),
						factory.token(JavaCallStart.class, "fluidFetch")
						),
				factory.token(JavaCallEnd.class));
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		if (FLUID_MAPPINGS.containsKey(identifier.value)) {
			String type = (String)FLUID_MAPPINGS.get(identifier.value);
			javaMethod.javaClass.includeImportForType(type);
			tokens.add(i + 2, new JavaParenthesisEnd());
			tokens.add(i, new JavaParenthesisStart());
			tokens.add(i + 1, new JavaCast(type));
		}
	}
}
