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
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class OverrideLocalVarType extends AbstractMethodBodyTransformation {

	private static final Map variables;
	static {
		Map map = new HashMap();
		map.put("ActualHashSet.arrayStats.minIdx", "int");
		map.put("ActualHashSet.arrayStats.maxIdx", "int");
		map.put("ActualHashSet.arrayStats.idx", "int");
		map.put("ActualHashSet.arrayStats.medCnt", "int");
		map.put("ActualHashSet.arrayStats.mode", "int");
		map.put("ActualHashSet.arrayStats.modeVal", "int");
		map.put("ActualHashSet.arrayStats.totCnt", "int");
		map.put("ActualHashSet.arrayStats.cv", "int");
		map.put("ActualHashSet.arrayStats.avg", "float");
		map.put("ActualHashSet.arrayStats.oo", "PrintWriter");
		
		
		map.put("GrandHashTableTester.runTest.str", "WriteStream");
		map.put("GrandHashTableTester.stomp.table", "GrandHashTable");
		map.put("GrandHashTableTester.stomp.rGen", "Random");
		map.put("GrandHashTableTester.stomp.rNum", "int");

		map.put("MuTable.test.iTable", "IntegerTable");
		
		map.put("TextyRcvr.receiveBooleanVar.result", "char");
		
		map.put("RequestHandler.printOn.staticFn", "Fn");
		
		map.put("FlockInfo.registerInfo.shep", "Abraham");
		map.put("FlockInfo.registerInfo.cat", "Category");
		
		map.put("StaticFunctionPointer.invokeFunction.args", "Array");
		
		map.put("MainDummy.toFileRunString.aStream", "PrintWriter");
		map.put("MainDummy.toFileRunString.saveCerr", "PrintWriter");
		
		map.put("Binary2Rcvr.receiveBooleanVar.result", "int");
		
		map.put("ActualCookbook.sendClassList.rec", "Recipe");
		
		map.put("CopyRecipe.parse.result", "Heaper");
		
		map.put("Tester.tryTest.time", "long");
		map.put("Tester.tryTest.str", "PrintWriter");
		map.put("GrandHashTableTester.runTest.str", "PrintWriter");
		
		map.put("Package.makePackage.result", "Package");
		
		//TODO just for tests
		map.put("Test.test.shouldOverrideLocalVars", "int");
		
		variables = Collections.unmodifiableMap(map);
	}
	
	public OverrideLocalVarType() {
		super();
	}
	public OverrideLocalVarType(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaType.class),
				factory.token(JavaIdentifier.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier tempName = (JavaIdentifier)tokens.get(i+1);
		String shortName = javaMethod.name+"."+tempName.value;
		String fullName = javaMethod.javaClass.className+"."+shortName;
		String newType = (String)variables.get(fullName);
		if (newType == null) {
			newType = (String)variables.get(shortName);
		}
		if (newType != null) {
			JavaType type = (JavaType)tokens.get(i);
			type.value = newType;
		}
		return i;
	}
}
