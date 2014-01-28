/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaType;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



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
		
		map.put("Recipe.mapCuisine.d", "Association");
		map.put("Recipe.staticTimeNonInherited.cxc", "CxxClassDescription");
		
		map.put("Category.create.catName", "String");
		
		map.put("VolumeTester.allTestsOn.protoArray", "UInt8Array");
		map.put("VolumeTester.allTestsOn.count", "int");
		map.put("VolumeTester.allTestsOn.chunk", "int");
		map.put("VolumeTester.allTestsOn.result", "FeEdition");
		map.put("VolumeTester.allTestsOn.server", "FeServer");
		map.put("VolumeTester.allTestsOn.stepper", "RandomStepper");
		
		
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
