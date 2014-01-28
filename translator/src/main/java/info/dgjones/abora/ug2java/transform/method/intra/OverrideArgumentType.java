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
import java.util.Map;
import java.util.StringTokenizer;

import info.dgjones.abora.ug2java.JavaField;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.transform.method.MethodTransformation;

public class OverrideArgumentType implements MethodTransformation {

	private static final Map METHODS;
	static {
		Map map = new HashMap();
		map.put("printOn", "PrintWriter");
		map.put("showOn", "PrintWriter");
		map.put("isEqual", "Heaper");
		map.put("DiskManager.fluidSpace", "Array");
		map.put("Heaper2UInt32Cache.make", "int");
		//TODO should we really have to override this?
		map.put("TextyRcvr.getCharToken", "char");
		map.put("ActualHashSet.arrayStats", "IntArray");
		//TODO we should be able to calculate the following based on their declared type: array {Array of: IntegerVar}
		map.put("SnarfHandler.sortTest", "IntArray");
		map.put("SnarfHandler.sortTestDown", "IntArray");
		
		map.put("NoShuffler.shuffle16", "UInt8Array");
		map.put("NoShuffler.shuffle32", "UInt8Array");
		map.put("NoShuffler.shuffle64", "UInt8Array");
		map.put("SimpleShuffler.shuffle16", "UInt8Array");
		map.put("SimpleShuffler.shuffle32", "UInt8Array");
		map.put("SimpleShuffler.shuffle64", "UInt8Array");
		map.put("ByteShuffler.shuffle16", "UInt8Array");
		map.put("ByteShuffler.shuffle32", "UInt8Array");
		map.put("ByteShuffler.shuffle64", "UInt8Array");
		map.put("ByteShuffler.shuffle(int,PtrArray,int)", "int,UInt8Array,int");
		
		map.put("Category.Category", "AboraClass");
		map.put("TupleStepper.make(Object,Object)", "CrossSpace,PtrArray");
		map.put("Category.sendSelfTo(Object)", "Xmtr");
		map.put("XnReadStream.getBytes(PtrArray,int)", "UInt8Array,int");
		map.put("XnReadStream.getBytes(PtrArray,int,int)", "UInt8Array,int,int");
		map.put("XnBufferedReadStream.getBytes(PtrArray,int,int)", "UInt8Array,int,int");
		map.put("Emulsion.initImageEmulsions", "AboraClass");
		map.put("MainDummy.run", "String");
		map.put("MainDummy.runString", "String");
		map.put("MainDummy.XUUMAIN(int,String)", "int,Array");
		map.put("ServerChunk.fluidSpace", "Array");
		map.put("TextyRcvr.getIdentifier", "UInt8Array,int");
		map.put("Category.create", "Rcvr");
		map.put("MuSet.isEqual", "Heaper");
		
		map.put("Recipe.Recipe(Category,Recipe)", "Category,Association");
		map.put("StubRecipe.StubRecipe(Category,Recipe)", "Category,Association");
		map.put("CopyRecipe.CopyRecipe(Category,Recipe)", "Category,Association");
		map.put("CategoryRecipe.CategoryRecipe(Category,Recipe)", "Category,Association");

		METHODS = Collections.unmodifiableMap(map);
	}

	
	public void transform(JavaMethod javaMethod) {
		String shortName = javaMethod.name;
		String fullName = javaMethod.javaClass.className+"."+shortName;
		String parameterName = javaMethod.getQualifiedSignature();
		if (!javaMethod.parameters.isEmpty() && (METHODS.containsKey(shortName) || METHODS.containsKey(fullName) || METHODS.containsKey(parameterName))) {
			String typeNames = (String)METHODS.get(parameterName);
			if (typeNames == null) {
				typeNames = (String)METHODS.get(fullName);
			}
			if (typeNames == null) {
				typeNames = (String)METHODS.get(shortName);
			}
			int argument = 0;
			for (StringTokenizer tokenizer = new StringTokenizer(typeNames, ","); tokenizer.hasMoreTokens();) {
				String typeName = tokenizer.nextToken();
				JavaField field =  (JavaField)javaMethod.parameters.get(argument);
				field.type = typeName;
				argument += 1;
			}
		}
	}

}