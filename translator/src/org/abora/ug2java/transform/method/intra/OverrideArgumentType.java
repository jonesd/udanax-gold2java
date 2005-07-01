/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;

public class OverrideArgumentType implements MethodTransformation {

	private static final Map METHODS;
	static {
		Map map = new HashMap();
		map.put("printOn", "PrintWriter");
		map.put("showOn", "PrintWriter");
		map.put("DiskManager.fluidSpace", "Array");
		map.put("Heaper2UInt32Cache.make", "int");
		//TODO should we really have to override this?
		map.put("TextyRcvr.getCharToken", "char");
		map.put("ActualHashSet.arrayStats", "IntArray");
		//TODO we should be able to calculate the following based on their declared type: array {Array of: IntegerVar}
		map.put("SnarfHandler.sortTest", "IntArray");
		map.put("SnarfHandler.sortTestDown", "IntArray");
		map.put("SimpleShuffler.shuffle16", "UInt8Array");
		map.put("SimpleShuffler.shuffle32", "UInt8Array");
		map.put("SimpleShuffler.shuffle64", "UInt8Array");
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