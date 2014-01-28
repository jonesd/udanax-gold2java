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
package info.dgjones.abora.ug2java.transform.type;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import info.dgjones.abora.ug2java.JavaClass;
import info.dgjones.abora.ug2java.JavaField;



public class OverrideClassVariables implements ClassTransformer {

	private static final Map METHODS;
	static {
		Map map = new HashMap();
		map.put("DiskManager.myFluidSpace", "Array");
		map.put("DeleteExecutor.StorageArray", "PtrArray");

		map.put("ActualHashSet.AddTallys", "IntArray");
		map.put("ActualHashSet.DeleteTallys", "IntArray");
		map.put("ActualHashSet.StepperTally", "IntArray");
		map.put("ActualHashSet.TestTallys", "IntArray");
		
		map.put("ReadMemStream.myBuffer", "UInt8Array");
		map.put("WriteMemStream.myCollection", "UInt8Array");
		
		map.put("Emulsion.TheImageEmulsion", "Emulsion");
		map.put("Emulsion.myFluids", "OrderedCollection");
		map.put("Emulsion.myFluidsUsed", "boolean");
		
		map.put("Category.myClass", "AboraClass");
		
		map.put("ServerChunk.myFluidSpace", "Array");
		
		map.put("ListenerEmulsion.defaultFluidSpace", "Array");
		
		map.put("TextyRcvr.ReceiveStringBuffer", "UInt8Array");

		METHODS = Collections.unmodifiableMap(map);
	}

	
	private String overrideWith(JavaClass javaClass, JavaField field) {
		String shortName = field.name;
		String fullName = javaClass.className+"."+shortName;
		String type = (String)METHODS.get(fullName);
		if (type == null) {
			type = (String)METHODS.get(shortName);
		}
		return type;
	}
	
	public void transform(JavaClass javaClass) {
		for (Iterator iter = javaClass.fields.iterator(); iter.hasNext();) {
			JavaField field = (JavaField) iter.next();
			String replacementType = overrideWith(javaClass, field);
			if (replacementType != null) {
				field.type = replacementType;
			}
		}
	}
}
