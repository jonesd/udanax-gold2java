package org.abora.ug2java.transform.type;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaField;



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
