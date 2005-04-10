package org.abora.ug2java;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;



public class ClassTransformers implements ClassTransformer {

	private final List transformers;
	
	public ClassTransformers() {
		List list = new ArrayList();
		list.add(new OverrideClassVariables());
		list.add(new TransformReceiverConstructor());
		transformers = Collections.unmodifiableList(list);
	}
	
	
	public void transform(JavaClass javaClass) {
		for (Iterator iter = transformers.iterator(); iter.hasNext();) {
			ClassTransformer transformer = (ClassTransformer) iter.next();
			transformer.transform(javaClass);
		}
	}
}
