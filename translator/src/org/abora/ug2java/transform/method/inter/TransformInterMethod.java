
/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.inter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;



public class TransformInterMethod implements MethodTransformation {

	final private List transformers;
	
	
	public TransformInterMethod() {
		this(createTransformers());
	}
	
	public TransformInterMethod(List transformers) {
		this.transformers = transformers;
	}
	
	private static List createTransformers() {		
		List transformers = new ArrayList();
		
		transformers.add(new TransformDowncastStaticCallAssignment());
		
		return Collections.unmodifiableList(transformers);
	}
	
	public void transform(JavaMethod javaMethod) {
		for (Iterator iter = transformers.iterator(); iter.hasNext();) {
			MethodTransformation transformation = (MethodTransformation) iter.next();
			transformation.transform(javaMethod);
		}
	}

}
