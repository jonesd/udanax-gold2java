/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;

public class ExcludeStaticHeaper implements MethodTransformation {

	private static final List KEEP;
	static {
		List list = new ArrayList();
		list.add("passe");
		//list.add("takeOop");
		
		list.add("Heaper");
		list.add("actualHashForEqual");
		list.add("destroy");
		list.add("destruct");
		//list.add("destructor");
		//list.add("delete");
		list.add("equals");
		//list.add("getCategory");
		list.add("hash");
		list.add("hashForEqual");
		list.add("isEqual");
		list.add("printOn");
		list.add("sendSelfTo");

		KEEP = Collections.unmodifiableList(list);
	}

	
	public void transform(JavaMethod javaMethod) {
		if (javaMethod.javaClass.className.equals("Heaper")) {
			if (!KEEP.contains(javaMethod.name)) {
				javaMethod.shouldInclude = false;
			}
		}
	}

}