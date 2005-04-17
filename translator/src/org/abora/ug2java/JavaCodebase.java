package org.abora.ug2java;

import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;



public class JavaCodebase {

	private final Map javaClassLookup = new HashMap();
	public final Hashtable packageLookup = new Hashtable();

	public JavaCodebase() {
		super();
	}
	
	public void addJavaClass(JavaClass javaClass) {
		javaClassLookup.put(javaClass.className, javaClass);
	}
	
	public JavaClass getJavaClass(String className) {
		return (JavaClass)javaClassLookup.get(className);
	}

}
