package org.abora.ug2java;

import java.util.HashMap;
import java.util.Map;



public class Annotation {
	
	
	private final Map annotations = new HashMap();

	//TODO convert to bloch enumeration
	public static final String PROBLEM_SIGNALS = "Signals";

	public Annotation() {
		super();
	}
	
	public Object get(String annotationKey) {
		return annotations.get(annotationKey);
	}
	
	public Object getIfNone(String annotationKey, Object defaultValue) {
		if (annotations.containsKey(annotationKey)) {
			return get(annotationKey);
		} else {
			return defaultValue;
		}
	}
	
	public void put(String annotationKey, Object value) {
		annotations.put(annotationKey, value);
	}

}
