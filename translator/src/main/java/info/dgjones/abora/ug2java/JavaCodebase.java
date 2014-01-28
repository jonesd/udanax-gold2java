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
package info.dgjones.abora.ug2java;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Map;
import java.util.Set;



public class JavaCodebase {

	private final static Set PRIMITIVE_TYPES;
	static {
		Set set = new HashSet();
		set.add("float");
		set.add("double");
		set.add("int");
		set.add("long");
		set.add("byte");
		set.add("char");
		set.add("boolean");
		PRIMITIVE_TYPES = Collections.unmodifiableSet(set);
	}
	
	private final static Set NARROWING_PRIMITIVE_CONVERSATIONS;
	static {
		Set set = new HashSet();
		set.add("byte-char");
		
		set.add("short-byte");
		set.add("short-char");
		
		set.add("char-byte");
		set.add("char-short");
		
		set.add("int-byte");
		set.add("int-short");
		set.add("int-char");
		
		set.add("long-byte");
		set.add("long-short");
		set.add("long-char");
		set.add("long-int");
		
		set.add("float-byte");
		set.add("float-short");
		set.add("float-char");
		set.add("float-int");
		set.add("float-long");
		
		set.add("double-byte");
		set.add("double-short");
		set.add("double-char");
		set.add("double-int");
		set.add("double-long");
		set.add("double-float");
		
		NARROWING_PRIMITIVE_CONVERSATIONS = Collections.unmodifiableSet(set);
	}
	
	private final Map javaClassLookup = new HashMap();
	public final Hashtable packageLookup = new Hashtable();

	public JavaCodebase() {
		super();
	}
	
	public Collection allClasses() {
		return javaClassLookup.values();
	}
	
	public void addJavaClass(JavaClass javaClass) {
		javaClassLookup.put(javaClass.className, javaClass);
	}
	
	public JavaClass getJavaClass(String className) {
		return (JavaClass)javaClassLookup.get(className);
	}

	//TODO this kind of stuff should probably be somewhere else...
	public boolean isPrimitiveType(String typeName) {
		return PRIMITIVE_TYPES.contains(typeName);
	}

	public boolean shouldDowncast(String actualTypeName, String expectedTypeName) {
		if (isPrimitiveType(actualTypeName) || isPrimitiveType(expectedTypeName)) {
			return shouldDowncastPrimitive(actualTypeName, expectedTypeName);
		} else {
			return shouldDowncastClass(actualTypeName, expectedTypeName);
		}
	}

	private boolean shouldDowncastClass(String actualTypeName, String expectedTypeName) {
		JavaClass actualType = getJavaClass(actualTypeName);
		if (actualType == null) {
			return false;
		}
		JavaClass expectedType = getJavaClass(expectedTypeName);
		if (expectedType == null) {
			return false;
		}
		return expectedType.isSubclassAnyDepthOf(actualType);
	}
	
	private boolean shouldDowncastPrimitive(String actualTypeName, String expectedTypeName) {
		return NARROWING_PRIMITIVE_CONVERSATIONS.contains(actualTypeName+"-"+expectedTypeName);
	}

}
