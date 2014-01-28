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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import info.dgjones.abora.ug2java.util.ToStringGenerator;



public class JavaMethod extends JavaClassElement {
	public String modifiers = "";
	public String returnType;
	public MethodBody methodBody;
	public String name;
	public SmalltalkSource smalltalkSource;
	public String comment;
	public JavaClass javaClass;
	public List localVariables = new ArrayList();
	public List parameters = new ArrayList();
	public boolean shouldInclude = true;
	public boolean isDeprecated = false;
	private final Annotation annotations = new Annotation(); 
	public String methodCategory = "";
	
	public JavaMethod() {
		super();
	}
	
	public JavaMethod(String returnType, String name) {
		this.returnType = returnType;
		this.name = name;
	}
	
	public String findTypeOfVariable(String variableName) {
		String type = findTypeOfVariable(variableName, localVariables);
		if (type == null) {
			type = findTypeOfVariable(variableName, parameters);
		}
		if (type == null) {
			type = javaClass.findTypeOfVariable(variableName); 
		}
		return type;
	}

	private String findTypeOfVariable(String variableName, List javaFields) {
		for (Iterator iter = javaFields.iterator(); iter.hasNext();) {
			JavaField javaField = (JavaField) iter.next();
			if (javaField.name.equals(variableName)) {
				return javaField.type;
			}
		}
		return null;
	}

	public JavaCodebase getJavaCodebase() {
		return javaClass.getJavaCodebase();
	}

	public void addLocalVariable(JavaField field) {
		localVariables.add(field);
	}

	public boolean isStatic() {
		//TODO lame implementation...
		return modifiers.indexOf("static") != -1;
	}

	public void addParameter(JavaField parameter) {
		parameters.add(parameter);
	}
	
	public Annotation getAnnotations() {
		return annotations;
	}
	
	public String toString() {
		ToStringGenerator generator = new ToStringGenerator(this);
		generator.add("name", name);
		generator.add("class", javaClass);
		return generator.end();
	}

	public JavaField getParameter(int i) {
		return (JavaField)parameters.get(i);
	}
	
	public String getName() {
		return name;
	}
	
	public String getQualifiedName() {
		return javaClass.className+"."+getName();
	}
	
	public String getSignature() {
		return getName()+getParameterDeclaration();
	}
	
	public String getQualifiedSignature() {
		return getQualifiedName() + getParameterDeclaration();
	}
	
	private String getParameterDeclaration() {
		StringBuffer buffer = new StringBuffer();
		buffer.append('(');
		for (Iterator iter = parameters.iterator(); iter.hasNext();) {
			JavaField parameter = (JavaField) iter.next();
			buffer.append(parameter.type);
			if(iter.hasNext()) {
				buffer.append(',');
			}
		}
		buffer.append(')');
		return buffer.toString();
	}

	public boolean isConstructor() {
		// TODO proper implementation
		return getName().equals(javaClass.className);
	}

}
