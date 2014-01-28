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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import info.dgjones.abora.ug2java.JavaClass;
import info.dgjones.abora.ug2java.JavaField;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.MethodBody;
import info.dgjones.abora.ug2java.SmalltalkSource;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;



public class TransformReceiverConstructor implements ClassTransformer {

	public void transform(JavaClass javaClass) {
		if (javaClass.className.equals("Heaper")) {
			return;
		}
		boolean foundEmptyConstructor = false;
		boolean foundRcvrConstructor = false;
		for (Iterator iter = javaClass.methods.iterator(); iter.hasNext();) {
			JavaMethod method = (JavaMethod) iter.next();
			if (method.name.equals(javaClass.className) && method.parameters.isEmpty()) {
				foundEmptyConstructor = true;
			}
			if (method.name.equals(javaClass.className) && method.parameters.size() == 1 && ((JavaField)method.parameters.get(0)).type.equals("Rcvr")) {
				foundRcvrConstructor = true;
			}
		}
		if (!foundEmptyConstructor) {
			javaClass.methods.add(createEmptyConstructor(javaClass));
		}
		if (!foundRcvrConstructor) {
			javaClass.methods.add(createReceiverConstructor(javaClass));
		}
	}

	private JavaMethod createReceiverConstructor(JavaClass javaClass) {
		JavaMethod method = new JavaMethod();
		method.javaClass = javaClass;
		method.modifiers = "";
		method.name = javaClass.className;
		method.parameters.add(new JavaField("", "Rcvr", "receiver"));
		method.returnType = "";
		method.comment = null;
		
		SmalltalkSource source = new SmalltalkSource();
		source.context = "";
		source.text = "Generated during transformation";
		method.smalltalkSource = source;
		
		List tokens = new ArrayList();
		tokens.add(new JavaCallKeywordStart("super"));
		tokens.add(new JavaIdentifier("receiver"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		MethodBody body = new MethodBody(tokens);		
		method.methodBody = body;
		
		return method;
	}

	private JavaMethod createEmptyConstructor(JavaClass javaClass) {
		JavaMethod method = new JavaMethod();
		method.javaClass = javaClass;
		method.modifiers = "";
		method.name = javaClass.className;
		method.returnType = "";
		method.comment = null;
		
		SmalltalkSource source = new SmalltalkSource();
		source.context = "";
		source.text = "Generated during transformation";
		method.smalltalkSource = source;
		
		List tokens = new ArrayList();
		MethodBody body = new MethodBody(tokens);		
		method.methodBody = body;
		
		return method;
	}
}
