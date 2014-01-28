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
import java.util.List;

import info.dgjones.abora.ug2java.JavaClass;
import info.dgjones.abora.ug2java.JavaField;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.MethodBody;
import info.dgjones.abora.ug2java.SmalltalkSource;
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.IntegerLiteral;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaToken;


/**
 * Support optional paramater by adding a new method which supplies a default value for the last
 * argument in the signature of the method. This default value is for now one of a small number
 * of simple literals. It is possible to support many optional parameters by filling in the
 * parameters left to right with suitable default values.
 * 
 * Neither Smalltalk nor Java (before JDK 1.5) support optional paramers, though C does.
 */
public class AddDefaultParameter implements ClassTransformer {

	public void transform(JavaClass javaClass) {
		if (javaClass.className.equals("SHTO")) {
			addStatic(javaClass, "SHTO", "make", new String[] {"String", "aString"}, "0");
		} else if (javaClass.className.equals("BeEdition")) {
				addInstance(javaClass, "Stepper", "retrieve", new String[] {"XnRegion", "region", "OrderSpec", "order"}, "0");
				addInstance(javaClass, "Stepper", "retrieve", new String[] {"XnRegion", "region"}, "null");
				addInstance(javaClass, "Stepper", "retrieve", new String[] {}, "null");
		} else if (javaClass.className.equals("Encrypter")) {
			addStatic(javaClass, "Encrypter", "make", new String[] {"Sequence", "identifier", "UInt8Array", "publicKey"}, "null");
		} else if (javaClass.className.equals("DiskManager")) {
			addInstance(javaClass, "void", "purgeClean", new String[] {}, "false");
		} else if (javaClass.className.equals("CrossOrderSpec")) {
			addStatic(javaClass, "CrossOrderSpec", "make", new String[] {"CrossSpace", "space"}, "null");
			addStatic(javaClass, "CrossOrderSpec", "make", new String[] {"CrossSpace", "space", "PtrArray", "subOrderings"}, "null");
		}
	}
	
	public JavaMethod addStatic(JavaClass javaClass, String returnType, String name, String[] params, String additionalParam) {
		return addStatic(javaClass, returnType, name, params, additionalParam, name);
	}
	
	public JavaMethod addStatic(JavaClass javaClass, String returnType, String name, String[] params, String additionalParam, String call) {
		return addMethod(javaClass, "static ", returnType, name, params, additionalParam, call);
	}

	public JavaMethod addInstance(JavaClass javaClass, String returnType, String name, String[] params, String additionalParam) {
		return addInstance(javaClass, returnType, name, params, additionalParam, name);
	}
	
	public JavaMethod addInstance(JavaClass javaClass, String returnType, String name, String[] params, String additionalParam, String call) {
		return addMethod(javaClass, "", returnType, name, params, additionalParam, call);
	}

	protected JavaMethod addMethod(JavaClass javaClass, String modifiers, String returnType, String name, String[] params, String additionalParam, String call) {
		JavaMethod method = new JavaMethod(returnType, name);
		List tokens = new ArrayList();
		if (!returnType.equals("void")) {
			tokens.add(new JavaKeyword("return"));
		}
		tokens.add(new JavaCallKeywordStart(call));
		for (int i = 0; i < params.length; i += 2) {
			String paramType = params[i];
			String paramName = params[i+1];
			method.addParameter(new JavaField(paramType, paramName));
			tokens.add(new JavaIdentifier(paramName));
			tokens.add(new JavaCallArgumentSeparator());
		}
		JavaToken additional;
		if (additionalParam.equals("0")) {
			additional = new IntegerLiteral(0);
		} else if (additionalParam.equals("null") || additionalParam.equals("false") || additionalParam.equals("true")) {
			additional = new JavaKeyword(additionalParam);
		} else {
			throw new IllegalArgumentException("Cant interpret additional param: "+additionalParam);
		}
		tokens.add(additional);
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = modifiers;
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddDefaultParameter";
		javaClass.addMethod(method);
		return method;
	}
}
