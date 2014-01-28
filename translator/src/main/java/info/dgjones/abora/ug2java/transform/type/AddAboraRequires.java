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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import info.dgjones.abora.ug2java.Annotation;
import info.dgjones.abora.ug2java.JavaClass;
import info.dgjones.abora.ug2java.JavaField;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.MethodBody;
import info.dgjones.abora.ug2java.SmalltalkSource;
import info.dgjones.abora.ug2java.javatoken.JavaBlockEnd;
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaLiteral;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;



public class AddAboraRequires implements ClassTransformer {

	public void transform(JavaClass javaClass) {
		if (javaClass.className.equals("CalcCreator")) {
			addRequires(javaClass, "Recipe");
			
		} else if (javaClass.className.equals("BootPlan")) {
			addRequires(javaClass, "Recipe");
			
		} else if (javaClass.className.equals("BackendBootMaker")) {
			addRequires(javaClass, "Recipe");

		}
	}

	private void addRequires(JavaClass javaClass, String requiredClassName) {
		JavaMethod javaMethod = javaClass.getMethod("initTimeNonInherited");
		
		//TODO next section duplicated from TransformRequires...
		Set required = (Set)javaMethod.getAnnotations().get(Annotation.REQUIRES);
		if (required == null) {
			required = new HashSet();
			javaMethod.getAnnotations().put(Annotation.REQUIRES, required);
		}

		required.add(requiredClassName);
	}
	
}
