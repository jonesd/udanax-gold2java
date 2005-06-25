/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.MethodTransformation;



public class TransformPasseMethodCategory implements MethodTransformation {

	public void transform(JavaMethod javaMethod) {
		if (javaMethod.methodCategory.indexOf("passe") != -1) {
			// TODO duplicated behaviour from TransformPasse...
			List tokens = new ArrayList();
			tokens.add(new JavaKeyword("throw"));
			tokens.add(new JavaKeyword("new"));
			tokens.add(new JavaCallKeywordStart("PasseException"));
			tokens.add(new JavaCallEnd());
			tokens.add(new JavaStatementTerminator());
			javaMethod.methodBody = new MethodBody(tokens);

			javaMethod.isDeprecated = true;
		}
	}

}
