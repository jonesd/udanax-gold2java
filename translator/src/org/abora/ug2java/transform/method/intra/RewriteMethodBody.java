package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.MethodTransformation;



public class RewriteMethodBody implements MethodTransformation {

	
	public void transform(JavaMethod javaMethod) {
		if (javaMethod.name.equals("isANumber")
				&& (javaMethod.javaClass.className.equals("PrimIEEE32") || javaMethod.javaClass.className.equals("PrimIEEE64"))) {
			rewritePrimIEEE64isANumber(javaMethod);
		}
	}
	
	public void rewritePrimIEEE64isANumber(JavaMethod method) {
		
		List tokens = new ArrayList();
		tokens.add(new JavaComment("Transform: Rewrote body"));
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaIdentifier("AboraSupport"));
		tokens.add(new JavaCallKeywordStart("isANumber"));
		tokens.add(new JavaIdentifier("myValue"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		
		method.methodBody = new MethodBody(tokens);
	}

}
