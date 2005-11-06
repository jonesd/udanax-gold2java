package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
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
		} else if (javaMethod.name.equals("endPacket") && javaMethod.javaClass.className.equals("Binary2Rcvr")) {
			rewriteBinary2RcvrEndPacket(javaMethod);
		} else if (javaMethod.getQualifiedSignature().equals("Abraham.restartAbraham(Rcvr)")) {
			rewriteAbrahamRestartAbraham(javaMethod);
		} else if (javaMethod.getQualifiedName().equals("Category.find")) {
			rewriteCategoryFind(javaMethod);
		}
	}

	/**
	 * Clear down contents - assume it is some old code that use to return whether to
	 * one had reached the end of a packet, rather than code to run when the end of
	 * a packet had been already detected.
	 */
	private void rewriteBinary2RcvrEndPacket(JavaMethod javaMethod) {
		List tokens = new ArrayList();
		tokens.add(new JavaComment("Transform: Rewrote body"));
		tokens.add(new JavaIdentifier("myStream"));
		tokens.add(new JavaCallStart("getByte"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		
		javaMethod.methodBody = new MethodBody(tokens);
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

	public void rewriteAsUntranslated(JavaMethod method) {
		
		List tokens = new ArrayList();
		tokens.add(new JavaComment("Transform: Rewrote body"));
		tokens.add(new JavaKeyword("throw"));
		tokens.add(new JavaKeyword("new"));
		tokens.add(new JavaCallKeywordStart("UnsupportedOperationException"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		
		method.methodBody = new MethodBody(tokens);
	}

	public void rewriteAbrahamRestartAbraham(JavaMethod method) {
		
		List tokens = new ArrayList();
		tokens.add(new JavaComment("Transform: Rewrote body"));
		
		tokens.add(new JavaIdentifier("myToken"));
		tokens.add(new JavaAssignment());
		tokens.add(new JavaIdentifier("TheTokenSource"));
		tokens.add(new JavaCallStart("takeToken"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		tokens.add(new JavaIdentifier("myInfo"));
		tokens.add(new JavaAssignment());
		tokens.add(new JavaIdentifier("null"));		
		tokens.add(new JavaStatementTerminator());
		
		method.methodBody = new MethodBody(tokens);
	}

	/**
	 * The original version of this is very Smalltalk specific, making extensive use 
	 * of the Smalltalk SystemDictionary for access to globals. Rewrite to use
	 * AboraSupport mechanisms. 
	 */
	public void rewriteCategoryFind(JavaMethod method) {
		
		List tokens = new ArrayList();
		tokens.add(new JavaComment("Transform: Rewrote body"));
		
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaIdentifier("AboraSupport"));
		tokens.add(new JavaCallKeywordStart("findCategory"));
		tokens.add(new JavaIdentifier("catName"));
		tokens.add(new JavaCallStart("toString"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		
		method.methodBody = new MethodBody(tokens);
		method.returnType = "Category";
	}
}
