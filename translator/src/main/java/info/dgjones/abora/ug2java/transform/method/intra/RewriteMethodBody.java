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
package info.dgjones.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.List;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.MethodBody;
import info.dgjones.abora.ug2java.javatoken.JavaAssignment;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaComment;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.transform.method.MethodTransformation;



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
