package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;



public class TransformMethod {

	final private JavaClass javaClass;
	
	
	public TransformMethod(JavaClass classWriter) {
		this.javaClass = classWriter;
	}
	
	public void transform(JavaMethod javaMethod) {
		new TransformAndOrs().transform(javaMethod);
		ensureIfTestInParentheses(javaMethod.methodBody);
		ensureReasonableStatementTerminators(javaMethod.methodBody);
		new TransformSelfSends().transform(javaMethod);
		new TransformUses().transform(javaMethod);
		new TransformSmalltalkOnly().transform(javaMethod);
		new TransformTranslateOnlyString().transform(javaMethod);
		new TransformSuperCreate().transform(javaMethod);
		new TransformCreateCall().transform(javaMethod);
		new TransformWhileTrue().transform(javaMethod);
		new TransformTimesRepeat().transform(javaMethod);
		new TransformCritical().transform(javaMethod);
		new TransformValueNowOrOnOnUnwindDo().transform(javaMethod);
		new TransformOverrideCalls().transform(javaMethod);
		new TransformBitAndOrXor().transform(javaMethod);
		new TransformBlast().transform(javaMethod);
		new TransformIsKindOf().transform(javaMethod);
		new TransformCast().transform(javaMethod);
		new TransformAlmostTo().transform(javaMethod);
		new TransformSubclassResponsibility().transform(javaMethod);
		new TransformStaticCall().transform(javaMethod);
		new TransformReturnVoid().transform(javaMethod);
		new TransformClassReference().transform(javaMethod);
	}


	protected void ensureReasonableStatementTerminators(MethodBody body) {
		List tokens = body.tokens;
		//TODO we are ignoring comments?
		if (tokens.isEmpty()) {
			return;
		}
		if (!(tokens.get(tokens.size() - 1) instanceof JavaStatementTerminator) || !(tokens.get(tokens.size() - 1) instanceof JavaBlockEnd)) {
			tokens.add(new JavaStatementTerminator());
		}
		for (int i = tokens.size() - 1; i >= 1; i--) {
			JavaToken token = (JavaToken) tokens.get(i);
			if (token instanceof JavaBlockEnd) {
				JavaToken previousToken = (JavaToken) tokens.get(i - 1);
				if (!(previousToken instanceof JavaStatementTerminator) || !(previousToken instanceof JavaBlockStart)) {
					tokens.add(i, new JavaStatementTerminator());
				}
			}
		}
		for (int i = tokens.size() - 1; i >= 1; i--) {
			JavaToken token = (JavaToken) tokens.get(i);
			if (token instanceof JavaStatementTerminator) {
				JavaToken previousToken = (JavaToken) tokens.get(i - 1);
				if ((previousToken instanceof JavaStatementTerminator)
					|| (previousToken instanceof JavaBlockStart)
					|| (previousToken instanceof JavaBlockEnd)) {
					tokens.remove(i);
				}
			}
		}
	}

	protected void ensureIfTestInParentheses(MethodBody body) {
		List tokens = body.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.get(i);
			if ((token instanceof JavaKeyword) && token.value.equals("if") && !(tokens.get(i + 1) instanceof JavaParenthesisStart)) {
				int earlyParentheses = 0;
				int earlyBlocks = 0;
				boolean found = false;
				for (int j = i; j < tokens.size(); j++) {
					JavaToken test = (JavaToken) tokens.get(j);
					if (test instanceof JavaParenthesisStart) {
						earlyParentheses++;
					} else if (test instanceof JavaParenthesisEnd) {
						earlyParentheses--;
					} else if (test instanceof JavaBlockEnd) {
						earlyBlocks--;
					} else if (test instanceof JavaBlockStart) {
						if (earlyParentheses > 0 || earlyBlocks > 0) {
							earlyBlocks++;
						} else {
							found = true;
							tokens.add(j, new JavaParenthesisEnd());
							break;
						}
					}
				}
				if (!found) {
					throw new IllegalStateException("Couldn't find suitable location to insert missing ) for if");
				}
				tokens.add(i + 1, new JavaParenthesisStart());
			}
		}
	}

}
