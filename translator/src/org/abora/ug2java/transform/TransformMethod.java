package org.abora.ug2java.transform;

import java.util.StringTokenizer;
import java.util.Vector;

import org.abora.ug2java.ClassWriter;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.JavaType;



public class TransformMethod {

	final private ClassWriter classWriter;
	
	
	public TransformMethod(ClassWriter classWriter) {
		this.classWriter = classWriter;
	}
	
	public void transform(MethodBody methodBody) {
		transformAndOrs(methodBody);
		ensureIfTestInParentheses(methodBody);
		ensureReasonableStatementTerminators(methodBody);
		trimSelfSends(methodBody);
		trimUses(methodBody);
		transformSmalltalkOnly(methodBody);
		transformTranslateOnlyString(methodBody);
		transformSuperCreate(methodBody);
		transformCreateCall(methodBody);
		transformWhileTrue(methodBody);
		transformTimesRepeat(methodBody);
		transformCritical(methodBody);
		transformValueNowOrOnUnwindDo(methodBody);
		transformOverrideCalls(methodBody);
		transformBitAndOrXor(methodBody);
		transformBlasts(methodBody);
		transformIsKindOf(methodBody);
		transformCasts(methodBody);
		transformAlmostTo(methodBody);
		transformSubclassResponsibility(methodBody);
		transformStaticCall(methodBody);
		transformReturnVoid(methodBody);
		transformClassReference(methodBody);
	}

	protected void transformWhileTrue(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 2; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaCallKeywordStart) && (token.value.equals("whileTrue"))) {
				if ((tokens.elementAt(i - 1) instanceof JavaBlockEnd) && (tokens.elementAt(i - 2) instanceof JavaStatementTerminator)) {
					int preBlockStart = body.findStartOfBlock(i - 1);
					tokens.add(preBlockStart, new JavaKeyword("while"));
					tokens.remove(preBlockStart + 1);
					tokens.add(preBlockStart + 1, new JavaParenthesisStart());
					tokens.remove(i - 1); // ;
					tokens.remove(i - 1); // }					
					tokens.add(i - 1, new JavaParenthesisEnd());
					int postCallEnd = body.findClosingCallEnd(i);
					if (postCallEnd + 1 < tokens.size() && (tokens.elementAt(postCallEnd + 1) instanceof JavaStatementTerminator)) {
						tokens.remove(postCallEnd + 1);
					}
					tokens.remove(postCallEnd);
					tokens.remove(i);
				}
			}
		}
	}

	protected void transformValueNowOrOnUnwindDo(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 1; i < tokens.size() - 2; i++) {
			JavaToken end = (JavaToken) tokens.elementAt(i);
			if (end instanceof JavaBlockEnd) {
				JavaToken token = (JavaToken) tokens.elementAt(i + 1);
				if ((token instanceof JavaCallKeywordStart) && (token.value.equals("valueNowOrOnUnwindDo"))) {
					if (tokens.elementAt(i + 2) instanceof JavaBlockStart) {
						int start = body.findStartOfBlock(i);
						int postCallEnd = body.findClosingCallEnd(i + 1);
						if (postCallEnd + 1 < tokens.size() && (tokens.elementAt(postCallEnd + 1) instanceof JavaStatementTerminator)) {
							tokens.remove(postCallEnd + 1);
						}
						tokens.remove(postCallEnd);
						tokens.remove(i + 1);
						tokens.add(i + 1, new JavaKeyword("finally"));
						tokens.add(start, new JavaKeyword("try"));
					}
				}
			}
		}
	}

	protected void transformTranslateOnlyString(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size() - 2; i++) {
			JavaToken str = (JavaToken) tokens.elementAt(i);
			if (str instanceof JavaLiteral) {
				JavaToken call = (JavaToken) tokens.elementAt(i + 1);
				if (call instanceof JavaCallStart && call.value.equals("translateOnly")) {
					JavaToken callEnd = (JavaToken) tokens.elementAt(i + 2);
					if (callEnd instanceof JavaCallEnd) {
						tokens.add(i, new JavaComment("translateOnly " + str.value));
						tokens.remove(i + 1);
						tokens.remove(i + 1);
						tokens.remove(i + 1);
						if (i + 1 < tokens.size() && tokens.elementAt(i + 1) instanceof JavaStatementTerminator) {
							tokens.remove(i + 1);
						}
					}
				}
			}
		}
	}

	protected void transformTimesRepeat(MethodBody body) {
		final String incrementVariable = "i";
		Vector tokens = body.tokens;
		for (int i = 1; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaCallKeywordStart) && (token.value.equals("timesRepeat"))) {
				if (tokens.elementAt(i + 1) instanceof JavaBlockStart) {
					int start = body.findStartOfExpression(i - 1);
					int postCallEnd = body.findClosingCallEnd(i);
					if (postCallEnd + 1 < tokens.size() && (tokens.elementAt(postCallEnd + 1) instanceof JavaStatementTerminator)) {
						tokens.remove(postCallEnd + 1);
					}
					tokens.remove(postCallEnd);
					tokens.remove(i);
					tokens.add(i, new JavaKeyword(";"));
					tokens.add(i + 1, new JavaIdentifier(incrementVariable));
					tokens.add(i + 2, new JavaKeyword("++"));
					tokens.add(i + 3, new JavaParenthesisEnd());
					tokens.add(start, new JavaKeyword("for"));
					tokens.add(start + 1, new JavaParenthesisStart());
					tokens.add(start + 2, new JavaType("int"));
					tokens.add(start + 3, new JavaIdentifier(incrementVariable));
					tokens.add(start + 4, new JavaAssignment());
					tokens.add(start + 5, new JavaLiteral("0"));
					tokens.add(start + 6, new JavaKeyword(";"));
					tokens.add(start + 7, new JavaIdentifier(incrementVariable));
					tokens.add(start + 8, new JavaKeyword("<"));
				}
			}
		}
	}

	protected void transformSuperCreate(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size() - 1; i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if (token instanceof JavaIdentifier && token.value.equals("super")) {
				JavaToken call = (JavaToken) tokens.elementAt(i + 1);
				if (call instanceof JavaCallStart && call.value.equals("create")) {
					call.value = "super";
					tokens.remove(i);
				}
			}
		}
	}

	protected void transformSubclassResponsibility(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken call = (JavaToken) tokens.elementAt(i);
			if (call instanceof JavaCallStart && call.value.equals("subclassResponsibility")) {
				tokens.add(i, new JavaKeyword("throw"));
				tokens.add(i + 1, new JavaKeyword("new"));
				call.value = "SubclassResponsibilityException";
				classWriter.includeImportForType("SubclassResponsibilityException");
				i+=2;
			}
		}
	}

	protected void transformStaticCall(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size() - 2; i++) {
			JavaToken call = (JavaToken) tokens.elementAt(i);
			if (call instanceof JavaCallStart && call.value.equals("class")) {
				JavaToken callEnd = (JavaToken) tokens.elementAt(i + 1);
				if (callEnd instanceof JavaCallEnd) {
					JavaToken nextCall = (JavaToken) tokens.elementAt(i + 2);
					if (nextCall instanceof JavaCallStart) {
						tokens.add(i, new JavaIdentifier(classWriter.className));
						tokens.remove(i + 1);
						tokens.remove(i + 1);
					}
				}
			}
		}
	}

	protected void transformSmalltalkOnly(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 1; i < tokens.size() - 1; i++) {
			JavaToken blockEnd = (JavaToken) tokens.elementAt(i);
			if (blockEnd instanceof JavaBlockEnd) {
				JavaToken call = (JavaToken) tokens.elementAt(i + 1);
				if (call instanceof JavaIdentifier && call.value.equals("smalltalkOnly")) {
					int blockStart = body.findStartOfBlock(i);
					tokens.remove(blockStart);
					tokens.add(blockStart, new JavaComment(">>> smalltalkOnly"));
					tokens.remove(blockEnd);
					tokens.add(i, new JavaComment("<<< smalltalkOnly"));
					tokens.remove(i + 1);
					if (i + 1 < tokens.size() && tokens.elementAt(i + 1) instanceof JavaStatementTerminator) {
						tokens.remove(i + 1);
					}
				}
			}
		}
	}

	protected void transformReturnVoid(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size() - 1; i++) {
			JavaToken call = (JavaToken) tokens.elementAt(i);
			if (call instanceof JavaKeyword && call.value.equals("return")) {
				JavaToken value = (JavaToken) tokens.elementAt(i + 1);
				if (value instanceof JavaIdentifier && value.value.equals("VOID")) {
					tokens.remove(i + 1);
				}
			}
		}
	}

	protected void transformOverrideCalls(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaCallStart) && ClassWriter.OVERRIDE_CALLS.get(token.value) != null) {
				token.value = (String) ClassWriter.OVERRIDE_CALLS.get(token.value);
			}
		}
	}

	protected void transformIsKindOf(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size() - 3; i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaIdentifier)) {
				JavaToken call = (JavaToken) tokens.elementAt(i + 1);
				if ((call instanceof JavaCallKeywordStart) && call.value.equals("isKindOf")) {
					JavaToken type = (JavaToken) tokens.elementAt(i + 2);
					if (type instanceof JavaIdentifier) {
						if (tokens.elementAt(i + 3) instanceof JavaCallEnd) {
							tokens.remove(i + 3);
							tokens.remove(i + 1);
							tokens.add(i + 1, new JavaKeyword("instanceof"));
						}
					}
				}
			}
		}
	}

	protected void transformCritical(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 1; i < tokens.size() - 1; i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaCallKeywordStart) && (token.value.equals("critical"))) {
				if (tokens.elementAt(i + 1) instanceof JavaBlockStart) {
					int start = body.findStartOfExpression(i - 1);
					int postCallEnd = body.findClosingCallEnd(i);
					if (postCallEnd + 1 < tokens.size() && (tokens.elementAt(postCallEnd + 1) instanceof JavaStatementTerminator)) {
						tokens.remove(postCallEnd + 1);
					}
					tokens.remove(postCallEnd);
					tokens.remove(i);
					tokens.add(i, new JavaParenthesisEnd());
					tokens.add(start, new JavaKeyword("synchronized"));
					tokens.add(start + 1, new JavaParenthesisStart());
				}
			}
		}
	}

	protected void transformCreateCall(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken call = (JavaToken) tokens.elementAt(i);
			if (call instanceof JavaCallStart && call.value.equals("create")) {
				if (i > 0 && (tokens.elementAt(i - 1) instanceof JavaIdentifier)) {
					JavaToken token = (JavaToken) tokens.elementAt(i - 1);
					if (token.value.equals("super")) {
						continue;
					}
					call.value = token.value;
					classWriter.includeImportForType(call.value);
					tokens.remove(i - 1);
					tokens.add(i - 1, new JavaKeyword("new"));
				} else {
					call.value = classWriter.className;
					tokens.add(i, new JavaKeyword("new"));
				}
			}
		}
	}

	protected void transformClassReference(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if (token instanceof JavaIdentifier) {
				if (i < tokens.size() - 1 && (tokens.elementAt(i + 1) instanceof JavaCallStart)) {
					continue;
				}
				if (i > 0) {
					JavaToken pre = (JavaToken) tokens.elementAt(i - 1);
					if ((pre instanceof JavaKeyword) && pre.value.equals("instanceof")) {
						continue;
					}
				}
				if (classWriter.packageLookup.get(token.value) != null) {
					tokens.add(i + 1, new JavaCallStart("getCategory"));
					tokens.add(i + 2, new JavaCallEnd());
				}
			}
		}
	}

	protected void transformCasts(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 1; i < tokens.size() - 2; i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if (token instanceof JavaCallKeywordStart && (token.value.equals("cast") || token.value.equals("quickCast"))) {
				JavaToken type = (JavaToken) tokens.elementAt(i + 1);
				if (type instanceof JavaIdentifier) {
					if (tokens.elementAt(i + 2) instanceof JavaCallEnd) {
						int start = body.findStartOfExpression(i - 1);
						tokens.remove(i + 2);
						tokens.remove(i + 1);
						tokens.remove(i);
						tokens.add(start, new JavaCast(type.value));
					}
				}
			}
		}
	}

	protected void transformBlasts(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size() - 3; i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaIdentifier) && token.value.equals("Heaper")) {
				JavaToken call = (JavaToken) tokens.elementAt(i + 1);
				if ((call instanceof JavaCallKeywordStart) && call.value.equals("BLAST")) {
					JavaToken message = (JavaToken) tokens.elementAt(i + 2);
					if (message instanceof JavaIdentifier) {
						if (tokens.elementAt(i + 3) instanceof JavaCallEnd) {
							tokens.remove(i);
							tokens.add(i, new JavaKeyword("throw"));
							tokens.add(i + 1, new JavaKeyword("new"));
							call.value = "AboraRuntimeException";
							classWriter.includeImportForType(call.value);
							message.value = "AboraRuntimeException." + message.value;
						}
					}
				}
			}
	
		}
	}

	protected void transformBitAndOrXor(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 1; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaCallKeywordStart)
				&& (token.value.equals("bitAnd") || token.value.equals("bitOr") || token.value.equals("bitXor"))) {
				int closingIndex = body.findClosingCallEnd(i);
				tokens.remove(closingIndex);
				tokens.remove(i);
				if (token.value.equals("bitAnd")) {
					tokens.add(i, new JavaKeyword("&"));
				} else if (token.value.equals("bitOr")) {
					tokens.add(i, new JavaKeyword("|"));
				} else {
					tokens.add(i, new JavaKeyword("^"));
				}
			}
		}
	}

	protected void transformAndOrs(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaCallKeywordStart) && (token.value.equals("and") || token.value.equals("or"))) {
				if (!(tokens.elementAt(i + 1) instanceof JavaBlockStart)) {
					throw new IllegalStateException("Expected { after short circuit (");
				}
				int closingIndex = body.findEndOfBlock(i + 1);
				if (!(tokens.elementAt(closingIndex + 1) instanceof JavaCallEnd)
					&& (!(tokens.elementAt(closingIndex + 1) instanceof JavaComment) && !(tokens.elementAt(closingIndex + 2) instanceof JavaCallEnd))) {
					throw new IllegalStateException("short circuit not properly terminated with )");
				}
				tokens.remove(closingIndex + 1);
				tokens.remove(closingIndex);
				tokens.add(closingIndex, new JavaParenthesisEnd());
				tokens.remove(i + 1);
				tokens.remove(i);
				String value;
				if (token.value.equals("and")) {
					value = "&&";
				} else {
					value = "||";
				}
				tokens.add(i, new JavaKeyword(value));
				tokens.add(i + 1, new JavaParenthesisStart());
			}
		}
	}

	protected void transformAlmostTo(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 1; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaCallKeywordStart) && (token.value.equals("almostToDo"))) {
				int expressionStart = body.findStartOfExpression(i - 1);
				tokens.add(expressionStart, new JavaKeyword("for"));
				tokens.add(expressionStart + 1, new JavaParenthesisStart());
	
				int blockStart = body.findNextTokenOfType(i + 2, JavaBlockStart.class);
				JavaToken variableType = (JavaToken) tokens.elementAt(blockStart + 1);
				if (!variableType.value.equals("int")) {
					System.out.println("-- Warning: Non-int almostToDo variable");
					//	throw new IllegalStateException("Non-int almostToDo variable");
				}
				JavaToken variable = (JavaToken) tokens.elementAt(blockStart + 2);
				tokens.add(expressionStart + 2, variableType);
				tokens.add(expressionStart + 3, variable);
				tokens.add(expressionStart + 4, new JavaAssignment());
				tokens.add(i + 5, new JavaKeyword(";"));
				tokens.add(i + 6, variable);
				tokens.add(i + 7, new JavaKeyword("<"));
				tokens.remove(i + 8);
				tokens.add(blockStart + 4, new JavaKeyword(";"));
				tokens.add(blockStart + 5, variable);
				tokens.add(blockStart + 6, new JavaKeyword("++"));
				tokens.add(blockStart + 7, new JavaParenthesisEnd());
				tokens.remove(blockStart + 8);
				tokens.remove(blockStart + 9);
				tokens.remove(blockStart + 9);
				tokens.remove(blockStart + 9);
				int blockEnd = body.findEndOfBlock(blockStart + 8);
				tokens.remove(blockEnd + 1);
				tokens.remove(blockEnd + 1);
			}
		}
	}

	protected void trimUses(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 3; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaIdentifier) && token.value.equals("USES") && (tokens.elementAt(i - 1) instanceof JavaBlockEnd)) {
				if (i + 1 < tokens.size() && (tokens.elementAt(i + 1) instanceof JavaStatementTerminator)) {
					tokens.remove(i + 1);
				}
				tokens.remove(i);
				tokens.remove(i - 1);
				int j = i - 2;
				while (!(tokens.elementAt(j) instanceof JavaBlockStart)) {
					tokens.remove(j);
					j--;
				}
				tokens.remove(j);
			}
		}
	}

	protected void trimSelfSends(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = tokens.size() - 2; i >= 0; i--) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaIdentifier) && token.value.equals("this") && (tokens.elementAt(i + 1) instanceof JavaCallStart)) {
				tokens.remove(i);
			}
		}
	}

	protected void ensureReasonableStatementTerminators(MethodBody body) {
		Vector tokens = body.tokens;
		//TODO we are ignoring comments?
		if (tokens.isEmpty()) {
			return;
		}
		if (!(tokens.lastElement() instanceof JavaStatementTerminator) || !(tokens.lastElement() instanceof JavaBlockEnd)) {
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
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaKeyword) && token.value.equals("if") && !(tokens.elementAt(i + 1) instanceof JavaParenthesisStart)) {
				int earlyParentheses = 0;
				int earlyBlocks = 0;
				boolean found = false;
				for (int j = i; j < tokens.size(); j++) {
					JavaToken test = (JavaToken) tokens.elementAt(j);
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
