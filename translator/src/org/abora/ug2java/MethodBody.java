/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ug2java;

import java.util.Enumeration;
import java.util.Vector;

public class MethodBody {
	public Vector tokens;

	public MethodBody(Vector tokens) {
		super();
		this.tokens = tokens;
	}

	protected int findClosingCallEnd(int callStart) {
		int earlyCalls = 0;
		for (int i = callStart + 1; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if (token instanceof JavaCallStart) {
				earlyCalls++;
			} else if (token instanceof JavaCallEnd) {
				earlyCalls--;
				if (earlyCalls < 0) {
					return i;
				}
			}
		}
		throw new IllegalStateException("Could not find closing callend");
	}

	protected JavaToken findExistingJavaCallKeyword(Vector expression) {
		JavaToken existingKeyword = null;
		for (Enumeration e = expression.elements(); e.hasMoreElements();) {
			JavaToken token = (JavaToken) e.nextElement();
			if (token instanceof JavaCallKeywordStart) {
				existingKeyword = token;
				break;
			}
		}
		return existingKeyword;
	}

	protected int findEndOfBlock(int blockStart) {
		int earlyParentheses = 0;
		int earlyBlocks = 0;
		for (int i = blockStart + 1; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if (token instanceof JavaParenthesisStart) {
				earlyParentheses++;
			} else if (token instanceof JavaParenthesisEnd) {
				earlyParentheses--;
			} else if (token instanceof JavaBlockStart) {
				earlyBlocks++;
			} else if (token instanceof JavaBlockEnd) {
				if (earlyParentheses > 0 || earlyBlocks > 0) {
					earlyBlocks--;
				} else {
					return i;
				}
			}
		}
		throw new IllegalStateException("Could not find closing block");
	}

	protected int findStartOfBlock(int blockEnd) {
		int earlyBlocks = 0;
		for (int i = blockEnd - 1; i >= 0; i--) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if (token instanceof JavaBlockEnd) {
				earlyBlocks++;
			} else if (token instanceof JavaBlockStart) {
				if (earlyBlocks > 0) {
					earlyBlocks--;
				} else {
					return i;
				}
			}
		}
		throw new IllegalStateException("Could not find starting block");
	}

	protected int findStartOfExpression(int endIndex) {
		int laterParentheses = 0;
		int laterBlocks = 0;
		for (int i = endIndex; i >= 0; i--) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if (token instanceof JavaParenthesisStart) {
				laterParentheses--;
			} else if (token instanceof JavaParenthesisEnd) {
				laterParentheses++;
			} else if (token instanceof JavaBlockStart) {
				laterBlocks--;
			} else if (token instanceof JavaBlockEnd) {
				laterBlocks++;
			} else if (
				(token instanceof JavaAssignment)
					|| (token instanceof JavaStatementTerminator)
					|| ((token instanceof JavaKeyword) && token.value.equals("return"))) {
				if (laterParentheses == 0 && laterBlocks == 0) {
					return i + 1;
				}
			}
			if ((laterParentheses < 0 && laterBlocks == 0) || (laterParentheses == 0 && laterBlocks < 0)) {
				return i + 1;
			}
			if (laterParentheses == 0 && laterBlocks == 0 && i == 0) {
				return 0;
			}
		}
		throw new IllegalStateException("Could not find start of expression");
	}

	protected int findNextTokenOfType(int startIndex, Class aClass) {
		for (int i = startIndex; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if (aClass.isAssignableFrom(token.getClass())) {
				return i;
			}
		}
		throw new IllegalStateException("Could not find any more " + aClass);
	}
}
