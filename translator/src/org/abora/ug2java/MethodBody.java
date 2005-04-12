/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */

package org.abora.ug2java;

import java.util.Enumeration;
import java.util.List;
import java.util.Vector;

import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;

public class MethodBody {
	public List tokens;

	public MethodBody(List tokens) {
		super();
		this.tokens = tokens;
	}
	
	public JavaToken get(int index) {
		return (JavaToken)tokens.get(index);
	}
	
	public void remove(int index) {
		tokens.remove(index);
	}
	
	public void removeShouldMatch(int index, Class aClass) {
		removeShouldMatch(index, aClass, null);
	}
	
	public void removeShouldMatch(int index, String value) {
		removeShouldMatch(index, null, value);
	}

	public void removeShouldMatch(int index, Class aClass, String value) {
		shouldMatch(index, aClass, value);
		remove(index);
	}

	public void shouldMatch(int index, Class aClass) {
		shouldMatch(index, aClass, null);
	}
	
	public void shouldMatch(int index, String value) {
		shouldMatch(index, null, value);
	}
	public void shouldMatch(int index, Class aClass, String value) {
		JavaToken tokenToRemove = (JavaToken)tokens.get(index);
		if (aClass != null && !(aClass.isInstance(tokenToRemove))) {
			throw new IllegalStateException("Removing index:"+index+" expected class:"+aClass+" but found:"+tokenToRemove);
		}
		if (value != null && !value.equals(tokenToRemove.value)) {
			throw new IllegalStateException("Removing index:"+index+" expected value:"+value+" but found:"+tokenToRemove);
		}
	}

	public int findClosingCallEnd(int callStart) {
		int earlyCalls = 0;
		for (int i = callStart + 1; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.get(i);
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

	public JavaToken findExistingJavaCallKeyword(Vector expression) {
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

	public int findEndOfBlockQuietFail(int blockStart) {
		int earlyParentheses = 0;
		int earlyBlocks = 0;
		for (int i = blockStart + 1; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.get(i);
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
		return -1;
	}

	public int findEndOfBlock(int blockStart) {
		int end = findEndOfBlockQuietFail(blockStart);
		if (end == -1) {
			throw new IllegalStateException("Could not find closing block");
		} else {
			return end;
		}
	}

	public int findStartOfBlock(int blockEnd) {
		int earlyBlocks = 0;
		for (int i = blockEnd - 1; i >= 0; i--) {
			JavaToken token = (JavaToken) tokens.get(i);
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

	public int findStartOfExpression(int endIndex) {
		int laterParentheses = 0;
		int laterBlocks = 0;
		for (int i = endIndex; i >= 0; i--) {
			JavaToken token = (JavaToken) tokens.get(i);
			if (token instanceof JavaParenthesisStart || token instanceof JavaCallStart) {
				laterParentheses--;
			} else if (token instanceof JavaParenthesisEnd || token instanceof JavaCallEnd) {
				laterParentheses++;
			} else if (token instanceof JavaBlockStart) {
				laterBlocks--;
			} else if (token instanceof JavaBlockEnd) {
				laterBlocks--;
//				laterBlocks++;
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

	public int findStartOfExpressionMinimal(int endIndex) {
		int laterParentheses = 0;
		int laterBlocks = 0;
		for (int i = endIndex; i >= 0; i--) {
			JavaToken token = (JavaToken) tokens.get(i);
			if (token instanceof JavaParenthesisStart || token instanceof JavaCallStart) {
				laterParentheses--;
			} else if (token instanceof JavaParenthesisEnd || token instanceof JavaCallEnd) {
				laterParentheses++;
			} else if (token instanceof JavaBlockStart) {
				laterBlocks--;
			} else if (token instanceof JavaBlockEnd) {
				laterBlocks--;
//				laterBlocks++;
			} else if (
				(token instanceof JavaAssignment)
					|| (token instanceof JavaStatementTerminator)
					|| ((token instanceof JavaKeyword) && token.value.equals("return"))
					|| (token instanceof JavaKeyword)
					|| (token instanceof JavaCallArgumentSeparator)) {
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

	public int findNextTokenOfTypeQuietFail(int startIndex, Class aClass) {
		for (int i = startIndex; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.get(i);
			if (aClass.isAssignableFrom(token.getClass())) {
				return i;
			}
		}
		return -1;
	}

	public int findNextTokenOfType(int startIndex, Class aClass) {
		int i = findNextTokenOfTypeQuietFail(startIndex, aClass);
		if (i == -1) {
			throw new IllegalStateException("Could not find any more " + aClass);
		} else {
			return i;
		}
	}

	public int findPreviousTokenOfTypeQuietFail(int startIndex, Class aClass) {
		for (int i = startIndex; i >= 0; --i) {
			JavaToken token = (JavaToken) tokens.get(i);
			if (aClass.isAssignableFrom(token.getClass())) {
				return i;
			}
		}
		return -1;
	}

	public int findPreviousTokenOfType(int startIndex, Class aClass) {
		int i = findPreviousTokenOfTypeQuietFail(startIndex, aClass);
		if (i == -1) {
			throw new IllegalStateException("Could not find earlier  " + aClass);
		} else {
			return i;
		}
	}

	public int findEndOfExpression(int startIndex) {
		int laterParentheses = 0;
		int laterBlocks = 0;
		for (int i = startIndex; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.get(i);
			if (token instanceof JavaParenthesisStart) {
				laterParentheses++;
			} else if (token instanceof JavaParenthesisEnd) {
				laterParentheses--;
			} else if (token instanceof JavaBlockStart) {
				laterBlocks++;
			} else if (token instanceof JavaBlockEnd) {
				laterBlocks--;
			} else if (token instanceof JavaStatementTerminator) {
				if (laterParentheses == 0 && laterBlocks == 0) {
					return i - 1;
				}
			}
			if ((laterParentheses < 0 && laterBlocks == 0) || (laterParentheses == 0 && laterBlocks < 0)) {
				return i - 1;
			}
			if (laterParentheses == 0 && laterBlocks == 0 && i == tokens.size() - 1) {
				return tokens.size() - 1;
			}
		}
		throw new IllegalStateException("Could not find end of expression");
	}
}
