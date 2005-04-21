package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;

public class TransformUnreachableCode extends AbstractMethodBodyTransformation {
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.any(
				factory.token(JavaKeyword.class, "throw"),
				factory.token(JavaKeyword.class, "return"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		
		int end = javaMethod.methodBody.findNextTokenOfTypeQuietFail(i, JavaStatementTerminator.class);
		if (end == -1) {
			return i;
		}
		int nextBlockStart = javaMethod.methodBody.findNextTokenOfTypeQuietFail(i, JavaBlockStart.class);
		if (nextBlockStart != -1 && nextBlockStart < end) {
			end = javaMethod.methodBody.findEndOfBlock(nextBlockStart);
		}
		int endOfBlock;
		if (javaMethod.methodBody.findPreviousTokenOfTypeQuietFail(i, JavaBlockStart.class) == -1) {
			// top level - so can remove the rest of the method
			endOfBlock = tokens.size();
		} else {
			endOfBlock = javaMethod.methodBody.findNextTokenOfTypeQuietFail(end+1, JavaBlockEnd.class);
			if (endOfBlock == -1) {
				endOfBlock = tokens.size();
			}
		}
		for (int j = endOfBlock - 1; j > end; --j) {
			tokens.remove(j);
		}
		return i;
	}

}