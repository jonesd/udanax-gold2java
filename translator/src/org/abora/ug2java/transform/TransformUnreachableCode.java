package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.tokenmatcher.MatchSequence;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;

public class TransformUnreachableCode extends AbstractMethodBodyTransformation {

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		MatchSequence matchSequence = new MatchSequence();
		matchSequence.add(factory.token(JavaKeyword.class, "throw"));
		matchSequence.add(factory.token(JavaKeyword.class, "new"));
		matchSequence.add(factory.token(JavaCallStart.class));
		matchSequence.add(factory.token(JavaIdentifier.class));
		matchSequence.add(factory.token(JavaCallEnd.class));
		matchSequence.add(factory.token(JavaStatementTerminator.class));
		matchSequence.add(factory.token(JavaKeyword.class, "return"));
		matchSequence.add(factory.token(JavaIdentifier.class, "null"));
		return matchSequence;
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		if (tokens.get(i + 8) instanceof JavaComment) {
			tokens.remove(i + 8);
		}
		if (!(tokens.get(i + 8) instanceof JavaStatementTerminator)) {
			throw new IllegalStateException("Expected a ; here");
		}
		tokens.remove(i + 8);
		tokens.remove(i + 7);
		tokens.remove(i + 6);
	}

}