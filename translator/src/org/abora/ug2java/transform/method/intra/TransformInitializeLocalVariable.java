/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformInitializeLocalVariable extends AbstractMethodBodyTransformation {

	private static final Set variables;
	static {
		Set set = new HashSet();
		set.add("CanopyCrum.makeJoin.prev");
		set.add("ScruSet.printOnWithSyntax.dSet");
		set.add("IntegerRegion.withInt.me");
		set.add("IntegerRegion.intersects.pending");
		set.add("GenericCrossRegion.intersects.others");
		set.add("EndorsementsChange.fetchFinder.result");
		set.add("PermissionsChange.fetchFinder.result");
		set.add("PrimPtrTable.hashFind.loc");
		set.add("PrimPtrTable.hashFind.removed");
		set.add("RealRegion.make.flag");
		set.add("ActualHashSet.arrayStats.totCnt");
		
		//TODO just for tests
		set.add("Test.test.shouldInitialize");
		
		variables = Collections.unmodifiableSet(set);
	}
	
	public TransformInitializeLocalVariable() {
		super();
	}
	public TransformInitializeLocalVariable(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaType.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier tempName = (JavaIdentifier)tokens.get(i+1);
		String shortName = javaMethod.name+"."+tempName.value;
		String fullName = javaMethod.javaClass.className+"."+shortName;
		if (variables.contains(fullName) || variables.contains(shortName)) {
			tokens.add(i+2, new JavaAssignment());
			//TODO take into account of type
			String tempType = ((JavaType)tokens.get(i)).value;
			String initialValue = tempType.equals("int") ? "0" : "null";
			tokens.add(i+3, new JavaIdentifier(initialValue));
			tokens.add(i, new JavaComment("TODO variable may not be initialized before being used"));
		}
		return i;
	}
}
