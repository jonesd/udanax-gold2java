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

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaAssignment;
import info.dgjones.abora.ug2java.javatoken.JavaComment;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaType;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



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
