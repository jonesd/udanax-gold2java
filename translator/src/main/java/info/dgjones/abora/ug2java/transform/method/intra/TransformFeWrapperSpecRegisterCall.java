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

import java.util.List;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * TODO this class only exists because of limitations inferring type for downcasting
 * arguments within a call
 */
public class TransformFeWrapperSpecRegisterCall extends AbstractMethodBodyTransformation {

	public TransformFeWrapperSpecRegisterCall() {
		super();
	}
	public TransformFeWrapperSpecRegisterCall(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "FeWrapperSpec"),
				factory.token(JavaCallKeywordStart.class, "register(Abstract|Direct|Indirect)"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallKeywordStart call = (JavaCallKeywordStart)tokens.get(i+1);
		String[] casts;
		if (call.value.equals("registerAbstract")) {
			casts = new String[] {"", "", "FeWrapperSpecHolder"};
		} else if (call.value.equals("registerDirect")) {
			casts = new String[] {"", "", "FeDirectWrapperMaker", "FeDirectWrapperChecker", "FeWrapperSpecHolder"};
		} else {
			casts = new String[] {"", "", "", "FeIndirectWrapperMaker", "FeIndirectWrapperChecker", "FeWrapperSpecHolder"};
		}
		
		
		List argStarts = javaMethod.methodBody.extractCallArgStarts(i+1);
		if (argStarts.size() != casts.length) {
			System.out.println("--Warning: Mismatch of FeWrapperSpec register casts: "+casts+" but found: "+argStarts);
			return i;
		}
		
		for (int j = casts.length-1; j >= 0; j--) {
			String cast = casts[j];
			if (!cast.equals("")) {
				int insert = ((Integer)argStarts.get(j)).intValue();
				tokens.add(insert, new JavaCast(cast));
			}
		}
		
		return i;
	}
}
