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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.javatoken.JavaType;
import info.dgjones.abora.ug2java.transform.method.MethodTransformation;

/**
 * Sometimes the block level declaration of our variables differs enough from Smalltalk,
 * so that can end up with conflicting variable declarations of the same variable name.
 */
public class EnsureUniqueLocalVarNames implements MethodTransformation {

	public void transform(JavaMethod javaMethod) {
		Map nameOccurrence = new HashMap();
		//TODO populate nameOccurrences with all instance variables?
		//TODO handle declarations within blocks only lasting to the end of the block
		
		List tokens = javaMethod.methodBody.tokens;
		for (int i = 0; i < tokens.size(); ++i) {
			JavaToken token = (JavaToken)tokens.get(i);
			if (token instanceof JavaIdentifier && couldBeLocalVariableName(token.value)) {
				Integer occurrences = (Integer)nameOccurrence.get(token.value);
				if (isVariableDeclaration(tokens, i)) {
					if (occurrences == null) {
						occurrences = new Integer(0);
					} else {
						occurrences = new Integer(occurrences.intValue() + 1);
					}
					nameOccurrence.put(token.value, occurrences);
				}
				if (occurrences != null && occurrences.intValue() > 0) {
					token.value = token.value+occurrences;
				}
			}
		}
	}

	private boolean isVariableDeclaration(List tokens, int i) {
		return i > 0 && tokens.get(i-1) instanceof JavaType;
	}

	private boolean couldBeLocalVariableName(String name) {
		return Character.isLowerCase(name.charAt(0));
	}
	
	

}