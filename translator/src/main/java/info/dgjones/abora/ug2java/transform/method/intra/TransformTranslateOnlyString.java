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
import info.dgjones.abora.ug2java.javatoken.JavaBlockEnd;
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaComment;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.StringLiteral;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformTranslateOnlyString extends AbstractMethodBodyTransformation {

	public TransformTranslateOnlyString() {
		super();
	}
	public TransformTranslateOnlyString(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(StringLiteral.class), 
				factory.token(JavaCallStart.class, "translateOnly"), 
				factory.token(JavaCallEnd.class));
	}
	
	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		StringLiteral javaLiteral = (StringLiteral)tokens.get(i);
		//TODO general insanity here. Why didn't we just remember the original value?
		String cleanSource = stipStringWrapping(javaLiteral.value);
		
		tokens.remove(i+2);
		tokens.remove(i+1);
		tokens.remove(i);

		tokens.add(i, new JavaBlockStart());
		tokens.add(i+1, new JavaComment(cleanSource));
		tokens.add(i+2, new JavaBlockEnd());
		tokens.add(i+3, new JavaIdentifier("translateOnly"));
		
		return i;
	}
	
	private String stipStringWrapping(String value) {
		String cleaner = value.replaceAll("\\\\n\"\\+\n\"", "\n");
//		cleaner = cleaner.replaceAll("\n\"", "\n");
		return cleaner.substring(1, cleaner.length()-1);
	}
}
