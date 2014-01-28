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
package info.dgjones.abora.ug2java.transform.tokenmatcher;

import java.util.List;
import java.util.regex.Pattern;

import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.util.ToStringGenerator;



public class MatchToken implements TokenMatcher {

	private final Class tokenClass;
	private final Pattern tokenValue;
	
	
	public MatchToken(Class tokenClass) {
		this(tokenClass, null);
	}
	
	public MatchToken(Class tokenClass, String tokenValue) {
		this.tokenClass = tokenClass;
		if (tokenValue != null) {
			this.tokenValue = Pattern.compile(tokenValue);
		} else {
			this.tokenValue = null;
		}
	}
	
	public boolean doesMatch(List tokens, int i) {
		JavaToken javaToken = (JavaToken)tokens.get(i);
		return (tokenClass.isAssignableFrom(javaToken.getClass())) && (tokenValue == null || tokenValue.matcher(javaToken.value).matches());
	}

	public String toString() {
		ToStringGenerator generator = new ToStringGenerator(this);
		generator.add("class", tokenClass);
		generator.add("value", tokenValue);
		return generator.end();
	}
}
