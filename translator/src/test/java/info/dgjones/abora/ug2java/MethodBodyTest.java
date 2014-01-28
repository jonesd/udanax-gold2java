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
package info.dgjones.abora.ug2java;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaComment;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.IntegerLiteral;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;



public class MethodBodyTest extends TestCase {

	MethodBody body;
	List tokens;
	
	protected void setUp() throws Exception {
		super.setUp();
		
		tokens = new ArrayList();
		body = new MethodBody(tokens);
	}
	
	public void testFindNumberOfCallArgsNotStartingOnACall() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaKeyword("+"));
		tokens.add(new JavaIdentifier("blah"));
		
		try {
			body.findNumberOfCallArgs(0);
		} catch (IllegalStateException e) {
			assertTrue(e.toString(), e.getMessage().indexOf("0") != -1);
		}
	}

	public void testFindNumberOfCallArgsWithNoArgs() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaCallStart("call"));
		tokens.add(new JavaCallEnd());
		
		assertEquals(0, body.findNumberOfCallArgs(1));
	}

	public void testFindNumberOfCallArgsWithNoArgsButAComment() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaCallStart("call"));
		tokens.add(new JavaComment("some kind of comment"));
		tokens.add(new JavaCallEnd());
		
		assertEquals(0, body.findNumberOfCallArgs(1));
	}

	public void testFindNumberOfCallArgsWith1Arg() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaCallKeywordStart("call"));
		tokens.add(new IntegerLiteral(189));
		tokens.add(new JavaCallEnd());
		
		assertEquals(1, body.findNumberOfCallArgs(1));
	}

	public void testFindNumberOfCallArgsWith2Args() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaCallKeywordStart("call"));
		tokens.add(new IntegerLiteral(189));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaIdentifier("blah"));
		tokens.add(new JavaCallEnd());
		
		assertEquals(2, body.findNumberOfCallArgs(1));
	}

	public void testFindNumberOfCallArgsWithEmbeddedCalls() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaCallKeywordStart("call"));
		
		// Embedded call w/2 args
		tokens.add(new JavaCallKeywordStart("innerCall"));
		tokens.add(new JavaIdentifier("blah"));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaIdentifier("blah2"));
		tokens.add(new JavaCallEnd());
		
		tokens.add(new JavaCallEnd());
		
		assertEquals(1, body.findNumberOfCallArgs(1));
	}
}
