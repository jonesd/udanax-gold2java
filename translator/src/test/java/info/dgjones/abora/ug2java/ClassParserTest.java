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

import info.dgjones.abora.ug2java.stscanner.ChunkDetails;


public class ClassParserTest extends WriteMethodTestCase {

	public ClassParserTest(String name) {
		super(name);
	}

	public void testParseGetOrMakeCxxClassDescriptionChunk() {
		
		ChunkDetails chunk = new ChunkDetails("context", "(Test getOrMakeCxxClassDescription)\n" + 
				"	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!\n" + 
				"");
		javaClass.classQuotes.add(chunk);
		
		ClassParser parser = new ClassParser();
		parser.setJavaClass(javaClass);
		
		parser.parseGetOrMakeCxxClassDescriptionChunk(chunk);
		
		JavaMethod method = javaClass.getMethod("initializeClassAttributes");
		assertNotNull("generated initializeClassAttributes", method);
		parser.transformMethod(method);

		String methodText = writeMethod(method);
		assertBodyEquals("public static void initializeClassAttributes() {\nAboraSupport.findAboraClass(Test.class).setAttributes( new Set().add(\"CONCRETE\").add(\"NOTATYPE\"));\n}\n", methodText);
	}
	
	public void testTransformSmalltalkSymbolToJava() {
		assertEquals("SIMPLE", ClassParser.transformSmalltalkSymbolToJava("simple"));
		assertEquals("MORE_COMPLICATED", ClassParser.transformSmalltalkSymbolToJava("moreComplicated"));
		//TODO test :
		
	}
}
