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
package info.dgjones.abora.ug2java.transform.type;

import java.io.PrintWriter;
import java.io.StringWriter;

import info.dgjones.abora.ug2java.JavaClass;
import info.dgjones.abora.ug2java.JavaCodebase;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.WriteMethodTestCase;
import info.dgjones.abora.ug2java.writer.ClassWriter;


public class AddDefaultParameterTest extends WriteMethodTestCase {

	
	public AddDefaultParameterTest(String name) {
		super(name);
	}

	public void testInstance() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "String", "method", new String[0], "false");
		
		String actual = writeMethod(method);
		String expected = "public String method() {\nreturn method( false );\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testInstanceInteger0() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "String", "method", new String[0], "0");
		
		String actual = writeMethod(method);
		String expected = "public String method() {\nreturn method(0);\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testInstanceTrue() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "String", "method", new String[0], "true");
		
		String actual = writeMethod(method);
		String expected = "public String method() {\nreturn method( true );\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testInstanceNull() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "String", "method", new String[0], "null");
		
		String actual = writeMethod(method);
		String expected = "public String method() {\nreturn method( null );\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testInstanceNonLiteral() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		try {
			transformation.addInstance(javaClass, "String", "method", new String[0], "anotherVariable");
			fail("expected exception");
		} catch (IllegalArgumentException e) {
			assertTrue(e.getMessage(), e.getMessage().startsWith("Cant interpret additional param:"));
		}
	}

	public void testInstanceWithArgs() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "String", "method", new String[] {"int", "var1", "String", "var2"}, "0");
		
		String actual = writeMethod(method);
		String expected = "public String method(int var1, String var2) {\nreturn method(var1, var2, 0);\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testInstannceVoidReturn() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "void", "method", new String[0], "0");
		
		String actual = writeMethod(method);
		String expected = "public void method() {\nmethod(0);\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testStaticInteger0() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addStatic(javaClass, "String", "method", new String[0], "0");
		
		String actual = writeMethod(method);
		String expected = "public static String method() {\nreturn method(0);\n}\n";
		assertBodyEquals(expected, actual);
	}

}
