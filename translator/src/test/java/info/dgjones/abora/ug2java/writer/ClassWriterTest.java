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
package info.dgjones.abora.ug2java.writer;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import info.dgjones.abora.ug2java.JavaClass;
import info.dgjones.abora.ug2java.JavaCodebase;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.MethodBody;
import info.dgjones.abora.ug2java.javatoken.IntegerLiteral;
import info.dgjones.abora.ug2java.javatoken.JavaAssignment;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;


public class ClassWriterTest extends TestCase {

	public ClassWriterTest(String name) {
		super(name);
	}
	
	public void testClassComment() {
		JavaCodebase javaCodebase = new JavaCodebase();
		javaCodebase.packageLookup.put("Heaper", "info.dgjones.abora.gold.xpp.basic");
		JavaClass javaClass = new JavaClass("Test", "Heaper", javaCodebase);
		ClassWriter classWriter = new ClassWriter(javaClass, "File\ncomment");
		classWriter.quoteSmalltalk = false;
		classWriter.shouldIndent = false;

		javaClass.setComment("Hello\nthere and here");
		javaClass.classCategory = "info.dgjones.abora";
		
		String actual = writeClass(classWriter);
		String expected = "/*\n * File\n * comment\n */\n\npackage info.dgjones.abora;\n\n/**\n * Hello\n * there and here\n */\npublic class Test extends Heaper {\n\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testImports() {
		JavaCodebase javaCodebase = new JavaCodebase();
		javaCodebase.packageLookup.put("Heaper", "info.dgjones.abora.gold.xpp.basic");
		JavaClass javaClass = new JavaClass("Test", "Heaper", javaCodebase);
		ClassWriter classWriter = new ClassWriter(javaClass, null);
		classWriter.quoteSmalltalk = false;
		classWriter.shouldIndent = false;

		javaClass.classCategory = "info.dgjones.abora";
		
		//Should not need reference to self
		javaClass.includeImportForType(javaClass.className);
		
		//imports should be sorted by package/class
		javaCodebase.packageLookup.put("Test1", "info.dgjones.abora.gold.xpp.basic");
		javaClass.includeImportForType("Test1");
		javaClass.includeImportForType("Heaper");
		javaCodebase.packageLookup.put("AbC", "info.dgjones.abora.extra");
		javaClass.includeImportForType("AbC");
		javaCodebase.packageLookup.put("SamePackage", javaClass.classCategory);
		javaClass.includeImportForType("SamePackage");

		// Duplicates should be ignored
		javaClass.includeImportForType("SamePackage");

		String actual = writeClass(classWriter);
		String expected = "package info.dgjones.abora;\n\nimport info.dgjones.abora.SamePackage;\nimport info.dgjones.abora.extra.AbC;\nimport info.dgjones.abora.gold.xpp.basic.Heaper;\nimport info.dgjones.abora.gold.xpp.basic.Test1;\n\npublic class Test extends Heaper {\n\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testStaticBlocks() {
		JavaCodebase javaCodebase = new JavaCodebase();
		javaCodebase.packageLookup.put("Heaper", "info.dgjones.abora.gold.xpp.basic");
		JavaClass javaClass = new JavaClass("Test", "Heaper", javaCodebase);
		ClassWriter classWriter = new ClassWriter(javaClass, null);
		classWriter.quoteSmalltalk = false;
		classWriter.shouldIndent = true;

		javaClass.classCategory = "org.abora";

		JavaMethod static1 = new JavaMethod("void", "static1");
		List static1tokens = new ArrayList();
		static1tokens.add(new JavaIdentifier("var1"));
		static1tokens.add(new JavaAssignment());
		static1tokens.add(new IntegerLiteral(123));
		static1tokens.add(new JavaStatementTerminator());
		static1tokens.add(new JavaIdentifier("var2"));
		static1tokens.add(new JavaAssignment());
		static1tokens.add(new IntegerLiteral(456));
		static1tokens.add(new JavaStatementTerminator());
		static1.methodBody = new MethodBody(static1tokens);
		javaClass.addStaticBlock(static1);

		JavaMethod static2 = new JavaMethod("void", "static2");
		List static2tokens = new ArrayList();
		static2tokens.add(new JavaCallStart("runInitializers"));
		static2tokens.add(new JavaCallEnd());
		static2tokens.add(new JavaStatementTerminator());
		static2.methodBody = new MethodBody(static2tokens);
		javaClass.addStaticBlockFirst(static2);

		String actual = writeClass(classWriter);
		String expected = "package org.abora;\n\npublic class Test extends Heaper {\n\nstatic {\n\trunInitializers();\n}\n\nstatic {\n\tvar1 = 123;\n\tvar2 = 456;\n}\n\n}\n";
		assertBodyEquals(expected, actual);
	}

	protected String writeClass(ClassWriter classWriter) {
		StringWriter stringWriter = new StringWriter();
		PrintWriter printWriter = new PrintWriter(stringWriter);
		classWriter.write(printWriter);
		printWriter.close();
		return stringWriter.toString();
	}
	protected void assertBodyEquals(String expectedJava, String actualJava) {
		actualJava = actualJava.replaceAll(System.getProperty("line.separator"), "\n");
		assertEquals(expectedJava, actualJava);
	}

}
