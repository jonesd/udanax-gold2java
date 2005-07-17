package org.abora.ug2java.writer;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaCodebase;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.javatoken.IntegerLiteral;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;


public class ClassWriterTest extends TestCase {

	public ClassWriterTest(String name) {
		super(name);
	}
	
	public void testClassComment() {
		JavaCodebase javaCodebase = new JavaCodebase();
		javaCodebase.packageLookup.put("Heaper", "org.abora.gold.xpp.basic");
		JavaClass javaClass = new JavaClass("Test", "Heaper", javaCodebase);
		ClassWriter classWriter = new ClassWriter(javaClass, "File\ncomment");
		classWriter.quoteSmalltalk = false;
		classWriter.shouldIndent = false;

		javaClass.setComment("Hello\nthere and here");
		javaClass.classCategory = "org.abora";
		
		String actual = writeClass(classWriter);
		String expected = "/*\n * File\n * comment\n */\n\npackage org.abora;\n\n/**\n * Hello\n * there and here\n */\npublic class Test extends Heaper {\n\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testImports() {
		JavaCodebase javaCodebase = new JavaCodebase();
		javaCodebase.packageLookup.put("Heaper", "org.abora.gold.xpp.basic");
		JavaClass javaClass = new JavaClass("Test", "Heaper", javaCodebase);
		ClassWriter classWriter = new ClassWriter(javaClass, null);
		classWriter.quoteSmalltalk = false;
		classWriter.shouldIndent = false;

		javaClass.classCategory = "org.abora";
		
		//Should not need reference to self
		javaClass.includeImportForType(javaClass.className);
		
		//imports should be sorted by package/class
		javaCodebase.packageLookup.put("Test1", "org.abora.gold.xpp.basic");
		javaClass.includeImportForType("Test1");
		javaClass.includeImportForType("Heaper");
		javaCodebase.packageLookup.put("AbC", "org.abora.extra");
		javaClass.includeImportForType("AbC");
		javaCodebase.packageLookup.put("SamePackage", javaClass.classCategory);
		javaClass.includeImportForType("SamePackage");

		// Duplicates should be ignored
		javaClass.includeImportForType("SamePackage");

		String actual = writeClass(classWriter);
		String expected = "package org.abora;\n\nimport org.abora.SamePackage;\nimport org.abora.extra.AbC;\nimport org.abora.gold.xpp.basic.Heaper;\nimport org.abora.gold.xpp.basic.Test1;\n\npublic class Test extends Heaper {\n\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testStaticBlocks() {
		JavaCodebase javaCodebase = new JavaCodebase();
		javaCodebase.packageLookup.put("Heaper", "org.abora.gold.xpp.basic");
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
