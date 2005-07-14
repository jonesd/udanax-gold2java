package org.abora.ug2java.writer;

import java.io.PrintWriter;
import java.io.StringWriter;

import junit.framework.TestCase;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaCodebase;


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
		String expected = "/*\n * File\n * comment\n */\npackage org.abora;\n\n\n\n/**\n * Hello\n * there and here\n */\npublic class Test extends Heaper {\n}\n";
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
