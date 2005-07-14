package org.abora.ug2java;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.abora.ug2java.writer.ClassWriter;

import junit.framework.TestCase;


public abstract class WriteMethodTestCase extends TestCase {

	public WriteMethodTestCase(String name) {
		super(name);
	}

	protected JavaClass javaClass;
	protected ClassWriter classWriter;

	public void setUp() {
		JavaCodebase javaCodebase = new JavaCodebase();
		javaCodebase.packageLookup.put("Heaper", "org.abora.gold.xpp.basic");
		javaClass = new JavaClass("Test", "Heaper", javaCodebase);
		classWriter = new ClassWriter(javaClass);
		classWriter.quoteSmalltalk = false;
		classWriter.shouldIndent = false;
	}
	
	protected String writeMethod(JavaMethod javaMethod) {
		StringWriter stringWriter = new StringWriter();
		PrintWriter printWriter = new PrintWriter(stringWriter);
		classWriter.writeMethod(javaMethod, printWriter);
		printWriter.close();
		return stringWriter.toString();
	}

	protected String writeClass() {
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
