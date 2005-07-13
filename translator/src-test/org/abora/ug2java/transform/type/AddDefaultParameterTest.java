package org.abora.ug2java.transform.type;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaCodebase;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.writer.ClassWriter;

import junit.framework.TestCase;


public class AddDefaultParameterTest extends TestCase {

	private JavaClass javaClass;
	private ClassWriter classWriter;

	public void setUp() {
		JavaCodebase javaCodebase = new JavaCodebase();
		javaCodebase.packageLookup.put("Heaper", "org.abora.gold.xpp.basic");
		javaClass = new JavaClass("Test", javaCodebase);
		classWriter = new ClassWriter(javaClass);
		classWriter.quoteSmalltalk = false;
		classWriter.shouldIndent = false;
	}
	
	private String writeMethod(JavaMethod javaMethod) {
		StringWriter stringWriter = new StringWriter();
		PrintWriter printWriter = new PrintWriter(stringWriter);
		classWriter.writeMethod(javaMethod, printWriter);
		printWriter.close();
		return stringWriter.toString();
	}

	private void assertMethodBodyEquals(String expectedJava, String actualJava) {
		actualJava = actualJava.replaceAll(System.getProperty("line.separator"), "\n");
		assertEquals(expectedJava, actualJava);
	}

	public void testInstance() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "String", "method", new String[0], "false");
		
		String actual = writeMethod(method);
		String expected = "public String method() {\nreturn method( false );\n}\n";
		assertMethodBodyEquals(expected, actual);
	}

	public void testInstanceInteger0() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "String", "method", new String[0], "0");
		
		String actual = writeMethod(method);
		String expected = "public String method() {\nreturn method(0);\n}\n";
		assertMethodBodyEquals(expected, actual);
	}

	public void testInstanceTrue() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "String", "method", new String[0], "true");
		
		String actual = writeMethod(method);
		String expected = "public String method() {\nreturn method( true );\n}\n";
		assertMethodBodyEquals(expected, actual);
	}

	public void testInstanceNull() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "String", "method", new String[0], "null");
		
		String actual = writeMethod(method);
		String expected = "public String method() {\nreturn method( null );\n}\n";
		assertMethodBodyEquals(expected, actual);
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
		assertMethodBodyEquals(expected, actual);
	}

	public void testInstannceVoidReturn() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addInstance(javaClass, "void", "method", new String[0], "0");
		
		String actual = writeMethod(method);
		String expected = "public void method() {\nmethod(0);\n}\n";
		assertMethodBodyEquals(expected, actual);
	}

	public void testStaticInteger0() {
		AddDefaultParameter transformation = new AddDefaultParameter();
		JavaMethod method = transformation.addStatic(javaClass, "String", "method", new String[0], "0");
		
		String actual = writeMethod(method);
		String expected = "public static String method() {\nreturn method(0);\n}\n";
		assertMethodBodyEquals(expected, actual);
	}

}
