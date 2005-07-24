package org.abora.ug2java.transform.type;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaCodebase;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.WriteMethodTestCase;
import org.abora.ug2java.writer.ClassWriter;


public class AddMethodTest extends WriteMethodTestCase {

	public AddMethodTest(String name) {
		super(name);
	}
	
	public void testAddIntegerTableMakeInt() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addIntegerTableMakeInt(javaClass);
		
		String actual = writeMethod(method);
		String expected = "public static IntegerTable make(int i) {\nreturn makeIntegerVar(i);\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testAddImmuSetMakeMuSet() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addImmuSetMakeMuSet(javaClass);
		
		String actual = writeMethod(method);
		String expected = "public static ImmuSet make(MuSet i) {\nreturn ImmuSet.makeMuSet(i);\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testAddMuSetMakeIntegerVar() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addMuSetMakeIntegerVar(javaClass);
		
		String actual = writeMethod(method);
		String expected = "public static MuSet make(int i) {\nreturn makeIntegerVar(i);\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testAddUnsupportedMethodEmptyArgs() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addUnsupportedMethod(javaClass, "", "String", "test", new String[] {});
		
		String actual = writeMethod(method);
		String expected = "public String test() {\nthrow new UnsupportedOperationException();\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testAddUnsupportedMethodStatic() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addUnsupportedMethod(javaClass, "static ", "void", "test", new String[] {"int", "i", "String", "s"});
		
		String actual = writeMethod(method);
		String expected = "public static void test(int i, String s) {\nthrow new UnsupportedOperationException();\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testAddFeWorkSet() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addFeWorkSet(javaClass);
		
		String actual = writeMethod(method);
		String expected = "public FeWorkSet(FeEdition edition, FeWrapperSpec spec) {\nsuper(edition, spec);\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testAddHashSetTesterIntroduceTestsOn() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addHashSetTesterIntroduceTestsOn(javaClass);
		
		String actual = writeMethod(method);
		String expected = "public void introduceTestsOn(PrintWriter oo, MuSet set, SHTO shto) {\nintroduceTestsOn(oo, (HashSet) set, shto);\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testAddHashSetTesterStoreTestsOn() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addHashSetTesterStoreTestsOn(javaClass);
		
		String actual = writeMethod(method);
		String expected = "public void storeTestsOn(PrintWriter oo, MuSet set, SHTO shto) {\nstoreTestsOn(oo, (HashSet) set, shto);\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testAddDefineGlobal() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addDefineGlobal(javaClass);
		
		String actual = writeMethod(method);
		String expected = "public static void defineGlobal(String globalName, Heaper initialValue) {\nAboraSupport.defineGlobal(globalName, initialValue);\n}\n";
		assertBodyEquals(expected, actual);
	}

	public void testHeaperEquals() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addHeaperEquals(javaClass);
		
		String actual = writeMethod(method);
		String expected = "public boolean equals(Object o) {\nif (o instanceof Heaper) {\nreturn equals((Heaper) o);\n}\nelse {\nreturn false;\n}\n}\n";
		assertBodyEquals(expected, actual);
	}
}
