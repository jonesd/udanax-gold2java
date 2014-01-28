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

	public void testCategoryPrintOn() {
		AddMethod transformation = new AddMethod();
		JavaMethod method = transformation.addCategoryPrintOn(javaClass);
		
		String actual = writeMethod(method);
		String expected = "public void printOn(PrintWriter oo) {\noo.print(getAboraClass().name());\noo.print(\"(\");\noo.print(name());\noo.print(\")\");\n}\n";
		assertBodyEquals(expected, actual);
	}
}
