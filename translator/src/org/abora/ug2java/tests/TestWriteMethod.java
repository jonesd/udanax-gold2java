/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.tests;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Hashtable;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.abora.ug2java.ClassParser;
import org.abora.ug2java.ClassWriter;
import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.stscanner.ChunkDetails;

/**
 * JUnit test case for TestWriteMethod
 */

public class TestWriteMethod extends TestCase {
	private static final Class THIS = TestWriteMethod.class;

	private JavaClass javaClass;
	private ClassWriter classWriter;

	public TestWriteMethod(String name) {
		super(name);
	}

	public void setUp() {
		Hashtable packageLookup = new Hashtable();
		packageLookup.put("Heaper", "org.abora.gold.xpp.basic");
		javaClass = new JavaClass(packageLookup);
		javaClass.className = "Test";
		classWriter = new ClassWriter(javaClass);
		classWriter.quoteSmalltalk = false;
		classWriter.shouldIndent = false;
	}

	public static Test suite() {
		return new TestSuite(THIS);
	}

	public void testAnd() {
		String smalltalk = "test\none and: [two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none && (two);\n}\n", java);
	}

	public void testAndAnd() {
		String smalltalk = "test\none and: [two and: [three]]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none && (two && (three));\n}\n", java);
	}

	public void testAlmostTo() {
		String smalltalk = "test\n0 almostTo: fred happy do: [:i {UInt32} | blah ]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfor (int i = 0 ; i < fred.happy() ; i ++ ) {\nblah;\n}\n}\n", java);
	}

	public void testAssign() {
		String smalltalk = "test\nfred _ 1.\nharry :=2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfred = 1;\nharry = 2;\n}\n", java);
	}

	public void testAssignUnaryOperator() {
		String smalltalk = "test\nfred := harry kill!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfred = harry.kill();\n}\n", java);
	}

	public void testAtStore() {
		String smalltalk = "test\na < b ifTrue: [table at: 1 store: NULL. a := a + 1]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (a < b) {\ntable.store(1, null);\na = a + 1;\n}\n}\n", java);
	}

	public void testAtStoreValue() {
		String smalltalk = "test\ntable at: 1 storeValue: value!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\ntable.storeValue(1, value);\n}\n", java);
	}

	public void testBinaryOperator() {
		String smalltalk = "test\none < two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none < two;\n}\n", java);
	}

	public void testBinaryOperatorEquals() {
		String smalltalk = "test\none = two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none == two;\n}\n", java);
	}

	public void testBinaryOperatorIdentity() {
		String smalltalk = "test\none == two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none == two;\n}\n", java);
	}

	public void testBinaryOperatorNotEquals() {
		String smalltalk = "test\none ~= two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none != two;\n}\n", java);
	}

	public void testBinaryOperatorNotIdentity() {
		String smalltalk = "test\none ~~ two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none != two;\n}\n", java);
	}

	public void testBinaryOperatorWithUnary() {
		String smalltalk = "test\none and < two three!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none.and() < two.three();\n}\n", java);
	}

	public void testBitAnd() {
		String smalltalk = "test\none bitAnd: 7!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none & 7;\n}\n", java);
	}

	public void testBitOr() {
		String smalltalk = "test\none bitOr: 7!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none | 7;\n}\n", java);
	}

	public void testBitXor() {
		String smalltalk = "test\none bitXor: 7!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none ^ 7;\n}\n", java);
	}

	public void testBlast() {
		String smalltalk = "test\nHeaper BLAST: #TestBlah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nthrow new AboraRuntimeException(AboraRuntimeException.TEST_BLAH);\n}\n", java);
	}

	public void testBlock() {
		String smalltalk = "test\n[one two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n{\none.two();\n}\n}\n", java);
	}

	public void testBlockArguments() {
		String smalltalk = "test\n[:a :b {T} |one two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n{\nObject a;\nT b;\none.two();\n}\n}\n", java);
	}

	public void testBlockEmpty() {
		String smalltalk = "test\n[]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n{\n}\n}\n", java);
	}

	public void testBlockTemps() {
		String smalltalk = "test\n[|:a :b {T} |one two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n{\nObject a;\nT b;\none.two();\n}\n}\n", java);
	}

	public void testBrackets() {
		String smalltalk = "test\nfred := (one two)!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfred = (one.two());\n}\n", java);
	}

	public void testBracketsEmbedded() {
		String smalltalk = "test\nfred := ((one two))!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfred = ((one.two()));\n}\n", java);
	}

	public void testBracketsFollowing() {
		String smalltalk = "test\nfred := (one two) three!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfred = (one.two()).three();\n}\n", java);
	}

	public void testCascade() {
		//TODO probably not good enough
		String smalltalk = "test\none two; three!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none.two().three();\n}\n", java);
	}

	public void testCast() {
		String smalltalk = "test\nblah cast: Peter!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n(Peter) blah;\n}\n", java);
	}

	public void testCastCall() {
		String smalltalk = "test\nself happy cast: Peter!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n(Peter) happy();\n}\n", java);
	}

	public void testCastMore() {
		String smalltalk = "test\napple _ (blah able: george) cast: Peter!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\napple = (Peter) (blah.able(george));\n}\n", java);
	}

	public void testCAThashForEquals() {
		String smalltalk = "test\n^#cat.U.Test hashForEqual!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nreturn HashHelper.hashForEqual(this.getClass());\n}\n", java);
	}

	public void testCharacter() {
		String smalltalk = "test\n$a.$-!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n'a';\n'-';\n}\n", java);
	}

	public void testClassCall() {
		String smalltalk = "test\nHeaper blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nHeaper.blah();\n}\n", java);
	}

	public void testClassSelfCall() {
		String smalltalk = "test\nself class blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nTest.blah();\n}\n", java);
	}

	public void testClassNonReference() {
		String smalltalk = "test\nNonClass!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nNonClass;\n}\n", java);
	}

	public void testClassReference() {
		String smalltalk = "test\nHeaper!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nHeaper.getCategory();\n}\n", java);
	}

	public void testClassReference2() {
		String smalltalk = "test\ntable at: 1 storeValue: Heaper)!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\ntable.storeValue(1, Heaper.getCategory());\n}\n", java);
	}

	public void testComment() {
		String smalltalk = "test\nfred := 1\n\"Hello There\"!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfred = 1\n/* Hello There */\n;\n}\n", java);
	}

	public void testCreateCall() {
		String smalltalk = "test\nBlah create: 12!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nnew Blah(12);\n}\n", java);
	}

	public void testCreateCallSelf() {
		String smalltalk = "test\nself create: 12!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nnew Test(12);\n}\n", java);
	}

	public void testCreateSuper() {
		String smalltalk = "create: blah\nsuper create: blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public Test(Object blah) {\nsuper(blah);\n}\n", java);
	}

	public void testCreateSuperWithDeclarations() {
		String smalltalk = "create\n| blah |\nsuper create: 2. blah := 1!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public Test() {\nsuper(2);\nObject blah;\nblah = 1;\n}\n", java);
	}

	public void testCritical() {
		String smalltalk = "test\nmutex critical: [blah]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nsynchronized (mutex) {\nblah;\n}\n}\n", java);
	}

	public void testDiv() {
		String smalltalk = "test\n11 // 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n11 / 2;\n}\n", java);
	}

	public void testDOTasLong() {
		String smalltalk = "test\n12 DOTasLong!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n12;\n}\n", java);
	}

	public void testDOTasInt() {
		String smalltalk = "test\n12 DOTasInt!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n12;\n}\n", java);
	}

	public void testDOTasInt32() {
		String smalltalk = "test\n12 DOTasInt32!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n12;\n}\n", java);
	}

	public void testDOTasUInt32() {
		String smalltalk = "test\n12 DOTasUInt32!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n12;\n}\n", java);
	}

	public void testDouble() {
		String smalltalk = "test\n187.123!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n187.123;\n}\n", java);
	}

	public void testEmpty() {
		String smalltalk = "test!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n}\n", java);
	}

	public void testForEach() {
		String smalltalk = "test\nfred forEach: [:element {IntegerPos}| element]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"public void test() {\nfor (Stepper stomp = fred ; stomp.hasValue() ; stomp.step()) {\nIntegerPos element = (IntegerPos )stomp.fetch();\nelement;\n}\n}\n",
/*			"public void test() {\nfor (Stepper stepper = fred ; stepper.hasValue() ; stepper.step()) {\nIntegerPos element = (IntegerPos )stepper.fetch();\nelement;\n}\nstepper.destroy();\n}\n",*/
			java);
	}

	public void testForEachHeaper() {
		String smalltalk = "test\nfred forEach: [:element | element]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"public void test() {\nfor (Stepper stomp = fred ; stomp.hasValue() ; stomp.step()) {\nHeaper element = stomp.fetch();\nelement;\n}\n}\n",
			java);
	}

	public void testGetStatic() {
		String smalltalk = "getCategory\n!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public static void getCategory() {\n}\n", java);
	}

	public void testIfFalse() {
		String smalltalk = "test\n(one = two) ifFalse: [^one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif ( ! (one == two)) {\nreturn one;\n}\n}\n", java);
	}

	public void testIfTrue() {
		String smalltalk = "test\n(one = two) ifTrue: [^one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (one == two) {\nreturn one;\n}\n}\n", java);
	}

	public void testIfTrueIfFalse() {
		String smalltalk = "test\n(one = two) ifTrue: [^one] ifFalse: [^two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (one == two) {\nreturn one;\n}\nelse {\nreturn two;\n}\n}\n", java);
	}

	public void testIfTrueLeadingAssignments() {
		String smalltalk = "test\na := b := (one = two) ifTrue: [one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\na = b = if (one == two) {\none;\n}\n}\n", java);
	}

	public void testIfTrueLeadingReturn() {
		String smalltalk = "test\n^(one = two) ifTrue: [one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nreturn if (one == two) {\none;\n}\n}\n", java);
	}

	public void testIfTrueMissingTestParentheses() {
		String smalltalk = "test\none = two ifTrue: [^one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (one == two) {\nreturn one;\n}\n}\n", java);
	}

	public void testIfTrueMissingTestParenthesesComplicated() {
		String smalltalk = "test\none = (two + 2) ifTrue: [^one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (one == (two + 2)) {\nreturn one;\n}\n}\n", java);
	}

	public void testIfTrueMissingTestParenthesesMoreComplicated() {
		String smalltalk = "test\n(a blah: b) ~~ 98 ifTrue: [^one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif ((a.blah(b)) != 98) {\nreturn one;\n}\n}\n", java);
	}

	public void testInt32Zero() {
		String smalltalk = "test\nInt32Zero + 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n0 + 2;\n}\n", java);
	}

	public void testInteger() {
		String smalltalk = "test\n187!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n187;\n}\n", java);
	}

	public void testIntegerHex() {
		String smalltalk = "test\n16rA1!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n0xa1;\n}\n", java);
	}

	public void testIntegerLong() {
		String smalltalk = "test\n12345678901!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n12345678901L;\n}\n", java);
	}

	public void testIntegerNegative() {
		String smalltalk = "test\n-187!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n-187;\n}\n", java);
	}

	public void testIntegerVar0() {
		String smalltalk = "test\nblah _ IntegerVar0!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah = 0;\n}\n", java);
	}

	public void testIntegerIntegerVar0() {
		String smalltalk = "test\nblah _ Integer IntegerVar: 0!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah = 0;\n}\n", java);
	}

	public void testIntegerVarZero() {
		String smalltalk = "test\nblah _ IntegerVarZero!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah = 0;\n}\n", java);
	}

	public void testIsKindOf() {
		String smalltalk = "test\nblah isKindOf: Heaper!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah instanceof Heaper;\n}\n", java);
	}

	public void testJavaDocComment() {
		String smalltalk = "test\n\"Hello there\"!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("/**\n * Hello there\n */\npublic void test() {\n}\n", java);
	}

	public void testJavaDocCommentLong() {
		String smalltalk =
			"test\n\"Hello there, what is the time of day once we all are here and there we go to be. Is this a really long comment or is it simply a lot of nonesense made up on the spot.\"!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"/**\n * Hello there, what is the time of day once we all are here and there we go to be. Is this a\n * really long comment or is it simply a lot of nonesense made up on the spot.\n */\npublic void test() {\n}\n",
			java);
	}

	public void testKeyword() {
		String smalltalk = "test\none two three: four!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none.two().three(four);\n}\n", java);
	}

	public void testKeyword2() {
		String smalltalk = "test\none two three: four and: 55!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none.two().threeAnd(four, 55);\n}\n", java);
	}

	public void testModulus() {
		String smalltalk = "test\n11 \\\\ 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n11 % 2;\n}\n", java);
	}

	public void testMethodNameFullStop() {
		String smalltalk = "test.extra!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n}\n", java);
	}

	public void testMethodNameFullStopKeyword() {
		String smalltalk = "test\nself blah.extra: 1!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah(1);\n}\n", java);
	}

	public void testMultipleStatements() {
		String smalltalk = "test\none two.\nborris := three + 3!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none.two();\nborris = three + 3;\n}\n", java);
	}

	public void testNil() {
		String smalltalk = "test\nfred := nil!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfred = null;\n}\n", java);
	}

	public void testNULL() {
		String smalltalk = "test\nfred := NULL!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfred = null;\n}\n", java);
	}

	public void testOr() {
		String smalltalk = "test\none or: [two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none || (two);\n}\n", java);
	}

	public void testOverrideReturnType() {
		String smalltalk = "{Blah} isEqual\n^blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public boolean isEqual() {\nreturn blah;\n}\n", java);
	}

	public void testOverrideReturnType2() {
		String smalltalk = "isEqual\n^blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public boolean isEqual() {\nreturn blah;\n}\n", java);
	}

	public void testOverrideVoidReturnTypeWithClass() {
		String smalltalk = "{Blah} make\n^blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public Blah make() {\nreturn blah;\n}\n", java);
	}

	public void testOverrideVoidReturnTypeWithClass2() {
		String smalltalk = "make\n^blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public Test make() {\nreturn blah;\n}\n", java);
	}

	public void testOverrideVoidReturnType() {
		String smalltalk = "{TableStepper} stepper\n^blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public TableStepper stepper() {\nreturn blah;\n}\n", java);
	}

	public void testOverrideVoidReturnType2() {
		String smalltalk = "stepper\n^blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public Stepper stepper() {\nreturn blah;\n}\n", java);
	}

	public void testPrint() {
		String smalltalk = "test: aStream { ostream }\naStream << self blah: 34!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test(PrintWriter aStream) {\naStream.print(blah(34));\n}\n", java);
	}

	public void testQuickCast() {
		String smalltalk = "test\nblah quickCast: Peter!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n(Peter) blah;\n}\n", java);
	}

	public void testReturn() {
		String smalltalk = "test\n^fred!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nreturn fred;\n}\n", java);
	}

	public void testReturnIllegalLeading() {
		String smalltalk = "test\n\123 ^fred!";

		try {
			writeInstanceMethod(smalltalk);
			fail("expected exception");
		} catch (IllegalStateException e) {
			//expected
		}
	}

	public void testReturnLeadingComments() {
		String smalltalk = "test\n123.\"whole\" \"lot of comments\" ^fred!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n123;\n/* whole */\n/* lot of comments */\nreturn fred;\n}\n", java);
	}

	public void testReturnVoid() {
		String smalltalk = "test\n^VOID!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nreturn ;\n}\n", java);
	}

	public void testSelfSends() {
		String smalltalk = "test\nself kill. self one: 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nkill();\none(2);\n}\n", java);
	}

	public void testSmalltalkOnly() {
		String smalltalk = "test\n[one blah] smalltalkOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n/* >>> smalltalkOnly */\none.blah();\n/* <<< smalltalkOnly */\n}\n", java);
	}

	public void testString() {
		String smalltalk = "test\n'hi there'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n\"hi there\";\n}\n", java);
	}

	public void testStringDeclaration() {
		String smalltalk = "test: string {Character star}\nstring!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test(String string) {\nstring;\n}\n", java);
	}

	public void testStringWrapped() {
		String smalltalk = "test\n'hi there\nand here\n'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n\"hi there\\n\"+\n\"and here\\n\"+\n\"\";\n}\n", java);
	}

	public void testSubclassResponsibility() {
		String smalltalk = "test\nself subclassResponsibility!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nthrow new SubclassResponsibilityException();\n}\n", java);
	}

	public void testSuper() {
		String smalltalk = "test\nsuper borris!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nsuper.borris();\n}\n", java);
	}

	public void testSymbol() {
		String smalltalk = "test\n#HiThere!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nHI_THERE;\n}\n", java);
	}

	public void testSymbolInQuotes() {
		String smalltalk = "test\n#'HiThere'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nHI_THERE;\n}\n", java);
	}

	public void testSymbolInQuotesComplex() {
		String smalltalk = "test\n#'HiThere: Again'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nHI_THERE__AGAIN;\n}\n", java);
	}

	public void testSymbolInQuotesUpperCase() {
		String smalltalk = "test\n#'HI_THERE'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nHI_THERE;\n}\n", java);
	}

	public void testSymbolArrayNew() {
		String smalltalk = "test\n#()!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nArray.new();\n}\n", java);
	}

	public void testTemps() {
		String smalltalk = "test\n| one two {Test}|!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nObject one;\nTest two;\n}\n", java);
	}

	public void testTempsEmpty() {
		String smalltalk = "test\n| |!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n}\n", java);
	}

	public void testTimesRepeat() {
		String smalltalk = "test\n4 timesRepeat: [self blah]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfor (int i = 0 ; i < 4 ; i ++ ) {\nblah();\n}\n}\n", java);
	}

	public void testTranslateOnlyString() {
		String smalltalk = "test\n'hello there' translateOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n/* translateOnly \"hello there\" */\n}\n", java);
	}

	public void testValueNowOrOnUnwindDo() {
		String smalltalk = "test\n[blah] valueNowOrOnUnwindDo: [hello]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\ntry {\nblah;\n}\nfinally {\nhello;\n}\n}\n", java);
	}

	public void testUInt32Zero() {
		String smalltalk = "test\nUInt32Zero + 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n0 + 2;\n}\n", java);
	}

	public void testUses() {
		String smalltalk = "test\n1. [HistoryCrum] USES. 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n1;\n2;\n}\n", java);
	}

	public void testUnaryOperator() {
		String smalltalk = "test\nfred kill!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfred.kill();\n}\n", java);
	}

	public void testUnaryOperatorMultiple() {
		String smalltalk = "test\nharry one two three!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nharry.one().two().three();\n}\n", java);
	}

	public void testUnreachableCodeReturnFodder() {
		String smalltalk = "test\nHeaper BLAST: #NotInTable.\n^NULL!";
		
		String java = writeInstanceMethod(smalltalk);
		
		assertEquals("public void test() {\nthrow new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);\n}\n", java);
	}

	public void testUnreachableCodeReturnFodderWithComment() {
		String smalltalk = "test\nHeaper BLAST: #NotInTable.\n^NULL \"fodder\"!";
		
		String java = writeInstanceMethod(smalltalk);
		
		assertEquals("public void test() {\nthrow new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);\n}\n", java);
	}
	
	public void testWhileTrue() {
		String smalltalk = "test\n[a < 1] whileTrue: [a _ a + 1]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nwhile (a < 1) {\na = a + 1;\n}\n}\n", java);
	}

	protected String writeInstanceMethod(String smalltalk) {
		return writeMethod(smalltalk, "");
	}

	protected String writeMethod(String smalltalk, String modifiers) {
		ChunkDetails details = new ChunkDetails("", smalltalk);
		StringWriter stringWriter = new StringWriter();
		PrintWriter printWriter = new PrintWriter(stringWriter);
		ClassParser classParser = new ClassParser();
		classParser.setJavaClass(javaClass);
		JavaMethod javaMethod = classParser.parseMethod(details, modifiers);
		classWriter.writeMethod(javaMethod, printWriter);
		printWriter.close();
		return stringWriter.toString();
	}
}