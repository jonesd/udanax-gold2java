/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ug2java.tests;

import java.io.*;
import java.util.Hashtable;

import org.abora.ug2java.*;

import junit.framework.*;

/**
 * JUnit test case for TestWriteMethod
 */

public class TestWriteMethod extends TestCase {
	private static final Class THIS = TestWriteMethod.class;

	private ClassWriter classWriter;

	public TestWriteMethod(String name) {
		super(name);
	}

	public void setUp() {
		Hashtable packageLookup = new Hashtable();
		packageLookup.put("Heaper", "org.abora.gold.xpp.basic");
		classWriter = new ClassWriter(packageLookup);
		classWriter.className = "Test";
		classWriter.quoteSmalltalk = false;
	}

	public static Test suite() {
		return new TestSuite(THIS);
	}

	public void testAnd() {
		String smalltalk = "test\none and: [two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none && (two);\r\n}\r\n", java);
	}

	public void testAndAnd() {
		String smalltalk = "test\none and: [two and: [three]]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none && (two && (three));\r\n}\r\n", java);
	}

	public void testAlmostTo() {
		String smalltalk = "test\n0 almostTo: fred happy do: [:i {UInt32} | blah ]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfor (int i = 0 ; i < fred.happy() ; i ++ ) {\r\nblah;\r\n}\r\n}\r\n", java);
	}

	public void testAssign() {
		String smalltalk = "test\nfred _ 1.\nharry :=2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfred = 1;\r\nharry = 2;\r\n}\r\n", java);
	}

	public void testAssignUnaryOperator() {
		String smalltalk = "test\nfred := harry kill!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfred = harry.kill();\r\n}\r\n", java);
	}

	public void testAtStoreValue() {
		String smalltalk = "test\ntable at: 1 storeValue: value)!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\ntable.storeValue(1, value);\r\n}\r\n", java);
	}

	public void testBinaryOperator() {
		String smalltalk = "test\none < two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none < two;\r\n}\r\n", java);
	}

	public void testBinaryOperatorEquals() {
		String smalltalk = "test\none = two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none == two;\r\n}\r\n", java);
	}

	public void testBinaryOperatorIdentity() {
		String smalltalk = "test\none == two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none == two;\r\n}\r\n", java);
	}

	public void testBinaryOperatorNotEquals() {
		String smalltalk = "test\none ~= two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none != two;\r\n}\r\n", java);
	}

	public void testBinaryOperatorNotIdentity() {
		String smalltalk = "test\none ~~ two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none != two;\r\n}\r\n", java);
	}

	public void testBinaryOperatorWithUnary() {
		String smalltalk = "test\none and < two three!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none.and() < two.three();\r\n}\r\n", java);
	}

	public void testBitAnd() {
		String smalltalk = "test\none bitAnd: 7!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none & 7;\r\n}\r\n", java);
	}

	public void testBitOr() {
		String smalltalk = "test\none bitOr: 7!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none | 7;\r\n}\r\n", java);
	}

	public void testBitXor() {
		String smalltalk = "test\none bitXor: 7!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none ^ 7;\r\n}\r\n", java);
	}

	public void testBlast() {
		String smalltalk = "test\nHeaper BLAST: #TestBlah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nthrow new AboraRuntimeException(AboraRuntimeException.TEST_BLAH);\r\n}\r\n", java);
	}

	public void testBlock() {
		String smalltalk = "test\n[one two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n{\r\none.two();\r\n}\r\n}\r\n", java);
	}

	public void testBlockArguments() {
		String smalltalk = "test\n[:a :b {T} |one two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n{\r\nObject a;\r\nT b;\r\none.two();\r\n}\r\n}\r\n", java);
	}

	public void testBlockEmpty() {
		String smalltalk = "test\n[]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n{\r\n}\r\n}\r\n", java);
	}

	public void testBlockTemps() {
		String smalltalk = "test\n[|:a :b {T} |one two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n{\r\nObject a;\r\nT b;\r\none.two();\r\n}\r\n}\r\n", java);
	}

	public void testBrackets() {
		String smalltalk = "test\nfred := (one two)!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfred = (one.two());\r\n}\r\n", java);
	}

	public void testBracketsEmbedded() {
		String smalltalk = "test\nfred := ((one two))!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfred = ((one.two()));\r\n}\r\n", java);
	}

	public void testBracketsFollowing() {
		String smalltalk = "test\nfred := (one two) three!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfred = (one.two()).three();\r\n}\r\n", java);
	}

	public void testCascade() {
		//TODO probably not good enough
		String smalltalk = "test\none two; three!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none.two().three();\r\n}\r\n", java);
	}

	public void testCast() {
		String smalltalk = "test\nblah cast: Peter!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n(Peter) blah;\r\n}\r\n", java);
	}

	public void testCastCall() {
		String smalltalk = "test\nself happy cast: Peter!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n(Peter) happy();\r\n}\r\n", java);
	}

	public void testCastMore() {
		String smalltalk = "test\napple _ (blah able: george) cast: Peter!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\napple = (Peter) (blah.able(george));\r\n}\r\n", java);
	}

	public void testCharacter() {
		String smalltalk = "test\n$a.$-!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n'a';\r\n'-';\r\n}\r\n", java);
	}

	public void testClassCall() {
		String smalltalk = "test\nHeaper blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nHeaper.blah();\r\n}\r\n", java);
	}

	public void testClassSelfCall() {
		String smalltalk = "test\nself class blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nTest.blah();\r\n}\r\n", java);
	}

	public void testClassNonReference() {
		String smalltalk = "test\nNonClass!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nNonClass;\r\n}\r\n", java);
	}

	public void testClassReference() {
		String smalltalk = "test\nHeaper!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nHeaper.getCategory();\r\n}\r\n", java);
	}

	public void testClassReference2() {
		String smalltalk = "test\ntable at: 1 storeValue: Heaper)!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\ntable.storeValue(1, Heaper.getCategory());\r\n}\r\n", java);
	}

	public void testComment() {
		String smalltalk = "test\nfred := 1\n\"Hello There\"!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfred = 1\r\n/* Hello There */\r\n;\r\n}\r\n", java);
	}

	public void testCreateCall() {
		String smalltalk = "test\nBlah create: 12!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nnew Blah(12);\r\n}\r\n", java);
	}

	public void testCreateCallSelf() {
		String smalltalk = "test\nself create: 12!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nnew Test(12);\r\n}\r\n", java);
	}

	public void testCreateSuper() {
		String smalltalk = "create: blah\nsuper create: blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public  Test(Object blah) {\r\nsuper(blah);\r\n}\r\n", java);
	}

	public void testCritical() {
		String smalltalk = "test\nmutex critical: [blah]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nsynchronized (mutex) {\r\nblah;\r\n}\r\n}\r\n", java);
	}

	public void testDouble() {
		String smalltalk = "test\n187.123!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n187.123;\r\n}\r\n", java);
	}

	public void testEmpty() {
		String smalltalk = "test!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n}\r\n", java);
	}

	public void testForEach() {
		String smalltalk = "test\nfred forEach: [:element | element]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"public void test() {\r\nfor (Iterator iterator = fred.forEach() ; iterator.hasNext() ; ) {\r\nObject element = (Object )iterator.next();\r\nelement;\r\n}\r\n}\r\n",
			java);
	}

	public void testGetStatic() {
		String smalltalk = "getCategory\n!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public static void getCategory() {\r\n}\r\n", java);
	}

	public void testIfFalse() {
		String smalltalk = "test\n(one = two) ifFalse: [^one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nif ( ! (one == two)) {\r\nreturn one;\r\n}\r\n}\r\n", java);
	}

	public void testIfTrue() {
		String smalltalk = "test\n(one = two) ifTrue: [^one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nif (one == two) {\r\nreturn one;\r\n}\r\n}\r\n", java);
	}

	public void testIfTrueIfFalse() {
		String smalltalk = "test\n(one = two) ifTrue: [^one] ifFalse: [^two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nif (one == two) {\r\nreturn one;\r\n}\r\nelse {\r\nreturn two;\r\n}\r\n}\r\n", java);
	}

	public void testIfTrueLeadingAssignments() {
		String smalltalk = "test\na := b := (one = two) ifTrue: [one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\na = b = if (one == two) {\r\none;\r\n}\r\n}\r\n", java);
	}

	public void testIfTrueLeadingReturn() {
		String smalltalk = "test\n^(one = two) ifTrue: [one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nreturn if (one == two) {\r\none;\r\n}\r\n}\r\n", java);
	}

	public void testIfTrueMissingTestParentheses() {
		String smalltalk = "test\none = two ifTrue: [^one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nif (one == two) {\r\nreturn one;\r\n}\r\n}\r\n", java);
	}

	public void testIfTrueMissingTestParenthesesComplicated() {
		String smalltalk = "test\none = (two + 2) ifTrue: [^one]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nif (one == (two + 2)) {\r\nreturn one;\r\n}\r\n}\r\n", java);
	}

	public void testInt32Zero() {
		String smalltalk = "test\nInt32Zero + 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n0 + 2;\r\n}\r\n", java);
	}

	public void testInteger() {
		String smalltalk = "test\n187!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n187;\r\n}\r\n", java);
	}

	public void testIntegerHex() {
		String smalltalk = "test\n16rA1!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n0xa1;\r\n}\r\n", java);
	}

	public void testIntegerLong() {
		String smalltalk = "test\n12345678901!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n12345678901L;\r\n}\r\n", java);
	}

	public void testIntegerNegative() {
		String smalltalk = "test\n-187!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n-187;\r\n}\r\n", java);
	}

	public void testIntegerVar0() {
		String smalltalk = "test\nblah _ IntegerVar0!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nblah = IntegerVar.zero();\r\n}\r\n", java);
	}

	public void testIntegerVarZero() {
		String smalltalk = "test\nblah _ IntegerVarZero!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nblah = IntegerVar.zero();\r\n}\r\n", java);
	}

	public void testIsKindOf() {
		String smalltalk = "test\nblah isKindOf: Heaper!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nblah instanceof Heaper;\r\n}\r\n", java);
	}

	public void testJavaDocComment() {
		String smalltalk = "test\n\"Hello there\"!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("/**\r\n * Hello there\r\n */\r\npublic void test() {\r\n}\r\n", java);
	}

	public void testJavaDocCommentLong() {
		String smalltalk =
			"test\n\"Hello there, what is the time of day once we all are here and there we go to be. Is this a really long comment or is it simply a lot of nonesense made up on the spot.\"!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"/**\r\n * Hello there, what is the time of day once we all are here and there we go to be. Is this a\r\n * really long comment or is it simply a lot of nonesense made up on the spot.\r\n */\r\npublic void test() {\r\n}\r\n",
			java);
	}

	public void testKeyword() {
		String smalltalk = "test\none two three: four!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none.two().three(four);\r\n}\r\n", java);
	}

	public void testKeyword2() {
		String smalltalk = "test\none two three: four and: 55!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none.two().threeAnd(four, 55);\r\n}\r\n", java);
	}

	public void testModulus() {
		String smalltalk = "test\n11 \\\\ 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n11 % 2;\r\n}\r\n", java);
	}

	public void testMethodNameFullStop() {
		String smalltalk = "test.extra!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n}\r\n", java);
	}

	public void testMethodNameFullStopKeyword() {
		String smalltalk = "test\nself blah.extra: 1!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nblah(1);\r\n}\r\n", java);
	}

	public void testMultipleStatements() {
		String smalltalk = "test\none two.\nborris := three + 3!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none.two();\r\nborris = three + 3;\r\n}\r\n", java);
	}

	public void testNil() {
		String smalltalk = "test\nfred := nil!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfred = null;\r\n}\r\n", java);
	}

	public void testNULL() {
		String smalltalk = "test\nfred := NULL!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfred = null;\r\n}\r\n", java);
	}

	public void testOr() {
		String smalltalk = "test\none or: [two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\none || (two);\r\n}\r\n", java);
	}

	public void testQuickCast() {
		String smalltalk = "test\nblah quickCast: Peter!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n(Peter) blah;\r\n}\r\n", java);
	}

	public void testReturn() {
		String smalltalk = "test\n^fred!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nreturn fred;\r\n}\r\n", java);
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

		assertEquals("public void test() {\r\n123;\r\n/* whole */\r\n/* lot of comments */\r\nreturn fred;\r\n}\r\n", java);
	}

	public void testReturnVoid() {
		String smalltalk = "test\n^VOID!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nreturn ;\r\n}\r\n", java);
	}

	public void testSelfSends() {
		String smalltalk = "test\nself kill. self one: 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nkill();\r\none(2);\r\n}\r\n", java);
	}

	public void testSmalltalkOnly() {
		String smalltalk = "test\n[one blah] smalltalkOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n/* >>> smalltalkOnly */\r\none.blah();\r\n/* <<< smalltalkOnly */\r\n}\r\n", java);
	}

	public void testString() {
		String smalltalk = "test\n'hi there'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n\"hi there\";\r\n}\r\n", java);
	}

	public void testStringDeclaration() {
		String smalltalk = "test: string {Character star}\nstring!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test(String string) {\r\nstring;\r\n}\r\n", java);
	}

	public void testSubclassResponsibility() {
		String smalltalk = "test\nself subclassResponsibility!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nAbstract.subclassResponsibility();\r\n}\r\n", java);
	}

	public void testSuper() {
		String smalltalk = "test\nsuper borris!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nsuper.borris();\r\n}\r\n", java);
	}

	public void testSymbol() {
		String smalltalk = "test\n#HiThere!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nHI_THERE;\r\n}\r\n", java);
	}

	public void testSymbolInQuotes() {
		String smalltalk = "test\n#'HiThere'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nHI_THERE;\r\n}\r\n", java);
	}

	public void testSymbolInQuotesComplex() {
		String smalltalk = "test\n#'HiThere: Again'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nHI_THERE__AGAIN;\r\n}\r\n", java);
	}

	public void testSymbolInQuotesUpperCase() {
		String smalltalk = "test\n#'HI_THERE'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nHI_THERE;\r\n}\r\n", java);
	}

	public void testSymbolArrayNew() {
		String smalltalk = "test\n#()!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nArray.new();\r\n}\r\n", java);
	}

	public void testTemps() {
		String smalltalk = "test\n| one two {Test}|!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nObject one;\r\nTest two;\r\n}\r\n", java);
	}

	public void testTempsEmpty() {
		String smalltalk = "test\n| |!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n}\r\n", java);
	}

	public void testTimesRepeat() {
		String smalltalk = "test\n4 timesRepeat: [self blah]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfor (int i = 0 ; i < 4 ; i ++ ) {\r\nblah();\r\n}\r\n}\r\n", java);
	}

	public void testTranslateOnlyString() {
		String smalltalk = "test\n'hello there' translateOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n/* translateOnly \"hello there\" */\r\n}\r\n", java);
	}

	public void testValueNowOrOnUnwindDo() {
		String smalltalk = "test\n[blah] valueNowOrOnUnwindDo: [hello]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\ntry {\r\nblah;\r\n}\r\nfinally {\r\nhello;\r\n}\r\n}\r\n", java);
	}

	public void testUInt32Zero() {
		String smalltalk = "test\nUInt32Zero + 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n0 + 2;\r\n}\r\n", java);
	}

	public void testUses() {
		String smalltalk = "test\n1. [HistoryCrum] USES. 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\n1;\r\n2;\r\n}\r\n", java);
	}

	public void testUnaryOperator() {
		String smalltalk = "test\nfred kill!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nfred.kill();\r\n}\r\n", java);
	}

	public void testUnaryOperatorMultiple() {
		String smalltalk = "test\nharry one two three!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nharry.one().two().three();\r\n}\r\n", java);
	}

	public void testWhileTrue() {
		String smalltalk = "test\n[a < 1] whileTrue: [a _ a + 1]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\r\nwhile (a < 1) {\r\na = a + 1;\r\n}\r\n}\r\n", java);
	}

	protected String writeInstanceMethod(String smalltalk) {
		return writeMethod(smalltalk, "");
	}

	protected String writeMethod(String smalltalk, String modifiers) {
		ChunkDetails details = new ChunkDetails("", smalltalk);
		StringWriter stringWriter = new StringWriter();
		PrintWriter printWriter = new PrintWriter(stringWriter);
		classWriter.writeMethod(printWriter, details, modifiers);
		printWriter.close();
		return stringWriter.toString();
	}
}