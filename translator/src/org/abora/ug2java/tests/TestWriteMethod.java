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
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
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

	public void testAlmostToDo() {
		String smalltalk = "test\n0 almostTo: fred happy do: [:i {UInt32} | blah ]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfor (int i = 0 ; i < fred.happy() ; i ++ ) {\nblah;\n}\n}\n", java);
	}

	public void testAlmostToDoAfterIf() {
		String smalltalk = "test\nmyValue == NULL ifTrue: [^VOID]. 0 almostTo: fred happy do: [:i {UInt32} | blah ]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (myValue == null) {\nreturn ;\n}\nfor (int i = 0 ; i < fred.happy() ; i ++ ) {\nblah;\n}\n}\n", java);
	}

	public void testAlmostToByDoPositive() {
		String smalltalk = "test\n0 almostTo: fred happy by: 2 do: [:i {UInt32} | blah ]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfor (int i = 0 ; i < fred.happy() ; i += 2 ) {\nblah;\n}\n}\n", java);
	}

	public void testAlmostToByDoNegative() {
		String smalltalk = "test\n0 almostTo: fred happy by: -2 do: [:i {UInt32} | blah ]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfor (int i = 0 ; i > fred.happy() ; i -= 2 ) {\nblah;\n}\n}\n", java);
	}

	public void testAssert() {
		String smalltalk = "test\n(fred < 1) assert!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (fred < 1) throw new AboraAssertionException();\n}\n", java);
	}

	public void testAssertWithMessage() {
		String smalltalk = "test\nfred < 1 assert: 'hello'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (fred < 1) throw new AboraAssertionException(\"hello\");\n}\n", java);
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

	public void testAtMethod() {
		String smalltalk = "atBlahGo\n!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void blahGo() {\n}\n", java);
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

	public void testBasicCastWithStar() {
		String smalltalk = "test\nmyTrace basicCast: Heaper star!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n(Heaper) myTrace;\n}\n", java);
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

	public void testBitShift() {
		String smalltalk = "test\none bitShift: 7!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none << 7;\n}\n", java);
	}

	public void testBitShiftRight() {
		String smalltalk = "test\none bitShiftRight: 7!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\none >> 7;\n}\n", java);
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

	public void testCastShouldOverrideType() {
		String smalltalk = "test\nblah cast: UInt32!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n(int) blah;\n}\n", java);
	}

	public void testCastMore() {
		String smalltalk = "test\napple _ (blah able: george) cast: Peter!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\napple = (Peter) (blah.able(george));\n}\n", java);
	}

	public void testCastInto() {
		String smalltalk = "test\nblah cast: Pair into: [:pair | pair left]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (blah instanceof Pair) {\nPair pair = (Pair) blah;\npair.left();\n}\n}\n", java);
	}

	public void testCastIntoOthers() {
		String smalltalk = "test\nblah cast: Pair into: [:pair | pair left] others: [1]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (blah instanceof Pair) {\nPair pair = (Pair) blah;\npair.left();\n}\nelse {\n1;\n}\n}\n", java);
	}

	public void testCastIntoCastIntoOthers() {
		String smalltalk = "test\nblah cast: Pair into: [:pair | pair left] cast: Region into: [:region | region size] others: [1]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (blah instanceof Pair) {\nPair pair = (Pair) blah;\npair.left();\n}\nelse if (blah instanceof Region) {\nRegion region = (Region) blah;\nregion.size();\n}\nelse {\n1;\n}\n}\n", java);
	}

	public void testCastIntoCastInto() {
		String smalltalk = "test\nblah cast: Pair into: [:pair | pair left] cast: Region into: [:region | region size]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nif (blah instanceof Pair) {\nPair pair = (Pair) blah;\npair.left();\n}\nelse if (blah instanceof Region) {\nRegion region = (Region) blah;\nregion.size();\n}\n}\n", java);
	}

	public void testCastExpressionIntoOthers() {
		String smalltalk = "test\none blah cast: Pair into: [:pair | pair left] others: [1]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nHeaper cast1 = one.blah();\nif (cast1 instanceof Pair) {\nPair pair = (Pair) cast1;\npair.left();\n}\nelse {\n1;\n}\n}\n", java);
	}

	public void testCategoryName() {
		String smalltalk = "test\n^self getCategory name!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nreturn getClass().getName();\n}\n", java);
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

		assertEquals("public void test() {\nAboraSupport.findCategory(Heaper.class);\n}\n", java);
	}

	public void testClassReference2() {
		String smalltalk = "test\ntable at: 1 storeValue: Heaper)!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\ntable.storeValue(1, AboraSupport.findCategory(Heaper.class));\n}\n", java);
	}

	public void testComment() {
		String smalltalk = "test\nfred := 1\n\"Hello There\"!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfred = 1;\n/* Hello There */\n}\n", java);
	}

	public void testCompileFodder() {
		String smalltalk = "test\nblah. ^false \"compiler fodder\"!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah;\n}\n", java);
	}

	public void testCompileFodder2() {
		String smalltalk = "test\nblah. ^false \"fodder\"!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah;\n}\n", java);
	}

	public void testConditionalOperator() {
		String smalltalk = "test\n^ (one = two) ifTrue: [one] ifFalse: [two]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nreturn (one == two) ? one : two;\n}\n", java);
	}

	public void testDiskManagerConsistentDefaultDirty() {
		String smalltalk = "test\nDiskManager consistent: [self diskUpdate]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nAboraBlockSupport.enterConsistent();\ntry {\ndiskUpdate();\n}\nfinally {\nAboraBlockSupport.exitConsistent();\n}\n}\n", java);
	}

	public void testDiskManagerConsistentWithIdentifier() {
		String smalltalk = "test\nDiskManager consistent: 2 with: [self diskUpdate]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nAboraBlockSupport.enterConsistent(2);\ntry {\ndiskUpdate();\n}\nfinally {\nAboraBlockSupport.exitConsistent();\n}\n}\n", java);
	}

	public void testDiskManagerConsistentWithExpression() {
		String smalltalk = "test\nDiskManager consistent: self dirtyLevel with: [self diskUpdate]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nAboraBlockSupport.enterConsistent(dirtyLevel());\ntry {\ndiskUpdate();\n}\nfinally {\nAboraBlockSupport.exitConsistent();\n}\n}\n", java);
	}

	public void testDiskManagerInsistentWithExpression() {
		String smalltalk = "test\nDiskManager insistent: self dirtyLevel with: [self diskUpdate]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nAboraBlockSupport.enterInsistent(dirtyLevel());\ntry {\ndiskUpdate();\n}\nfinally {\nAboraBlockSupport.exitInsistent();\n}\n}\n", java);
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
	
	public void testDOhashForEqual() {
		String smalltalk = "test\nblah DOThashForEqual!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nHashHelper.hashForEqual(blah);\n}\n", java);
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

	public void testFluidBindDuring() {
		String smalltalk = "test\nCurrentTrace fluidBind: myEnt newTrace during: [result := BeClub make: desc]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"public void test() {\nObject CurrentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());\ntry {\nresult = BeClub.make(desc);\n}\nfinally {\nAboraBlockSupport.exitFluidBindDuring(CurrentTrace, CurrentTraceOldValue);\n}\n}\n",
			java);
	}

	
	public void testFluidFetch() {
		String smalltalk = "test\nCurrentPacker fluidFetch blah!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"public void test() {\n((DiskManager) CurrentPacker.fluidFetch()).blah();\n}\n",
			java);
	}

	public void testForEach() {
		String smalltalk = "test\nfred forEach: [:element {IntegerPos}| element]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"public void test() {\nfor (Stepper stomp1 = fred ; stomp1.hasValue() ; stomp1.step()) {\nIntegerPos element = (IntegerPos )stomp1.fetch();\nelement;\n}\n}\n",
/*			"public void test() {\nfor (Stepper stepper = fred ; stepper.hasValue() ; stepper.step()) {\nIntegerPos element = (IntegerPos )stepper.fetch();\nelement;\n}\nstepper.destroy();\n}\n",*/
			java);
	}

	public void testForEachHeaper() {
		String smalltalk = "test\nfred forEach: [:element | element]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"public void test() {\nfor (Stepper stomp1 = fred ; stomp1.hasValue() ; stomp1.step()) {\nHeaper element = stomp1.fetch();\nelement;\n}\n}\n",
			java);
	}

	public void testForEachNested() {
		String smalltalk = "test\nfred forEach: [:element {IntegerPos}| blah forEach: [:element2 {RealPos} | element + element2]]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"public void test() {\nfor (Stepper stomp1 = fred ; stomp1.hasValue() ; stomp1.step()) {\nIntegerPos element = (IntegerPos )stomp1.fetch();\nfor (Stepper stomp2 = blah ; stomp2.hasValue() ; stomp2.step()) {\nRealPos element2 = (RealPos )stomp2.fetch();\nelement + element2;\n}\n}\n}\n",
			java);
	}

	public void testForIndices() {
		String smalltalk = "test\nfred forIndices: [:i {IntegerVar} :value {IntegerRegion}| element]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"public void test() {\nfor (TableStepper stomp1 = fred ; stomp1.hasValue() ; stomp1.step()) {\nint i = (int )stomp1.index();\nIntegerRegion value = (IntegerRegion )stomp1.fetch();\nelement;\n}\n}\n",
			java);
	}

	public void testForPositions() {
		String smalltalk = "test\nfred forPositions: [:key {IntegerPos} :value {IntegerRegion}| element]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals(
			"public void test() {\nfor (TableStepper stomp1 = fred ; stomp1.hasValue() ; stomp1.step()) {\nIntegerPos key = (IntegerPos )stomp1.position();\nIntegerRegion value = (IntegerRegion )stomp1.fetch();\nelement;\n}\n}\n",
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

	public void testInt32Min() {
		String smalltalk = "test\nInt32Min + 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n0x80000000 + 2;\n}\n", java);
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

	public void testIntegerCallOnIntVar() {
		String smalltalk = "test: blah {IntegerVar}\nblah integer!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test(int blah) {\nIntegerPos.make(blah);\n}\n", java);
	}

	public void testIntegerCallNonInt() {
		String smalltalk = "test: blah {Heaper}\nblah integer!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test(Heaper blah) {\nblah.integer();\n}\n", java);
	}

	public void testIntegerCallOnIntLiteral() {
		String smalltalk = "test\n27 integer!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nIntegerPos.make(27);\n}\n", java);
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

	public void testIntegerIntegerVar() {
		String smalltalk = "test\nblah _ Integer IntegerVar: tally!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah = tally;\n}\n", java);
	}

	public void testIntegerVarZero() {
		String smalltalk = "test\nblah _ IntegerVarZero!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah = 0;\n}\n", java);
	}

	public void testIntegerZero() {
		String smalltalk = "test\nblah _ IntegerZero!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah = IntegerPos.zero();\n}\n", java);
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

	public void testMax() {
		String smalltalk = "test\none max: two!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nMath.max(one, two);\n}\n", java);
	}

	public void testMin() {
		String smalltalk = "test\na := 1 min: blah grr!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\na = Math.min(1, blah.grr());\n}\n", java);
	}

	public void testModulus() {
		String smalltalk = "test\n11 \\\\ 2!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n11 % 2;\n}\n", java);
	}

	public void testMethodNameFullStop() {
		String smalltalk = "test.Extra!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void testExtra() {\n}\n", java);
	}

	public void testMethodNameFullStopCreate() {
		String smalltalk = "create.Extra!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public Test() {\n}\n", java);
	}

	public void testMethodNameFullStopKeyword() {
		String smalltalk = "test\nself blah.Extra: 1!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblahExtra(1);\n}\n", java);
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

	public void testNotBoolean() {
		String smalltalk = "test: blah {BooleanVar}\n^blah not!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test(boolean blah) {\nreturn ! blah;\n}\n", java);
	}

	public void testNotExpression() {
		String smalltalk = "test\n^blah isAlive not!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nreturn ! blah.isAlive();\n}\n", java);
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

	public void testParameterNamedSize() {
		String smalltalk = "test: size {IntegerVar}\nsize > 4 ifTrue: [\nsize] ifFalse: [size - 4]!";
		
		String java = writeInstanceMethod(smalltalk);
		
		assertEquals("public void test(int size) {\nif (size > 4) {\nsize;\n}\nelse {\nsize - 4;\n}\n}\n", java);
	}
//	create.IntegerVar: size {IntegerVar} 
//	"The optional argument just hints at the number of elements
//	 to eventually be added.  It makes no difference semantically."
//	| newSize {UInt32} |
//	super create.
//	size > 4 ifTrue: [newSize _ size DOTasLong] ifFalse: [newSize _ 4].

	public void testPasse() {
		String smalltalk = "test\nself passe!";
		
		String java = writeInstanceMethod(smalltalk);
		
		assertEquals("public void test() {\nthrow new PasseException();\n}\n", java);
	}

	public void testPrint() {
		String smalltalk = "test: aStream { ostream }\naStream << self blah: 34!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test(PrintWriter aStream) {\naStream.print(blah(34));\n}\n", java);
	}

	public void testPrintPrint() {
		String smalltalk = "test: aStream { ostream }\naStream << 12 + 1 << 13 + 2 << 14 + 3!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test(PrintWriter aStream) {\naStream.print(12 + 1);\naStream.print(13 + 2);\naStream.print(14 + 3);\n}\n", java);
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

	public void testShouldImplement() {
		String smalltalk = "test\n^Someone shouldImplement!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nthrow new ShouldImplementException(\"Someone\");\n}\n", java);
	}

	public void testSignals() {
		String smalltalk = "test\n^self signals: #(NotInTable)!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nthrow new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);\n}\n", java);
	}

	public void testSmalltalkOnly() {
		String smalltalk = "smalltalkOnly\n[one blah] smalltalkOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void smalltalkOnly() {\none.blah();\n}\n", java);
	}

	public void testSmalltalkOnlyMixed() {
		String smalltalk = "smalltalkOnly\n[two blah] translateOnly. [one blah] smalltalkOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void smalltalkOnly() {\none.blah();\n}\n", java);
	}

	public void testSmalltalkOnlySimpleTranlation() {
		String smalltalk = "test\n[one blah] smalltalkOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n{\none.blah();\n}\nsmalltalkOnly;\n}\n", java);
	}

	public void testStrcmp() {
		String smalltalk = "test\nString strcmp: 'hi there' with: 'here'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n\"hi there\".compareTo(\"here\");\n}\n", java);
	}

	public void testStrlen() {
		String smalltalk = "test\nString strlen: 'hi there'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n\"hi there\".length();\n}\n", java);
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

	public void testStringDeclaration2() {
		String smalltalk = "test: string {char star}\nstring!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test(String string) {\nstring;\n}\n", java);
	}

	public void testStringWrapped() {
		String smalltalk = "test\n'hi there\nand here\n'!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n\"hi there\\n\"+\n\"and here\\n\"+\n\"\";\n}\n", java);
	}

	public void testStatementTerminationEmpty() {
		String smalltalk = "test\n!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n}\n", java);
	}

	public void testStatementTerminationTrailingComment() {
		String smalltalk = "test\nself blah \"hello\"!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nblah();\n/* hello */\n}\n", java);
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
	public void testSymbolWithColons() {
		String smalltalk = "test\n#IDSpace.U.newIDs.U.N2:with:!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nIDSPACE_UNEW_IDS_UN2_WITH_;\n}\n", java);
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

	public void testToDo() {
		String smalltalk = "test\n1 to: fred happy do: [:i {UInt32} | blah ]!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\nfor (int i = 1 ; i <= fred.happy() ; i ++ ) {\nblah;\n}\n}\n", java);
	}

	public void testTranslateOnlyString() {
		String smalltalk = "test\n'hello there' translateOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n/* translateOnly \"hello there\" */\n}\n", java);
	}

	public void testTranslateOnly() {
		String smalltalk = "translateOnly\n[one blah] translateOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void translateOnly() {\none.blah();\n}\n", java);
	}

	public void testTranslateOnlyMixed() {
		String smalltalk = "translateOnly\n[one blah] translateOnly. [two blah] smalltalkOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void translateOnly() {\none.blah();\n}\n", java);
	}

	public void testTranslateOnlySimpleTranlation() {
		String smalltalk = "test\n[one blah] translateOnly!";

		String java = writeInstanceMethod(smalltalk);

		assertEquals("public void test() {\n{\none.blah();\n}\ntranslateOnly;\n}\n", java);
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

	public void testUnimplemented() {
		String smalltalk = "test\nself unimplemented!";
		
		String java = writeInstanceMethod(smalltalk);
		
		assertEquals("public void test() {\nthrow new UnimplementedException();\n}\n", java);
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