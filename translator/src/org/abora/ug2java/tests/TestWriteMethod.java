/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.tests;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.abora.ug2java.Annotation;
import org.abora.ug2java.ClassParser;
import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaCodebase;
import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.stscanner.ChunkDetails;
import org.abora.ug2java.transform.type.ClassTransformer;
import org.abora.ug2java.transform.type.ClassTransformers;
import org.abora.ug2java.writer.ClassWriter;

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
		JavaCodebase javaCodebase = new JavaCodebase();
		javaCodebase.packageLookup.put("Heaper", "org.abora.gold.xpp.basic");
		javaClass = new JavaClass("Test", javaCodebase);
		classWriter = new ClassWriter(javaClass);
		classWriter.quoteSmalltalk = false;
		classWriter.shouldIndent = false;
	}

	public static Test suite() {
		return new TestSuite(THIS);
	}

	public void testAbs() {
		String smalltalk = "test\nblah abs!";

		String expectedJava = "public void test() {\nMath.abs(blah);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAnd() {
		String smalltalk = "test\none and: [two]!";
		String expectedJava = "public void test() {\none && (two);\n}\n";

		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAndAnd() {
		String smalltalk = "test\none and: [two and: [three]]!";

		String expectedJava = "public void test() {\none && (two && (three));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAlmostToDo() {
		String smalltalk = "test\n0 almostTo: fred happy do: [:i {UInt32} | blah ]!";

		String expectedJava = "public void test() {\nfor (int i = 0; i < fred.happy(); i ++ ) {\nblah;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAlmostToDoNoStepType() {
		String smalltalk = "test\n0 almostTo: fred happy do: [:i | blah ]!";

		String expectedJava = "public void test() {\nfor (int i = 0; i < fred.happy(); i ++ ) {\nblah;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAlmostToDoAfterIf() {
		String smalltalk = "test\nmyValue == NULL ifTrue: [^VOID]. 0 almostTo: fred happy do: [:i {UInt32} | blah ]!";

		String expectedJava = "public void test() {\nif (myValue == null) {\nreturn ;\n}\nfor (int i = 0; i < fred.happy(); i ++ ) {\nblah;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAlmostToByDoPositive() {
		String smalltalk = "test\n0 almostTo: fred happy by: 2 do: [:i {UInt32} | blah ]!";

		String expectedJava = "public void test() {\nfor (int i = 0; i < fred.happy(); i += 2 ) {\nblah;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAlmostToByDoNegative() {
		String smalltalk = "test\n0 almostTo: fred happy by: -2 do: [:i {UInt32} | blah ]!";

		String expectedJava = "public void test() {\nfor (int i = 0; i > fred.happy(); i -= 2 ) {\nblah;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	
	public void testAsDouble() {
		String smalltalk = "test\nself fred asDouble!";

		String expectedJava = "public void test() {\n(double) fred();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAsFloat() {
		String smalltalk = "test\n^fred asFloat!";

		String expectedJava = "public void test() {\nreturn (float) fred;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAssert() {
		String smalltalk = "test\n(fred < 1) assert!";

		String expectedJava = "public void test() {\nif (fred < 1) {\nthrow new AboraAssertionException();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAssertWithMessage() {
		String smalltalk = "test\nfred < 1 assert: 'hello'!";

		String expectedJava = "public void test() {\nif (fred < 1) {\nthrow new AboraAssertionException(\"hello\");\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAssertSufficientBrackets() {
		String smalltalk = "test\n(a + b) < (c + d) assert: 'hello'!";

		String expectedJava = "public void test() {\nif ((a + b) < (c + d)) {\nthrow new AboraAssertionException(\"hello\");\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAssign() {
		String smalltalk = "test\nfred _ 1.\nharry :=2!";

		String expectedJava = "public void test() {\nfred = 1;\nharry = 2;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAssignUnaryOperator() {
		String smalltalk = "test\nfred := harry kill!";

		String expectedJava = "public void test() {\nfred = harry.kill();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAtIfAbsent() {
		String smalltalk = "test\nDismantleStatistics at: pos ifAbsent: [0]!";

		String expectedJava = "public void test() {\nDismantleStatistics.ifAbsent(pos, 0);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAtIfAbsentEmptyBlock() {
		String smalltalk = "test\nDismantleStatistics at: pos ifAbsent: []!";

		String expectedJava = "public void test() {\nDismantleStatistics.ifAbsent(pos, null);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAtMethod() {
		String smalltalk = "atBlahGo\n!";

		String expectedJava = "public void blahGo() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAtStore() {
		String smalltalk = "test\na < b ifTrue: [table at: 1 store: NULL. a := a + 1]!";

		String expectedJava = "public void test() {\nif (a < b) {\ntable.store(1, null);\na = a + 1;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testAtStoreValue() {
		String smalltalk = "test\ntable at: 1 storeValue: value!";

		String expectedJava = "public void test() {\ntable.storeValue(1, value);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBasicCastWithStar() {
		String smalltalk = "test\n^myTrace basicCast: Heaper star!";

		String expectedJava = "public void test() {\nreturn (Heaper) myTrace;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBinaryOperator() {
		String smalltalk = "test\none < two!";

		String expectedJava = "public void test() {\none < two;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBinaryOperatorEquals() {
		String smalltalk = "test\none = two!";

		String expectedJava = "public void test() {\none == two;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBinaryOperatorIdentity() {
		String smalltalk = "test\none == two!";

		String expectedJava = "public void test() {\none == two;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBinaryOperatorNotEquals() {
		String smalltalk = "test\none ~= two!";

		String expectedJava = "public void test() {\none != two;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBinaryOperatorNotIdentity() {
		String smalltalk = "test\none ~~ two!";

		String expectedJava = "public void test() {\none != two;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBinaryOperatorWithUnary() {
		String smalltalk = "test\none and < two three!";

		String expectedJava = "public void test() {\none.and() < two.three();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBitAnd() {
		String smalltalk = "test\none bitAnd: 7!";

		String expectedJava = "public void test() {\none & 7;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBitInvert() {
		String smalltalk = "test\none bitInvert!";

		String expectedJava = "public void test() {\n~ one;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBitInvertExpression() {
		String smalltalk = "test\n(flags bitAnd: (a bitOr: b) bitInvert) ~~ 0 ifTrue: [^1]!";

		String expectedJava = "public void test() {\nif ((flags & ~ (a | b)) != 0) {\nreturn 1;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBitOr() {
		String smalltalk = "test\none bitOr: 7!";

		String expectedJava = "public void test() {\none | 7;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBitXor() {
		String smalltalk = "test\none bitXor: 7!";

		String expectedJava = "public void test() {\none ^ 7;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBitShift() {
		String smalltalk = "test\none bitShift: 7!";

		String expectedJava = "public void test() {\none << 7;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBitShiftRight() {
		String smalltalk = "test\none bitShiftRight: 7!";

		String expectedJava = "public void test() {\none >> 7;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBlast() {
		String smalltalk = "test\nHeaper BLAST: #TestBlah!";

		String expectedJava = "public void test() {\nthrow new AboraRuntimeException(AboraRuntimeException.TEST_BLAH);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testBlastSelf() {
		String smalltalk = "test\nself BLAST: #OutOfBounds!";

		String expectedJava = "public void test() {\nthrow new AboraRuntimeException(AboraRuntimeException.OUT_OF_BOUNDS);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBlock() {
		String smalltalk = "test\n[one two]!";

		String expectedJava = "public void test() {\n{\none.two();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBlockArguments() {
		String smalltalk = "test\n[:a :b {T} |one two]!";

		String expectedJava = "public void test() {\n{\nObject a;\nT b;\none.two();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBlockEmpty() {
		String smalltalk = "test\n[]!";

		String expectedJava = "public void test() {\n{\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBlockTemps() {
		String smalltalk = "test\n[|:a :b {T} |one two]!";

		String expectedJava = "public void test() {\n{\nObject a;\nT b;\none.two();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBlockTempsDuplicateInSubsequentBlock() {
		String smalltalk = "test\n[|:a :b {T} |one two]. [| :b {A} | b blah]!";

		String expectedJava = "public void test() {\n{\nObject a;\nT b;\none.two();\n}\n{\nA b1;\nb1.blah();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBlockReturn() {
		String smalltalk = "test\n^[self release]!!";

		String expectedJava = "public void test() {\nrelease();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBrackets() {
		String smalltalk = "test\nfred := (one two)!";

		String expectedJava = "public void test() {\nfred = (one.two());\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBracketsEmbedded() {
		String smalltalk = "test\nfred := ((one two))!";

		String expectedJava = "public void test() {\nfred = ((one.two()));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testBracketsFollowing() {
		String smalltalk = "test\nfred := (one two) three!";

		String expectedJava = "public void test() {\nfred = (one.two()).three();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCascade() {
		//TODO probably not good enough
		String smalltalk = "test\n	Transcript show: 'table printing:'; cr!";

		String expectedJava = "public void test() {\nAboraSupport.logger.print(\"table printing:\");\nAboraSupport.logger.println();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCast() {
		String smalltalk = "test\n^blah cast: Peter!";

		String expectedJava = "public void test() {\nreturn (Peter) blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCastCall() {
		String smalltalk = "test\n^self happy cast: Peter!";

		String expectedJava = "public void test() {\nreturn (Peter) happy();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testCastCheck() {
		String smalltalk = "test\na cast: CoordinateSpace!";

		String expectedJava = "public void test() {\nCoordinateSpace aCast = (CoordinateSpace) a;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	


	public void testCastShouldOverrideType() {
		String smalltalk = "test\n^blah cast: UInt32!";

		String expectedJava = "public void test() {\nreturn (int) blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCastMore() {
		String smalltalk = "test\napple _ (blah able: george) cast: Peter!";

		String expectedJava = "public void test() {\napple = (Peter) (blah.able(george));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCastInto() {
		String smalltalk = "test\nblah cast: Pair into: [:pair | pair left]!";

		String expectedJava = "public void test() {\nif (blah instanceof Pair) {\nPair pair = (Pair) blah;\npair.left();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void XtestCastIntoMultipleTypes() {
		String smalltalk = "test\nblah cast: (Pair | Dsp) into: [:pair | pair left]!";

		String expectedJava = "public void test() {\nif (blah instanceof Pair) {\nPair pair = (Pair) blah;\npair.left();\n} else if (blah instanceof Dsp) {\nDsp pair = (Dsp) pair;\npair.left();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCastIntoOthers() {
		String smalltalk = "test\nblah cast: Pair into: [:pair | pair left] others: [1]!";

		String expectedJava = "public void test() {\nif (blah instanceof Pair) {\nPair pair = (Pair) blah;\npair.left();\n}\nelse {\n1;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCastIntoCastIntoOthers() {
		String smalltalk = "test\nblah cast: Pair into: [:pair | pair left] cast: Region into: [:region | region size] others: [1]!";

		String expectedJava = "public void test() {\nif (blah instanceof Pair) {\nPair pair = (Pair) blah;\npair.left();\n}\nelse if (blah instanceof Region) {\nRegion region = (Region) blah;\nregion.size();\n}\nelse {\n1;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCastIntoCastInto() {
		String smalltalk = "test\nblah cast: Pair into: [:pair | pair left] cast: Region into: [:region | region size]!";

		String expectedJava = "public void test() {\nif (blah instanceof Pair) {\nPair pair = (Pair) blah;\npair.left();\n}\nelse if (blah instanceof Region) {\nRegion region = (Region) blah;\nregion.size();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCastExpressionIntoOthers() {
		String smalltalk = "test\none blah cast: Pair into: [:pair | pair left] others: [1]!";

		String expectedJava = "public void test() {\nHeaper cast1 = one.blah();\nif (cast1 instanceof Pair) {\nPair pair = (Pair) cast1;\npair.left();\n}\nelse {\n1;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCategoryName() {
		String smalltalk = "test\n^self getCategory name!";

		String expectedJava = "public void test() {\nreturn getClass().getName();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCAThashForEquals() {
		String smalltalk = "test\n^#cat.U.Test hashForEqual!";

		String expectedJava = "public void test() {\nreturn HashHelper.hashForEqual(this.getClass());\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCerr() {
		String smalltalk = "test\ncerr << 'hello'!";

		String expectedJava = "public void test() {\nAboraSupport.logger.print(\"hello\");\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCharacterLiteral() {
		String smalltalk = "test\n$a. $-. $\\. $'!";

		String expectedJava = "public void test() {\n'a';\n'-';\n'\\\\';\n'\\\'';\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCharacterTab() {
		String smalltalk = "test\nCharacter tab!";

		String expectedJava = "public void test() {\nAboraCharacterSupport.tab();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testClassCall() {
		String smalltalk = "test\nHeaper blah!";

		String expectedJava = "public void test() {\nHeaper.blah();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testClassSelfCall() {
		String smalltalk = "test\nself class blah!";

		String expectedJava = "public void test() {\nTest.blah();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testClassCall2() {
		String smalltalk = "test\ninfo class == Heaper!";

		String expectedJava = "public void test() {\nAboraSupport.findCategory(info.getClass()) == AboraSupport.findCategory(Heaper.class);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	

	public void testClassNonReference() {
		String smalltalk = "test\nNonClass!";

		String expectedJava = "public void test() {\nNonClass;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testClassReference() {
		String smalltalk = "test\nHeaper!";

		String expectedJava = "public void test() {\nAboraSupport.findCategory(Heaper.class);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testClassReference2() {
		String smalltalk = "test\ntable at: 1 storeValue: Heaper)!";

		String expectedJava = "public void test() {\ntable.storeValue(1, AboraSupport.findCategory(Heaper.class));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCloseSocket() {
		String smalltalk = "test\n| fd {Int32}| fd close!";

		String expectedJava = "public void test() {\nint fd;\nAboraSocketSupport.close(fd);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testComment() {
		String smalltalk = "test\nfred := 1\n\"Hello There\"!";

		String expectedJava = "public void test() {\nfred = 1;\n/* Hello There */\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCommentWithEmbeddedLineComment() {
		String smalltalk = "test\nfred := 1\n\"Hello /* here */ There\"!";

		String expectedJava = "public void test() {\nfred = 1;\n// Hello /* here */ There\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCompileFodder() {
		String smalltalk = "test\na ifTrue: [^1] ifFalse: [^2].^false \"compiler fodder\"!";

		String expectedJava = "public void test() {\nif (a) {\nreturn 1;\n}\nelse {\nreturn 2;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCompileFodder2() {
		String smalltalk = "test\na ifTrue: [^1] ifFalse: [^2].^false \"fodder\"!";

		String expectedJava = "public void test() {\nif (a) {\nreturn 1;\n}\nelse {\nreturn 2;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCompileFodderNonElse() {
		String smalltalk = "test\na ifTrue: [^1].^false \"compiler fodder\"!";

		String expectedJava = "public void test() {\nif (a) {\nreturn 1;\n}\nreturn false;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testConditionalOperator() {
		String smalltalk = "test\n^ (one = two) ifTrue: [one] ifFalse: [two]!";

		String expectedJava = "public void test() {\nreturn (one == two) ? one : two;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testConditionalOperatorEmbedded() {
		String smalltalk = "test\nself blah: ((one = two) ifTrue: [one] ifFalse: [two])!";

		String expectedJava = "public void test() {\nblah(((one == two) ? one : two));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDiskManagerConsistentDefaultDirty() {
		String smalltalk = "test\nDiskManager consistent: [self diskUpdate]!";

		String expectedJava = "public void test() {\nAboraBlockSupport.enterConsistent();\ntry {\ndiskUpdate();\n}\nfinally {\nAboraBlockSupport.exitConsistent();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDiskManagerConsistentDefaultDirtyWithInterveningComment() {
		String smalltalk = "test\nDiskManager consistent: \"comment\" [self diskUpdate]!";

		String expectedJava = "public void test() {\nAboraBlockSupport.enterConsistent(\n/* comment */\n);\ntry {\ndiskUpdate();\n}\nfinally {\nAboraBlockSupport.exitConsistent();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDiskManagerConsistentWithIdentifier() {
		String smalltalk = "test\nDiskManager consistent: 2 with: [self diskUpdate]!";

		String expectedJava = "public void test() {\nAboraBlockSupport.enterConsistent(2);\ntry {\ndiskUpdate();\n}\nfinally {\nAboraBlockSupport.exitConsistent();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDiskManagerConsistentWithExpression() {
		String smalltalk = "test\nDiskManager consistent: self dirtyLevel with: [self diskUpdate]!";

		String expectedJava = "public void test() {\nAboraBlockSupport.enterConsistent(dirtyLevel());\ntry {\ndiskUpdate();\n}\nfinally {\nAboraBlockSupport.exitConsistent();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDiskManagerConsistentWithExpression2() {
		String smalltalk = "test\nCurrentPacker fluidGet consistent: self dirtyLevel with: [self diskUpdate]!";

		String expectedJava = "public void test() {\nDiskManager diskManager1 = ((DiskManager) CurrentPacker.fluidGet());\nAboraBlockSupport.enterConsistent(dirtyLevel(), diskManager1);\ntry {\ndiskUpdate();\n}\nfinally {\nAboraBlockSupport.exitConsistent(diskManager1);\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDiskManagerInsistentWithExpression() {
		String smalltalk = "test\nDiskManager insistent: self dirtyLevel with: [self diskUpdate]!";

		String expectedJava = "public void test() {\nAboraBlockSupport.enterInsistent(dirtyLevel());\ntry {\ndiskUpdate();\n}\nfinally {\nAboraBlockSupport.exitInsistent();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCreateAfter() {
		String smalltalk = "createAfter: trace\n^1!";

		String expectedJava = "public void createAfter(Object trace) {\nreturn 1;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCreateCall() {
		String smalltalk = "test\nBlah create: 12!";

		String expectedJava = "public void test() {\nnew Blah(12);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCreateCallAfter() {
		String smalltalk = "test\nmyBranch createAfter: self!";

		String expectedJava = "public void test() {\nmyBranch.createAfter(this);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testCreateCallSelf() {
		String smalltalk = "test\nself create: 12!";

		String expectedJava = "public void test() {\nnew Test(12);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCreateSuper() {
		String smalltalk = "create: blah\nsuper create: blah!";

		String expectedJava = "public Test(Object blah) {\nsuper(blah);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCreateSuperWithDeclarations() {
		String smalltalk = "create\n| blah |\nsuper create: 2. blah := 1!";

		String expectedJava = "public Test() {\nsuper(2);\nObject blah;\nblah = 1;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testCreateSuperWithExtendedName() {
		String smalltalk = "create: size {Int32} with.Executor: exec {XnExecutor}\nsuper create!";

		String expectedJava = "public Test(int size, XnExecutor exec) {\nsuper();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testCritical() {
		String smalltalk = "test\nmutex critical: [blah]!";

		String expectedJava = "public void test() {\nsynchronized (mutex) {\nblah;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDefineFluidWithBooleanLiteral() {
		String smalltalk = "test\nMuSet defineFluid: #InsideTransactionFlag with: DiskManager emulsion with: [false]!";

		String expectedJava = "public void test() {\nAboraSupport.defineFluid(MuSet.class, \"InsideTransactionFlag\", DiskManager.emulsion(), Boolean.FALSE);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDefineFluid() {
		String smalltalk = "test\nMuSet defineFluid: #ActiveClubs with: DiskManager emulsion with: [MuSet make]!";

		String expectedJava = "public void test() {\nAboraSupport.defineFluid(MuSet.class, \"ActiveClubs\", DiskManager.emulsion(), MuSet.make());\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDeleteString() {
		String smalltalk = "test\n| s {char star}| s delete. x delete!";

		String expectedJava = "public void test() {\nString s;\n/* Removed s.delete(); */\nx.delete();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDiv() {
		String smalltalk = "test\n11 // 2!";

		String expectedJava = "public void test() {\n11 / 2;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDOTasLong() {
		String smalltalk = "test\n12 DOTasLong!";

		String expectedJava = "public void test() {\n12;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDOTasInt() {
		String smalltalk = "test\n12 DOTasInt!";

		String expectedJava = "public void test() {\n12;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDOTasInt32() {
		String smalltalk = "test\n12 DOTasInt32!";

		String expectedJava = "public void test() {\n12;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDOTasUInt32() {
		String smalltalk = "test\n12 DOTasUInt32!";

		String expectedJava = "public void test() {\n12;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDOhashForEqual() {
		String smalltalk = "test\nblah DOThashForEqual!";

		String expectedJava = "public void test() {\nHashHelper.hashForEqual(blah);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testDOTputCharacter() {
		String smalltalk = "test\noo DOTput: ((myBuffer uIntAt: j) basicCast: Character)!";

		String expectedJava = "public void test() {\noo.print(((myBuffer.uIntAt(j))));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	

	public void testDouble() {
		String smalltalk = "test\n187.123!";

		String expectedJava = "public void test() {\n187.123;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDowncastArgument() {
		new JavaClass("A1", "Heaper", javaClass.getJavaCodebase());
		new JavaClass("A2", "A1", javaClass.getJavaCodebase());

		JavaMethod method = new JavaMethod("", "make");
		method.addParameter(new JavaField("static", "A2", "arg1"));
		method.methodBody = new MethodBody(new ArrayList());
		javaClass.addMethod(method);
		
		String smalltalk = "test\n| a1 {A1} | ^ self make: a1!";

		String expectedJava = "public static void test() {\nA1 a1;\nreturn make((A2) a1);\n}\n";
		
		String actualJava = writeMethod(smalltalk, "static ");
		assertMethodBodyEquals(expectedJava, actualJava);
	}

	public void testDowncastArgument2() {
		new JavaClass("A1", "Heaper", javaClass.getJavaCodebase());
		new JavaClass("A2", "A1", javaClass.getJavaCodebase());
		new JavaClass("A3", "A1", javaClass.getJavaCodebase());

		JavaMethod method = new JavaMethod("", "make");
		method.addParameter(new JavaField("static", "A2", "arg1"));
		method.addParameter(new JavaField("static", "A3", "arg2"));
		method.methodBody = new MethodBody(new ArrayList());
		javaClass.addMethod(method);
		
		String smalltalk = "test\n| a1 {A1} a11 {A1}| ^ self make: a1 with: a11!";

		String expectedJava = "public static void test() {\nA1 a1;\nA1 a11;\nreturn make((A2) a1, (A3) a11);\n}\n";
		
		String actualJava = writeMethod(smalltalk, "static ");
		assertMethodBodyEquals(expectedJava, actualJava);
	}
	
	public void testDowncastArgument2WithCast() {
		new JavaClass("A1", "Heaper", javaClass.getJavaCodebase());
		new JavaClass("A2", "A1", javaClass.getJavaCodebase());
		new JavaClass("A3", "A1", javaClass.getJavaCodebase());

		JavaMethod method = new JavaMethod("", "make");
		method.addParameter(new JavaField("static", "A2", "arg1"));
		method.addParameter(new JavaField("static", "A3", "arg2"));
		method.methodBody = new MethodBody(new ArrayList());
		javaClass.addMethod(method);
		
		String smalltalk = "test\n| a1 {A1} a11 {A1}| ^ self make: (a1 cast: A2) with: a11!";

		String expectedJava = "public static void test() {\nA1 a1;\nA1 a11;\nreturn make(((A2) a1), (A3) a11);\n}\n";
		
		String actualJava = writeMethod(smalltalk, "static ");
		assertMethodBodyEquals(expectedJava, actualJava);
	}

	public void testDowncastStaticCallAssignmentSubclass() {
		new JavaClass("A1", "Heaper", javaClass.getJavaCodebase());
		new JavaClass("A2", "A1", javaClass.getJavaCodebase());
		
		JavaClass m = new JavaClass("M", "Heaper", javaClass.getJavaCodebase());
		m.addMethod(new JavaMethod("A2", "make"));
		
		String smalltalk = "test\n| a1 {A1} | a1 := M make!";

		String expectedJava = "public void test() {\nA1 a1;\na1 = M.make();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDowncastStaticCallAssignmentInconsistentReturnType() {
		new JavaClass("A1", "Heaper", javaClass.getJavaCodebase());
		new JavaClass("A2", "A1", javaClass.getJavaCodebase());
		
		JavaClass m = new JavaClass("M", "Heaper", javaClass.getJavaCodebase());
		m.addMethod(new JavaMethod("A1", "make"));
		m.addMethod(new JavaMethod("Heaper", "make"));
		
		String smalltalk = "test\n| a2 {A2} | a2 := M make!";

		String expectedJava = "public void test() {\nA2 a2;\na2 = M.make();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDowncastStaticCallAssignment() {
		new JavaClass("A1", "Heaper", javaClass.getJavaCodebase());
		new JavaClass("A2", "A1", javaClass.getJavaCodebase());

		JavaClass m = new JavaClass("M", "Heaper", javaClass.getJavaCodebase());
		m.addMethod(new JavaMethod("A1", "make"));
		
		String smalltalk = "test\n| a2 {A2} | a2 := M make!";

		String expectedJava = "public void test() {\nA2 a2;\na2 = (A2) M.make();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDowncastStaticCallAssignmentSameType() {
		new JavaClass("A1", "Heaper", javaClass.getJavaCodebase());
		new JavaClass("A2", "A1", javaClass.getJavaCodebase());
		
		JavaClass m = new JavaClass("M", "Heaper", javaClass.getJavaCodebase());
		m.addMethod(new JavaMethod("A2", "make"));
		
		String smalltalk = "test\n| a2 {A2} | a2 := M make!";

		String expectedJava = "public void test() {\nA2 a2;\na2 = M.make();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testDownToDo() {
		String smalltalk = "test\n10 downTo: fred happy do: [:i {UInt32} | blah ]!";

		String expectedJava = "public void test() {\nfor (int i = 10; i >= fred.happy(); i -= 1 ) {\nblah;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testEmpty() {
		String smalltalk = "test!";

		String expectedJava = "public void test() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testFluidBindDuring() {
		String smalltalk = "test\nCurrentTrace fluidBind: myEnt newTrace during: [result := BeClub make: desc]!";

		String expectedJava = "public void test() {\nObject currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());\ntry {\nresult = BeClub.make(desc);\n}\nfinally {\nAboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testFluidBindDuringWithComment() {
		String smalltalk = "test\nCurrentTrace fluidBind: myEnt newTrace during: \"comment\" [result := BeClub make: desc]!";

		String expectedJava = "public void test() {\nObject currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, myEnt.newTrace());\ntry \n/* comment */\n{\nresult = BeClub.make(desc);\n}\nfinally {\nAboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testFluidFetch() {
		String smalltalk = "test\nCurrentPacker fluidFetch blah!";

		String expectedJava = "public void test() {\n((DiskManager) CurrentPacker.fluidFetch()).blah();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testFluidFetchInsideTransactionFlag() {
		String smalltalk = "test\nInsideTransactionFlag fluidFetch ifTrue: [self blah]!";

		String expectedJava = "public void test() {\nif (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue()) {\nblah();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testForceReturn() {
		String smalltalk = "testForceReturn\nself fred!";

		String expectedJava = "public void testForceReturn() {\nreturn fred();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testForEach() {
		String smalltalk = "test\nfred forEach: [:element {IntegerPos}| element]!";

		String expectedJava = "public void test() {\nfor (Stepper stomp1 = fred; stomp1.hasValue(); stomp1.step()) {\nIntegerPos element = (IntegerPos) stomp1.fetch();\nelement;\n}\n}\n";
		/*
		 * "public void test() {\nfor (Stepper stepper = fred ;
		 * stepper.hasValue() ; stepper.step()) {\nIntegerPos element =
		 * (IntegerPos )stepper.fetch();\nelement;\n}\nstepper.destroy();\n}\n",
		 */
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testForEachHeaper() {
		String smalltalk = "test\nfred forEach: [:element | element]!";

		String expectedJava = "public void test() {\nfor (Stepper stomp1 = fred; stomp1.hasValue(); stomp1.step()) {\nHeaper element = stomp1.fetch();\nelement;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testForEachNested() {
		String smalltalk = "test\nfred forEach: [:element {IntegerPos}| blah forEach: [:element2 {RealPos} | element + element2]]!";

		String expectedJava = "public void test() {\nfor (Stepper stomp1 = fred; stomp1.hasValue(); stomp1.step()) {\nIntegerPos element = (IntegerPos) stomp1.fetch();\nfor (Stepper stomp2 = blah; stomp2.hasValue(); stomp2.step()) {\nRealPos element2 = (RealPos) stomp2.fetch();\nelement + element2;\n}\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testForIndices() {
		String smalltalk = "test\nfred forIndices: [:i {IntegerVar} :value {IntegerRegion}| element]!";

		String expectedJava = "public void test() {\nfor (TableStepper stomp1 = fred; stomp1.hasValue(); stomp1.step()) {\nint i = (int) stomp1.index();\nIntegerRegion value = (IntegerRegion) stomp1.fetch();\nelement;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testForPositions() {
		String smalltalk = "test\nfred forPositions: [:key {IntegerPos} :value {IntegerRegion}| element]!";

		String expectedJava = "public void test() {\nfor (TableStepper stomp1 = fred; stomp1.hasValue(); stomp1.step()) {\nIntegerPos key = (IntegerPos) stomp1.position();\nIntegerRegion value = (IntegerRegion) stomp1.fetch();\nelement;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testGetStatic() {
		String smalltalk = "getCategory\n!";

		String expectedJava = "public static void getCategory() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testHandleDo() {
		JavaMethod problemsMethod = new JavaMethod("", "problemsBlah");
		problemsMethod.methodBody = new MethodBody(new ArrayList());
		Set problems = new HashSet();
		problems.add("BLAH");
		problemsMethod.getAnnotations().put(Annotation.PROBLEM_SIGNALS, problems);
		javaClass.addMethod(problemsMethod);
		
		String smalltalk = "test\nTest problems.Blah handle: [:ex | ^false] do: [self blahblah]!";

		String expectedJava = "public void test() {\ntry {\nblahblah();\n}\ncatch (AboraRuntimeException ex) {\nif (AboraRuntimeException.BLAH.equals(ex.getMessage())) {\nreturn false;\n}\nelse {\nthrow ex;\n}\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testHandleDoManyProblems() {
		JavaMethod problemsMethod = new JavaMethod("", "problemsBlah");
		problemsMethod.methodBody = new MethodBody(new ArrayList());
		Set problems = new TreeSet();
		problems.add("BLAH");
		problems.add("NOT_IN_SET");
		problemsMethod.getAnnotations().put(Annotation.PROBLEM_SIGNALS, problems);
		javaClass.addMethod(problemsMethod);
		
		String smalltalk = "test\nTest problems.Blah handle: [:ex | ^false] do: [self blahblah]!";

		String expectedJava = "public void test() {\ntry {\nblahblah();\n}\ncatch (AboraRuntimeException ex) {\nif (AboraRuntimeException.BLAH.equals(ex.getMessage()) || AboraRuntimeException.NOT_IN_SET.equals(ex.getMessage())) {\nreturn false;\n}\nelse {\nthrow ex;\n}\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testHandleDoBrackets() {
		JavaMethod problemsMethod = new JavaMethod("", "problemsBlah");
		problemsMethod.methodBody = new MethodBody(new ArrayList());
		Set problems = new HashSet();
		problems.add("BLAH");
		problemsMethod.getAnnotations().put(Annotation.PROBLEM_SIGNALS, problems);
		javaClass.addMethod(problemsMethod);
		
		String smalltalk = "test\n(Test problems.Blah) handle: [:ex | ^false] do: [self blahblah]!";

		String expectedJava = "public void test() {\ntry {\nblahblah();\n}\ncatch (AboraRuntimeException ex) {\nif (AboraRuntimeException.BLAH.equals(ex.getMessage())) {\nreturn false;\n}\nelse {\nthrow ex;\n}\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testHandleDoAllBlasts() {		
		String smalltalk = "test\n(Heaper problems.AllBlasts) handle: [:ex | ^false] do: [self blahblah]!";

		String expectedJava = "public void test() {\ntry {\nblahblah();\n}\ncatch (AboraRuntimeException ex) {\nreturn false;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIfTrue() {
		String smalltalk = "test\n(one = two) ifTrue: [^one]!";

		String expectedJava = "public void test() {\nif (one == two) {\nreturn one;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIfTrueIfFalse() {
		String smalltalk = "test\n(one = two) ifTrue: [^one] ifFalse: [^two]!";

		String expectedJava = "public void test() {\nif (one == two) {\nreturn one;\n}\nelse {\nreturn two;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIfTrueIfFalseWithCommentShouldNotBeInterpretedAsConditional() {
		String smalltalk = "test\n\"one\" \"two\" (one = two) ifTrue: [^one] ifFalse: [^two]!";

		String expectedJava = "/**\n * one\n */\npublic void test() {\n/* two */\nif (one == two) {\nreturn one;\n}\nelse {\nreturn two;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIfTrueLeadingAssignments() {
		String smalltalk = "test\na := b := (one = two) ifTrue: [one]!";

		String expectedJava = "public void test() {\na = b = if (one == two) {\none;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIfTrueLeadingReturn() {
		String smalltalk = "test\n^(one = two) ifTrue: [one]!";

		String expectedJava = "public void test() {\nreturn if (one == two) {\none;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIfTrueMissingTestParentheses() {
		String smalltalk = "test\none = two ifTrue: [^one]!";

		String expectedJava = "public void test() {\nif (one == two) {\nreturn one;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIfTrueMissingTestParenthesesComplicated() {
		String smalltalk = "test\none = (two + 2) ifTrue: [^one]!";

		String expectedJava = "public void test() {\nif (one == (two + 2)) {\nreturn one;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIfTrueMissingTestParenthesesMoreComplicated() {
		String smalltalk = "test\n(a blah: b) ~~ 98 ifTrue: [^one]!";

		String expectedJava = "public void test() {\nif ((a.blah(b)) != 98) {\nreturn one;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIfTrueMissingTestParenthesesPaired() {
		String smalltalk = "test\n(a blah: b) ~~ (a fred: b) ifTrue: [^one]!";

		String expectedJava = "public void test() {\nif ((a.blah(b)) != (a.fred(b))) {\nreturn one;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testInitializeLocalVariable() {
		String smalltalk = "test\n| shouldInitialize { Heaper }| ^shouldInitialize!";

		String expectedJava = "public void test() {\n/* TODO variable may not be initialized before being used */\nHeaper shouldInitialize = null;\nreturn shouldInitialize;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testInitializeLocalVariableInt() {
		String smalltalk = "test\n| shouldInitialize { Integer }| ^shouldInitialize!";

		String expectedJava = "public void test() {\n/* TODO variable may not be initialized before being used */\nint shouldInitialize = 0;\nreturn shouldInitialize;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testInt32Min() {
		String smalltalk = "test\nInt32Min + 2!";

		String expectedJava = "public void test() {\n0x80000000 + 2;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testInt32Zero() {
		String smalltalk = "test\nInt32Zero + 2!";

		String expectedJava = "public void test() {\n0 + 2;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testInteger() {
		String smalltalk = "test\n187!";

		String expectedJava = "public void test() {\n187;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerCallOnIntVar() {
		String smalltalk = "test: blah {IntegerVar}\nblah integer!";

		String expectedJava = "public void test(int blah) {\nIntegerPos.make(blah);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerCallExpression() {
		String smalltalk = "test\nresult with: i integer!";

		String expectedJava = "public void test() {\nresult.with(IntegerPos.make(i));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerCallExpression2() {
		String smalltalk = "test\nTuple two: Sequence zero with: -1 integer!";

		String expectedJava = "public void test() {\nTuple.two(Sequence.zero(), IntegerPos.make(-1));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerCallOnIntLiteral() {
		String smalltalk = "test\n27 integer!";

		String expectedJava = "public void test() {\nIntegerPos.make(27);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerHex() {
		String smalltalk = "test\n16rA1!";

		String expectedJava = "public void test() {\n0xa1;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerLong() {
		String smalltalk = "test\n12345678901!";

		String expectedJava = "public void test() {\n12345678901L;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerNegative() {
		String smalltalk = "test\n-187!";

		String expectedJava = "public void test() {\n-187;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerVar0() {
		String smalltalk = "test\nblah _ IntegerVar0!";

		String expectedJava = "public void test() {\nblah = 0;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerIntegerVar0() {
		String smalltalk = "test\nblah _ Integer IntegerVar: 0!";

		String expectedJava = "public void test() {\nblah = 0;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerIntegerVar() {
		String smalltalk = "test\nblah _ Integer IntegerVar: tally!";

		String expectedJava = "public void test() {\nblah = tally;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerIntegerVarWithExpression() {
		String smalltalk = "test\n^ Integer IntegerVar: (smallPrimeTable uIntAt: idx)!";

		String expectedJava = "public void test() {\nreturn (smallPrimeTable.uIntAt(idx));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerVarZero() {
		String smalltalk = "test\nblah _ IntegerVarZero!";

		String expectedJava = "public void test() {\nblah = 0;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIntegerZero() {
		String smalltalk = "test\nblah _ IntegerZero!";

		String expectedJava = "public void test() {\nblah = IntegerPos.zero();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testInteger0() {
		String smalltalk = "test\nblah _ Integer0!";

		String expectedJava = "public void test() {\nblah = IntegerPos.zero();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIsKindOf() {
		String smalltalk = "test\nblah isKindOf: Heaper!";

		String expectedJava = "public void test() {\nblah instanceof Heaper;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testIsKindOfCategory() {
		String smalltalk = "test\n| cat {Category}|blah isKindOf: cat!";

		String expectedJava = "public void test() {\nCategory cat;\nblah.isKindOf(cat);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testJavaDocComment() {
		String smalltalk = "test\n\"Hello there\"!";

		String expectedJava = "/**\n * Hello there\n */\npublic void test() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testJavaDocCommentLong() {
		String smalltalk = "test\n\"Hello there, what is the time of day once we all are here and there we go to be. Is this a really long comment or is it simply a lot of nonesense made up on the spot.\"!";

		String expectedJava = "/**\n * Hello there, what is the time of day once we all are here and there we go to be. Is this a\n * really long comment or is it simply a lot of nonesense made up on the spot.\n */\npublic void test() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testKeyword() {
		String smalltalk = "test\none two three: four!";

		String expectedJava = "public void test() {\none.two().three(four);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testKeyword2() {
		String smalltalk = "test\none two three: four and: 55!";

		String expectedJava = "public void test() {\none.two().threeAnd(four, 55);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testKeywordReserved() {
		String smalltalk = "test\n^IDSpace import: arg1!";

		String expectedJava = "public void test() {\nreturn IDSpace.importx(arg1);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	
	public void testKeywordMissingSpace() {
		String smalltalk = "test\nmyConcreteSpecs copyGrow:1!";

		String expectedJava = "public void test() {\nmyConcreteSpecs.copyGrow(1);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testListenFor() {
		String smalltalk = "test\naSocket listenFor: 5!";

		String expectedJava = "public void test() {\nAboraSocketSupport.listenFor(aSocket, 5);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testLocalVarWithReservedName() {
		String smalltalk = "test\n| byte {Int32}| ^byte!";

		String expectedJava = "public void test() {\nint bytex;\nreturn bytex;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testLOG() {
		String smalltalk = "test\nErrorLog LOG: [:ooo | ooo << 'human ' << self]!";

		String expectedJava = "public void test() {\nPrintWriter ooo = ErrorLog;\nooo.print(\"human \");\nooo.print(this);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	       		
	public void testMax() {
		String smalltalk = "test\none max: two!";

		String expectedJava = "public void test() {\nMath.max(one, two);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testMin() {
		String smalltalk = "test\na := 1 min: blah grr!";

		String expectedJava = "public void test() {\na = Math.min(1, blah.grr());\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testModulus() {
		String smalltalk = "test\n11 \\\\ 2!";

		String expectedJava = "public void test() {\n11 % 2;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testMethodNameFullStop() {
		String smalltalk = "test.Extra!";

		String expectedJava = "public void testExtra() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testMethodNameFullStopCreate() {
		String smalltalk = "create.Extra!";

		String expectedJava = "public Test() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testMethodNameFullStopKeyword() {
		String smalltalk = "test\nself blah.Extra: 1!";

		String expectedJava = "public void test() {\nblahExtra(1);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testMethodNameWith() {
		String smalltalk = "make.CoordinateSpace: cs {CoordinateSpace} with.CoordinateSpace: rs {CoordinateSpace}!";

		String expectedJava = "public Test makeCoordinateSpace(CoordinateSpace cs, CoordinateSpace rs) {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testMethodNameCallWith() {
		String smalltalk = "test\nself make.CoordinateSpace: c with.CoordinateSpace: rs!";

		String expectedJava = "public void test() {\nmakeCoordinateSpace(c, rs);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testMultipleStatements() {
		String smalltalk = "test\none two.\nborris := three + 3!";

		String expectedJava = "public void test() {\none.two();\nborris = three + 3;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testMuSetMakeIntegerVar() {
		String smalltalk = "test\nMuSet make: 300!";

		String expectedJava = "public void test() {\nMuSet.makeIntegerVar(300);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNegated() {
		String smalltalk = "test\na size negated!";

		String expectedJava = "public void test() {\n- a.size();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNewAlloc() {
		String smalltalk = "test\n(EmptyStepper new.AllocType: #PERSISTENT) create!";

		String expectedJava = "public void test() {\n/* TODO newAllocType */\nnew EmptyStepper();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNewBecome() {
		String smalltalk = "test\n(SuspendedHeaper new.Become: object) create!";

		String expectedJava = "public void test() {\n/* TODO newBecome */\nnew SuspendedHeaper();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNewBecomeSelf() {
		String smalltalk = "test\n(self new.Become: object) create!";

		String expectedJava = "public void test() {\n/* TODO newBecome */\nnew Test();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNewCall() {
		String smalltalk = "test\nArray new: size!";

		String expectedJava = "public void test() {\nnew Array(size);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNewCreate() {
		String smalltalk = "test\nself new create.Stepper: aStepper!";

		String expectedJava = "public void test() {\nnew Test(aStepper);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNewCreateForClass() {
		String smalltalk = "test\nDiskManagerEmulsion new create!";

		String expectedJava = "public void test() {\nnew DiskManagerEmulsion();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNextPut() {
		String smalltalk = "test\noo nextPut: $a!";

		String expectedJava = "public void test() {\noo.print('a');\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNextPutPrint() {
		String smalltalk = "test\noo nextPut: $-; print: self minHeight!";

		String expectedJava = "public void test() {\noo.print('-');\noo.print(minHeight());\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testNil() {
		String smalltalk = "test\nfred := nil!";

		String expectedJava = "public void test() {\nfred = null;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNotBoolean() {
		String smalltalk = "test: blah {BooleanVar}\n^blah not!";

		String expectedJava = "public void test(boolean blah) {\nreturn ! blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNotExpression() {
		String smalltalk = "test\n^blah isAlive not!";

		String expectedJava = "public void test() {\nreturn ! blah.isAlive();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNotIfFalseExpression() {
		String smalltalk = "test\nflags = (a bitOr: b) ifFalse: [^1]!";

		String expectedJava = "public void test() {\nif ( ! (flags == (a | b))) {\nreturn 1;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNotNilElse() {
		String smalltalk = "test\n(values fetch: i) notNil: [:fe {FeRangeElement} | element _ fe carrier] else: [^nil]!";

		String expectedJava = "public void test() {\nFeRangeElement fe = (FeRangeElement) (values.fetch(i));\nif (fe != null ) {\nelement = fe.carrier();\n}\nelse {\nreturn null;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNotNil() {
		String smalltalk = "test\n(values fetch: i) notNil: [:fe {FeRangeElement} | element _ fe carrier]!";

		String expectedJava = "public void test() {\nFeRangeElement fe = (FeRangeElement) (values.fetch(i));\nif (fe != null ) {\nelement = fe.carrier();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNotNULLElse() {
		String smalltalk = "test\n(values fetch: i) notNULL: [:fe {FeRangeElement} | element _ fe carrier] else: [^nil]!";

		String expectedJava = "public void test() {\nFeRangeElement fe = (FeRangeElement) (values.fetch(i));\nif (fe != null ) {\nelement = fe.carrier();\n}\nelse {\nreturn null;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testNULL() {
		String smalltalk = "test\nfred := NULL!";

		String expectedJava = "public void test() {\nfred = null;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOperatorIntNullIdentity() {
		//Sideffect of converting IntegerVar to int primitive
		String smalltalk = "test\n| a {Int32}| a == nil!";

		String expectedJava = "public void test() {\nint a;\na == 0;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOperatorObjectNullShouldFail() {
		//Sideffect of converting IntegerVar to int primitive
		String smalltalk = "test\n| a | a == nil!";

		String expectedJava = "public void test() {\nObject a;\na == null;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOperatorIntNullNotIdentity() {
		//Sideffect of converting IntegerVar to int primitive
		String smalltalk = "test\n| a {Int32}| a ~~ nil!";

		String expectedJava = "public void test() {\nint a;\na != 0;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOr() {
		String smalltalk = "test\none or: [two]!";

		String expectedJava = "public void test() {\none || (two);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOverrideLocalVarType() {
		String smalltalk = "test\n| shouldOverrideLocalVars | shouldOverrideLocalVars := 23!";

		String expectedJava = "public void test() {\nint shouldOverrideLocalVars;\nshouldOverrideLocalVars = 23;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOverrideReturnType() {
		String smalltalk = "{Blah} isEqual\n^blah!";

		String expectedJava = "public boolean isEqual() {\nreturn blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOverrideReturnType2() {
		String smalltalk = "isEqual\n^blah!";

		String expectedJava = "public boolean isEqual() {\nreturn blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOverrideVoidReturnTypeWithClass() {
		String smalltalk = "{Blah} make\n^blah!";

		String expectedJava = "public Blah make() {\nreturn blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOverrideVoidReturnTypeWithClass2() {
		String smalltalk = "make\n^blah!";

		String expectedJava = "public Test make() {\nreturn blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOverrideVoidReturnType() {
		String smalltalk = "{TableStepper} stepper\n^blah!";

		String expectedJava = "public TableStepper stepper() {\nreturn blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testOverrideVoidReturnType2() {
		String smalltalk = "stepper\n^blah!";

		String expectedJava = "public Stepper stepper() {\nreturn blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testParameterNamedSize() {
		String smalltalk = "test: size {IntegerVar}\nsize > 4 ifTrue: [\nsize] ifFalse: [size - 4]!";

		String expectedJava = "public void test(int size) {\nif (size > 4) {\nsize;\n}\nelse {\nsize - 4;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testParantheses() {
		String smalltalk = "test\n(self extra). [(1 + 2)]. [(1 + 2) + 3]!";

		String expectedJava = "public void test() {\nextra();\n{\n1 + 2;\n}\n{\n(1 + 2) + 3;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	//	create.IntegerVar: size {IntegerVar}

	//	"The optional argument just hints at the number of elements
	//	 to eventually be added. It makes no difference semantically."
	//	| newSize {UInt32} |
	//	super create.
	//	size > 4 ifTrue: [newSize _ size DOTasLong] ifFalse: [newSize _ 4].

	public void testPasse() {
		String smalltalk = "test\nself passe!";

		String expectedJava = "/**\n * @deprecated\n */\npublic void test() {\nthrow new PasseException();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testPasseWithTrailingCode() {
		String smalltalk = "test\nself passe. a ifTrue: [^1] ifFalse: [^2]. ^3!";

		String expectedJava = "/**\n * @deprecated\n */\npublic void test() {\nthrow new PasseException();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

public void testPasseIndentedWithTrailingCode() {
		String smalltalk = "test\na ifTrue: [1 + 2. self passe. a ifTrue: [^1] ifFalse: [^2]]. ^3!";

		String expectedJava = "/**\n * @deprecated\n */\npublic void test() {\nif (a) {\n1 + 2;\nthrow new PasseException();\n}\nreturn 3;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

public void testPointerToStaticMember() {
	String smalltalk = "test\nPromiseManager pointerToStaticMember: #noRequest: with: 'VHFn'!";

	String expectedJava = "public void test() {\n(VHFn) PromiseManager.pointerToStaticMember(\"NO_REQUEST_\", \"VHFn\");\n}\n";
	assertInstanceMethod(expectedJava, smalltalk);
}

	
	public void testPrint() {
		String smalltalk = "test: aStream { ostream }\naStream << self blah: 34!";

		String expectedJava = "public void test(PrintWriter aStream) {\naStream.print(blah(34));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testPrintPrint() {
		String smalltalk = "test: aStream { ostream }\naStream << 12 + 1 << 13 + 2 << 14 + 3!";

		String expectedJava = "public void test(PrintWriter aStream) {\naStream.print(12 + 1);\naStream.print(13 + 2);\naStream.print(14 + 3);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testPrintPrintLiterals() {
		String smalltalk = "test: aStream { ostream }\n'extra'. \"hello\" aStream << 'one' << 'two' << 'three'!";

		String expectedJava = "public void test(PrintWriter aStream) {\n\"extra\";\n/* hello */\naStream.print(\"one\");\naStream.print(\"two\");\naStream.print(\"three\");\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testPrintPrintWithLogger() {
		String smalltalk = "test\ncerr << 12 + 1 << 13 + 2 << 14 + 3!";

		String expectedJava = "public void test() {\nAboraSupport.logger.print(12 + 1);\nAboraSupport.logger.print(13 + 2);\nAboraSupport.logger.print(14 + 3);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testPrintPrintWithComment() {
		String smalltalk = "test\ncerr \"<< count << ' cells '\" << handler spaceLeft << ' spaceLeft.'.!";

		String expectedJava = "public void test() {\nAboraSupport.logger.print(handler.spaceLeft());\nAboraSupport.logger.print(\" spaceLeft.\");\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	

	public void testPrintOn() {
		String smalltalk = "printOn: aStream\naStream << self blah: 34!";

		String expectedJava = "public void printOn(PrintWriter aStream) {\naStream.print(blah(34));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testPrintOnBase() {
		String smalltalk = "printOn: aStream\nmyHashValue printOn: aStream base: 16!";

		String expectedJava = "public void printOn(PrintWriter aStream) {\naStream.print(Integer.toString(myHashValue, 16));\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testPrintStringInt() {
		String smalltalk = "test\n| i {UInt32} | i printString!";

		String expectedJava = "public void test() {\nint i;\nInteger.toString(i);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testPrintStringDouble() {
		String smalltalk = "test\n| d {IEEEDoubleVar} | d printString!";

		String expectedJava = "public void test() {\ndouble d;\nDouble.toString(d);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}


	public void testPrintStringRadix() {
		String smalltalk = "test\nself flags printStringRadix: 2!";

		String expectedJava = "public void test() {\nInteger.toString(flags(), 2);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testQuickCast() {
		String smalltalk = "test\n^blah quickCast: Peter!";

		String expectedJava = "public void test() {\nreturn (Peter) blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testRaisedTo() {
		String smalltalk = "test\n2 raisedTo: height - 2!";

		String expectedJava = "public void test() {\nMath.pow(2, height - 2);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testReanimate() {
		String smalltalk = "test\nfossil reanimate: [:recorder {ResultRecorder} | ^1]!";

		String expectedJava = "public void test() {\nResultRecorder recorder = AboraBlockSupport.enterRecorderFossilReanimate(fossil);\ntry {\nreturn 1;\n}\nfinally {\nAboraBlockSupport.exitRecorderFossilReanimate();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testReceiveHeaper() {
		String smalltalk = "test\n| h { Heaper } | h := rcvr receiveHeaper!";

		String expectedJava = "public void test() {\nHeaper h;\nh = rcvr.receiveHeaper();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testReceiveHeaperNonHeaper() {
		String smalltalk = "test\n| region { XnRegion } | region := rcvr receiveHeaper!";

		String expectedJava = "public void test() {\nXnRegion region;\nregion = (XnRegion) rcvr.receiveHeaper();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testReceiveHeaperDouble() {
		String smalltalk = "test\n| d { IEEE64 } | d := rcvr receiveHeaper!";

		String expectedJava = "public void test() {\ndouble d;\nd = rcvr.receiveIEEEDoubleVar()\n/* TODO was receiveHeaper */\n;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	public void testReceiveHeaperFloat() {
		String smalltalk = "test\n| f { IEEE32 } | f := rcvr receiveHeaper!";

		String expectedJava = "public void test() {\nfloat f;\nf = (float) rcvr.receiveIEEEDoubleVar()\n/* TODO was receiveHeaper */\n;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testReceiveIntegerVarForDeclaredBoolean() {
		String smalltalk = "test\n| startsInside { Boolean } | startsInside := rcvr receiveIntegerVar!";

		String expectedJava = "public void test() {\nboolean startsInside;\nstartsInside = rcvr.receiveBooleanVar();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testRemoveCall() {
		String smalltalk = "test\n^blah asSymbol!";

		String expectedJava = "public void test() {\nreturn blah;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testRenameCall() {
		String smalltalk = "testRename\nAddTallys _ Array new: 500 withAll: 0.!";

		String expectedJava = "public void testRename() {\nAddTallys = new IntArray(500, 0);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	
	public void testRequires() {
		String smalltalk = "test\nself REQUIRES: Stepper!";

		String expectedJava = "public void test() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testReturn() {
		String smalltalk = "test\n^fred!";

		String expectedJava = "public void test() {\nreturn fred;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
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

		String expectedJava = "public void test() {\n123;\n/* whole */\n/* lot of comments */\nreturn fred;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testReturnVoid() {
		String smalltalk = "test\n^VOID!";

		String expectedJava = "public void test() {\nreturn ;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSelfSends() {
		String smalltalk = "test\nself kill. self one: 2!";

		String expectedJava = "public void test() {\nkill();\none(2);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSendHeaper() {
		String smalltalk = "test\n| region { XnRegion } | xmtr sendHeaper: region!";

		String expectedJava = "public void test() {\nXnRegion region;\nxmtr.sendHeaper(region);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSendHeaperDouble() {
		String smalltalk = "test\n| d { IEEE64 } | xmtr sendHeaper: d!";

		String expectedJava = "public void test() {\ndouble d;\nxmtr.sendIEEEDoubleVar(d)\n/* TODO was sendHeaper */\n;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSendIntegerVar() {
		String smalltalk = "test\nxmtr sendIntegerVar: 99!";

		String expectedJava = "public void test() {\nxmtr.sendIntegerVar(99);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSendIntegerVarForBoolean() {
		String smalltalk = "test\n| b { Boolean } | xmtr sendIntegerVar: b!";

		String expectedJava = "public void test() {\nboolean b;\nxmtr.sendBooleanVar(b);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testSendIntegerVarForBooleanExpression() {
		String smalltalk = "test\nxmtr sendIntegerVar: integers isBoundedBelow not!";

		String expectedJava = "public void test() {\nxmtr.sendBooleanVar( ! integers.isBoundedBelow());\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testShouldImplement() {
		String smalltalk = "test\n^Someone shouldImplement!";

		String expectedJava = "public void test() {\nthrow new ShouldImplementException(\"Someone\");\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testShow() {
		String smalltalk = "test\nTranscript show: '.'!";

		String expectedJava = "public void test() {\nAboraSupport.logger.print(\".\");\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testShowOn() {
		String smalltalk = "showOn: oo\noo print: 'hello'!";

		String expectedJava = "public void showOn(PrintWriter oo) {\noo.print(\"hello\");\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSignals() {
		String smalltalk = "test\n^self signals: #(NotInTable)!";

		String expectedJava = "";
		assertInstanceMethod(expectedJava, smalltalk);
		
		JavaMethod method = javaClass.getMethod("test");
		
		assertFalse(method.shouldInclude);
		
		Set problems = (Set)method.getAnnotations().get(Annotation.PROBLEM_SIGNALS);
		assertNotNull(problems);
		assertEquals("size", 1, problems.size());
		assertEquals("NOT_IN_TABLE", (String)problems.iterator().next());
	}

	public void testSignalsMany() {
		String smalltalk = "test\n^self signals: #(NotInTable MustBeOwner)!";

		String expectedJava = "";
		assertInstanceMethod(expectedJava, smalltalk);
		
		JavaMethod method = javaClass.getMethod("test");
		
		assertFalse(method.shouldInclude);
		
		Set problems = (Set)method.getAnnotations().get(Annotation.PROBLEM_SIGNALS);
		assertNotNull(problems);
		assertEquals("size", 2, problems.size());
		Iterator iter = problems.iterator();
		//Should have sorted signal names
		assertEquals("MUST_BE_OWNER", iter.next());
		assertEquals("NOT_IN_TABLE", iter.next());
	}

	public void testSmalltalkOnly() {
		String smalltalk = "smalltalkOnly\n[one blah] smalltalkOnly!";

		String expectedJava = "public void smalltalkOnly() {\none.blah();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSmalltalkOnlyMixed() {
		String smalltalk = "smalltalkOnly\n[two blah] translateOnly. [one blah] smalltalkOnly!";

		String expectedJava = "public void smalltalkOnly() {\n/* Removed translateOnly */\none.blah();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSmalltalkOnlySimpleTranlation() {
		String smalltalk = "test\n[one blah] smalltalkOnly!";

		String expectedJava = "public void test() {\nAboraSupport.smalltalkOnly();\n{\none.blah();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testStrcmp() {
		String smalltalk = "test\nString strcmp: 'hi there' with: 'here'!";

		String expectedJava = "public void test() {\n\"hi there\".compareTo(\"here\");\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testStreamContents() {
		String smalltalk = "test\n^String streamContents: [:aStream | aStream print: 'hello']!";

		String expectedJava = "public void test() {\nStringWriter stringWriter = new StringWriter();\nPrintWriter aStream = new PrintWriter(stringWriter);\naStream.print(\"hello\");\nreturn stringWriter.toString();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testStrlen() {
		String smalltalk = "test\nString strlen: 'hi there'!";

		String expectedJava = "public void test() {\n\"hi there\".length();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testStringLiteral() {
		String smalltalk = "test\n'hi there'' \\\" '''' '!";

		String expectedJava = "public void test() {\n\"hi there' \\\\\\\" '' \";\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testStringAsTextWriteStream() {
		String smalltalk = "test\noo := '' asText writeStream.!";

		String expectedJava = "public void test() {\nStringWriter stringWriter = new StringWriter();\noo = new PrintWriter(stringWriter);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	
	public void testStringDeclaration() {
		String smalltalk = "test: string {Character star}\nstring!";

		String expectedJava = "public void test(String string) {\nstring;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testStringDeclaration2() {
		String smalltalk = "test: string {char star}\nstring!";

		String expectedJava = "public void test(String string) {\nstring;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testStringDo() {
		String smalltalk = "test\nstring do: [:ch {Character} | self putByte: ch]!";

		String expectedJava = "public void test() {\nfor (int doIndex = 0; doIndex < string.length(); doIndex ++ ) {\nchar ch = string.charAt(doIndex);\nputByte(ch);\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	
				
	public void testStringWrapped() {
		String smalltalk = "test\n'hi there\nand here\n'!";

		String expectedJava = "public void test() {\n\"hi there\\n\"+\n\"and here\\n\"+\n\"\";\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testStatementTerminationEmpty() {
		String smalltalk = "test\n!";

		String expectedJava = "public void test() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testStatementTerminationTrailingComment() {
		String smalltalk = "test\nself blah \"hello\"!";

		String expectedJava = "public void test() {\nblah();\n/* hello */\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSubclassResponsibility() {
		String smalltalk = "test\nself subclassResponsibility!";

		String expectedJava = "public void test() {\nthrow new SubclassResponsibilityException();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSubclassResponsibilityReturn() {
		String smalltalk = "test\n^self subclassResponsibility!";

		String expectedJava = "public void test() {\nthrow new SubclassResponsibilityException();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSuper() {
		String smalltalk = "test\nsuper borris!";

		String expectedJava = "public void test() {\nsuper.borris();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSymbol() {
		String smalltalk = "test\n#HiThere!";

		String expectedJava = "public void test() {\nHI_THERE;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSymbolInQuotes() {
		String smalltalk = "test\n#'HiThere'!";

		String expectedJava = "public void test() {\nHI_THERE;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSymbolInQuotesComplex() {
		String smalltalk = "test\n#'HiThere: Again'!";

		String expectedJava = "public void test() {\nHI_THERE__AGAIN;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSymbolInQuotesUpperCase() {
		String smalltalk = "test\n#'HI_THERE'!";

		String expectedJava = "public void test() {\nHI_THERE;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSymbolArrayNew() {
		String smalltalk = "test\n#()!";

		String expectedJava = "public void test() {\nnew Array();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testSymbolWithColons() {
		String smalltalk = "test\n#IDSpace.U.newIDs.U.N2:with:!";

		String expectedJava = "public void test() {\nIDSPACE_UNEW_IDS_UN2_WITH_;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testTemps() {
		String smalltalk = "test\n| one two {Test}|!";

		String expectedJava = "public void test() {\nObject one;\nTest two;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testTempsEmpty() {
		String smalltalk = "test\n| |!";

		String expectedJava = "public void test() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testTimesRepeat() {
		String smalltalk = "test\n4 timesRepeat: [self blah]!";

		String expectedJava = "public void test() {\nfor (int i = 0; i < 4; i ++ ) {\nblah();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testToDo() {
		String smalltalk = "test\n1 to: fred happy do: [:i {UInt32} | blah ]!";

		String expectedJava = "public void test() {\nfor (int i = 1; i <= fred.happy(); i ++ ) {\nblah;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testTranslateOnlyString() {
		String smalltalk = "test\n'one.blah();\nagain()' translateOnly!";

		String expectedJava = "public void test() {\nAboraSupport.translateOnly();\n{\n/* one.blah();\nagain() */\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testTranscript() {
		String smalltalk = "test\nTranscript << 'hello'!";

		String expectedJava = "public void test() {\nAboraSupport.logger.print(\"hello\");\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testTranslateOnly() {
		String smalltalk = "translateOnly\n[one blah] translateOnly!";

		String expectedJava = "public void translateOnly() {\none.blah();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testTranslateOnlyMixed() {
		String smalltalk = "translateOnly\n[one blah] translateOnly. [two blah] smalltalkOnly!";

		String expectedJava = "public void translateOnly() {\none.blah();\n/* Removed smalltalkOnly */\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testTranslateOnlySimpleTranlation() {
		String smalltalk = "test\n[one blah] translateOnly!";

		String expectedJava = "public void test() {\nAboraSupport.translateOnly();\n{\none.blah();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testValueNowOrOnUnwindDo() {
		String smalltalk = "test\n[blah] valueNowOrOnUnwindDo: [hello]!";

		String expectedJava = "public void test() {\ntry {\nblah;\n}\nfinally {\nhello;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testValueNowOrOnUnwindDoExpression() {
		String smalltalk = "test\n[blah] valueNowOrOnUnwindDo: self hello!";

		String expectedJava = "public void test() {\ntry {\nblah;\n}\nfinally {\nhello();\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testUInt8() {
		String smalltalk = "test\nmyStream putByte: $) uint8!";

		String expectedJava = "public void test() {\nmyStream.putByte(')');\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testUInt32Zero() {
		String smalltalk = "test\nUInt32Zero + 2!";

		String expectedJava = "public void test() {\n0 + 2;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testUses() {
		String smalltalk = "test\n1. [HistoryCrum] USES. 2!";

		String expectedJava = "public void test() {\n1;\n2;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testUsesDirect() {
		String smalltalk = "test\nImmuSet USES!";

		String expectedJava = "public void test() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testUsesMultiple() {
		String smalltalk = "test\n[HistoryCrum] USES. [TracePosition] USES. [Ent] USES.!";

		String expectedJava = "public void test() {\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testUnaryOperator() {
		String smalltalk = "test\nfred kill!";

		String expectedJava = "public void test() {\nfred.kill();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testUnaryOperatorMultiple() {
		String smalltalk = "test\nharry one two three!";

		String expectedJava = "public void test() {\nharry.one().two().three();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testUnimplemented() {
		String smalltalk = "test\nself unimplemented!";

		String expectedJava = "public void test() {\nthrow new UnimplementedException();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testUnreachableCodeReturnFodder() {
		String smalltalk = "test\nHeaper BLAST: #NotInTable.\n^NULL!";

		String expectedJava = "public void test() {\nthrow new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testUnreachableCodeReturn() {
		String smalltalk = "test\na ifTrue: [Heaper BLAST: #NotInTable.\n^99].\na blah.\n^99!";

		String expectedJava = "public void test() {\nif (a) {\nthrow new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);\n}\na.blah();\nreturn 99;\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testUnreachableCodeReturnFodderWithComment() {
		String smalltalk = "test\nHeaper BLAST: #NotInTable.\n^NULL \"fodder\"!";

		String expectedJava = "public void test() {\nthrow new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}
	
	public void testWhileFalse() {
		String smalltalk = "test\n[a < 1] whileFalse: [a _ a + 1]!";

		String expectedJava = "public void test() {\nwhile ( ! (a < 1)) {\na = a + 1;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testWhileTrue() {
		String smalltalk = "test\n[a < 1] whileTrue: [a _ a + 1]!";

		String expectedJava = "public void test() {\nwhile (a < 1) {\na = a + 1;\n}\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testWhileTrueNoBlock() {
		String smalltalk = "test\n[agenda step] whileTrue!";

		String expectedJava = "public void test() {\nwhile (agenda.step());\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
	}

	public void testXuTime() {
		String smalltalk = "test\nTime xuTime!";

		String expectedJava = "public void test() {\nAboraSupport.xuTime();\n}\n";
		assertInstanceMethod(expectedJava, smalltalk);
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
		javaClass.addMethod(javaMethod);
		
		ClassTransformer classTransformer = new ClassTransformers();
		classTransformer.transform(javaClass);
		
		classWriter.writeMethod(javaMethod, printWriter);
		printWriter.close();
		return stringWriter.toString();
	}

	protected void assertInstanceMethod(String expectedJava, String smalltalkSource) {
		String actualJava = writeInstanceMethod(smalltalkSource);
		assertMethodBodyEquals(expectedJava, actualJava);
	}

	private void assertMethodBodyEquals(String expectedJava, String actualJava) {
		actualJava = actualJava.replaceAll(System.getProperty("line.separator"), "\n");
		assertEquals(expectedJava, actualJava);
	}

}