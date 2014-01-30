/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.testing;

import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * Testers are for controlling the running of regression tests.
 * See "Regression Testing Procedures and Recommendations".
 */
public class Tester extends Thunk {

/*
udanax-top.st:58048:
Thunk subclass: #Tester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Testing'!
*/
/*
udanax-top.st:58052:
Tester comment:
'Testers are for controlling the running of regression tests.  
See "Regression Testing Procedures and Recommendations".'!
*/
/*
udanax-top.st:58055:
(Tester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:58097:
Tester class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:58100:
(Tester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Tester.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * A regression test is run by calling this method. What the tester writes to 'oo' is
 * actually written to file *o.txt and compared against an approved reference
 * file (*r.txt) of what this tester once used to output. If they match exactly,
 * then the test is passed. Otherwise, someone needs to manually understand why
 * they're different. The diff is in file *d.txt.
 * It is strongly recommended (in order to avoid regression errors) that when a
 * tester is extended to test something new that its output also be extended with
 * some result of the new test. The extended test will then fail the first time. The
 * programmer should verify that the reason for failure is exactly that the
 * tester now additionally outputs the correct results of the new test, in which
 * case this output should be made into the new reference output and the test run
 * again.
 */
public void allTestsOn(PrintWriter oo) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:58060:Tester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	"A regression test is run by calling this method. What the tester writes to 'oo' is 
	actually written to file *o.txt and compared against an approved reference 
	file (*r.txt) of what this tester once used to output. If they match exactly, 
	then the test is passed. Otherwise, someone needs to manually understand why 
	they're different. The diff is in file *d.txt. 
	
	It is strongly recommended (in order to avoid regression errors) that when a 
	tester is extended to test something new that its output also be extended with 
	some result of the new test. The extended test will then fail the first time. The 
	programmer should verify that the reason for failure is exactly that the 
	tester now additionally outputs the correct results of the new test, in which 
	case this output should be made into the new reference output and the test run 
	again."
	self subclassResponsibility!
*/
}
/**
 * The receiver reacts to the key and value (tested my matches:with:), execute it.
 */
public void execute() {
	AboraSupport.translateOnly();
	{
		allTestsOn(AboraSupport.logger);
	}
	AboraSupport.smalltalkOnly();
	{
		allTestsOn(AboraSupport.logger);
	}
	/* [| str {Stream of: char} time {IntegerVar} |
	str _ WriteStream on: (String new: 200).
	time _ Time millisecondsToRun: [self allTestsOn: str].
	Transcript cr; nextPutAll: str contents.
	Transcript << 'Run Time = ' ; << time; show: ' ms
'.	]  smalltalkOnly */
/*
udanax-top.st:58079:Tester methodsFor: 'running'!
{void} execute
	"The receiver reacts to the key and value (tested my matches:with:), execute it."
	
	[self allTestsOn: cerr] translateOnly.
	[self allTestsOn: cerr] smalltalkOnly.
	"[| str {Stream of: char} time {IntegerVar} |
	str _ WriteStream on: (String new: 200).
	time _ Time millisecondsToRun: [self allTestsOn: str].
	Transcript cr; nextPutAll: str contents.
	Transcript << 'Run Time = ' ; << time; show: ' ms
'.	]  smalltalkOnly"!
*/
}
public Tester() {
	super();
/*
udanax-top.st:58093:Tester methodsFor: 'creation'!
create
	super create!
*/
}
/**
 * Returns the tester whose name is 'tName'.  NULL if none.
 */
public static Tester fetchTester(String tName) {
	throw new UnimplementedException();
/*
udanax-top.st:58105:Tester class methodsFor: 'accessing'!
{Tester} fetchTester: tName {char star}
	"Returns the tester whose name is 'tName'.  NULL if none."
	
	self unimplemented.
	^NULL "fodder"
	"Testers stepper forEach: [:tester {Tester} |
		(tester match: tName)
			ifTrue: [^tester]].
	^NULL"!
*/
}
/**
 * Returns the tester whose name is 'tName'.  BLASTs if none.
 */
public static Tester getTester(String tName) {
	Tester result;
	result = fetchTester(tName);
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_FOUND);
	}
	return result;
/*
udanax-top.st:58115:Tester class methodsFor: 'accessing'!
{Tester} getTester: tName {char star}
	"Returns the tester whose name is 'tName'.  BLASTs if none."
	| result {Tester} |
	result _ self fetchTester: tName.
	(result == NULL)
		ifTrue: [Heaper BLAST: #NotFound].
	^result!
*/
}
/**
 * Tester auditionFromMenu
 */
public static void auditionFromMenu() {
	/* | menuDesc {OrderedCollection of: Array} tName {String} |
	menuDesc _ OrderedCollection new.
	Testers stepper forEach: [:tester {Tester} |
		menuDesc add: (Array with: tester name)].
	menuDesc _ menuDesc asSortedCollection: [:a :b | (a at: 1) <= (b at: 1)].
	menuDesc _ menuDesc asOrderedCollection.
	tName _ (PopUpMenu fromArray: menuDesc) startUp.
	tName = 0 
		ifTrue: [^VOID].
	(Tester getTester: tName) class audition. */
/*
udanax-top.st:58126:Tester class methodsFor: 'smalltalk: testing'!
{void} auditionFromMenu
	"Tester auditionFromMenu"
	"| menuDesc {OrderedCollection of: Array} tName {String} |
	menuDesc _ OrderedCollection new.
	Testers stepper forEach: [:tester {Tester} |
		menuDesc add: (Array with: tester name)].
	menuDesc _ menuDesc asSortedCollection: [:a :b | (a at: 1) <= (b at: 1)].
	menuDesc _ menuDesc asOrderedCollection.
	tName _ (PopUpMenu fromArray: menuDesc) startUp.
	tName = 0 
		ifTrue: [^VOID].
	(Tester getTester: tName) class audition."!
*/
}
/*
udanax-top.st:58139:Tester class methodsFor: 'smalltalk: testing'!
{String} defaultRcString
	^self name, '();
'!
*/
/*
udanax-top.st:58144:Tester class methodsFor: 'smalltalk: testing'!
{Behavior} publicClass
	^Tester class!
*/
public static String runTest() {
	return runTest(ALL_TESTS_ON_);
/*
udanax-top.st:58147:Tester class methodsFor: 'smalltalk: testing'!
{String} runTest
	^self runTest: #allTestsOn:!
*/
}
public static String spyTest() {
	return spyTest(ALL_TESTS_ON_);
/*
udanax-top.st:58151:Tester class methodsFor: 'smalltalk: testing'!
{String} spyTest
	^self spyTest: #allTestsOn:!
*/
}
public static String tryTest() {
	return tryTest(ALL_TESTS_ON_);
/*
udanax-top.st:58155:Tester class methodsFor: 'smalltalk: testing'!
{String} tryTest
	^self tryTest: #allTestsOn:!
*/
}
public static String tryTest(String test) {
	PrintWriter str;
	long time;
	StringWriter strString = new StringWriter();
	str = new PrintWriter(strString);
	long timeStart = System.currentTimeMillis();
	new Tester().perform(test, str);
	time = System.currentTimeMillis() - timeStart;
	AboraSupport.logger.print(strString.toString());
	AboraSupport.logger.print("Run Time = ");
	AboraSupport.logger.print(time);
	AboraSupport.logger.print(" ms\n"+
"");
	/*  Typically about16 ms is 'self perform:with:' overhead */
	return strString.toString();
/*
udanax-top.st:58159:Tester class methodsFor: 'smalltalk: testing'!
{String} tryTest: test {Symbol}
	| str {Stream of: char} time {IntegerVar} |
	str _ WriteStream on: (String new: 200).
	time _ Time millisecondsToRun: [self create perform: test with: str].
	Transcript show: str contents; endEntry.
	Transcript << 'Run Time = ' ; << time; << ' ms
'.				" Typically about16 ms is 'self perform:with:' overhead"
	^str contents!
*/
}
public static void suppressInitTimeInherited() {
/*
udanax-top.st:58171:Tester class methodsFor: 'smalltalk: initialization'!
suppressInitTimeInherited!
*/
}
/**
 * @deprecated
 */
public static void doLinkTime() {
	throw new PasseException();
/*
udanax-top.st:58175:Tester class methodsFor: 'smalltalk: passe'!
doLinkTime
	self passe. "use Initializer doLinkTime"!
*/
}
public Tester(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public void perform(String test, PrintWriter out) {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
public static String spyTest(String test) {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
public static String runTest(String test) {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
}
