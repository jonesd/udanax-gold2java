/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.testing;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Accumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.MuArray;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.testing.ScruSetTester;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class ScruSetTester extends Tester {

	protected ScruTable myTestSets;
/*
udanax-top.st:60218:
Tester subclass: #ScruSetTester
	instanceVariableNames: 'myTestSets {ScruTable NOCOPY of: ScruSet}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Testing'!
*/
/*
udanax-top.st:60222:
(ScruSetTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #(COPY boot ); add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:60490:
ScruSetTester class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:60493:
(ScruSetTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #(COPY boot ); add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ScruSetTester.class).setAttributes( new Set().add( new String[]
	{"COPY", "boot"}).add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public ScruSetTester() {
	super();
	myTestSets = null;
/*
udanax-top.st:60227:ScruSetTester methodsFor: 'creation'!
create
	super create.
	myTestSets _ NULL!
*/
}
public void allTestsOn(PrintWriter oo) {
	oo.print("Start ScruSet testing\n"+
"");
	Stepper stomper = testScruSets().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ScruSet s = (ScruSet) stomper.fetch();
		if (s == null) {
			continue ;
		}
		unaryCheck(s);
	}
	stomper.destroy();
	testIsEmpty(oo);
	testhasMember(oo);
	testContentsEqual(oo);
	testIntersects(oo);
	testIsSubsetOf(oo);
	oo.print("End of Scruset testing\n"+
"");
/*
udanax-top.st:60233:ScruSetTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference}
	oo << 'Start ScruSet testing
'.
	self testScruSets stepper forEach: [:s {ScruSet} |
		self unaryCheck: s].
	self testIsEmpty: oo.
	self testhasMember: oo.
	self testContentsEqual: oo.
	self testIntersects: oo.
	self testIsSubsetOf: oo.
	
	oo << 'End of Scruset testing
'!
*/
}
public void testContentsEqual(PrintWriter oo) {
	oo.print("testing contentsEqual() and contentsHash()\n"+
"");
	if ( ! ( ! ((getScruSet(2)).contentsEqual((getScruSet(3)))))) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ( ! ((getScruSet(2)).contentsEqual((getScruSet(4)))))) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ((getScruSet(2)).contentsEqual((getScruSet(6))))) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ((getScruSet(2)).contentsEqual((getScruSet(8))))) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ( ! 
	/* ensure reflexive */
	((getScruSet(3)).contentsEqual((getScruSet(2)))))) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ( ! ((getScruSet(4)).contentsEqual((getScruSet(2)))))) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ((getScruSet(6)).contentsEqual((getScruSet(2))))) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ((getScruSet(8)).contentsEqual((getScruSet(2))))) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ( ! ((getScruSet(5)).contentsEqual((getScruSet(4)))))) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ((getScruSet(5)).contentsEqual((getScruSet(7))))) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ((getScruSet(2)).contentsHash() == (getScruSet(6)).contentsHash())) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	if ( ! ((getScruSet(5)).contentsHash() == (getScruSet(7)).contentsHash())) {
		throw new AboraAssertionException("contentsEqual failed");
	}
	oo.print("end of ContentsEqual tests\n"+
"");
/*
udanax-top.st:60250:ScruSetTester methodsFor: 'tests'!
{void} testContentsEqual: oo {ostream reference}
	oo << 'testing contentsEqual() and contentsHash()
'.
	((self getScruSet: 2) contentsEqual: (self getScruSet: 3)) not
		assert: 'contentsEqual failed'.
	((self getScruSet: 2) contentsEqual: (self getScruSet: 4)) not
		assert: 'contentsEqual failed'.
	((self getScruSet: 2) contentsEqual: (self getScruSet: 6))
		assert: 'contentsEqual failed'.
	((self getScruSet: 2) contentsEqual: (self getScruSet: 8))
		assert: 'contentsEqual failed'.
	"ensure reflexive"
	((self getScruSet: 3) contentsEqual: (self getScruSet: 2)) not
		assert: 'contentsEqual failed'.
	((self getScruSet: 4) contentsEqual: (self getScruSet: 2)) not
		assert: 'contentsEqual failed'.
	((self getScruSet: 6) contentsEqual: (self getScruSet: 2))
		assert: 'contentsEqual failed'.
	((self getScruSet: 8) contentsEqual: (self getScruSet: 2))
		assert: 'contentsEqual failed'.
	((self getScruSet: 5) contentsEqual: (self getScruSet: 4)) not
		assert: 'contentsEqual failed'.
	((self getScruSet: 5) contentsEqual: (self getScruSet: 7))
		assert: 'contentsEqual failed'.
	((self getScruSet: 2) contentsHash = (self getScruSet: 6) contentsHash)
		assert: 'contentsEqual failed'.
	((self getScruSet: 5) contentsHash = (self getScruSet: 7) contentsHash)
		assert: 'contentsEqual failed'.
	oo << 'end of ContentsEqual tests
'!
*/
}
public void testhasMember(PrintWriter oo) {
	Heaper mem1;
	Heaper mem2;
	oo.print("testing hasMember and theOne\n"+
"");
	mem1 = (getScruSet(2)).theOne();
	if ( ! ((getScruSet(2)).hasMember(mem1))) {
		throw new AboraAssertionException("hasMember failed");
	}
	if ( ! ((getScruSet(8)).hasMember(mem1))) {
		throw new AboraAssertionException("hasMember failed");
	}
	if ( ! ((getScruSet(4)).hasMember(mem1))) {
		throw new AboraAssertionException("hasMember failed");
	}
	mem2 = (getScruSet(3)).theOne();
	if ( ! ((getScruSet(3)).hasMember(mem2))) {
		throw new AboraAssertionException("hasMember failed");
	}
	if ( ! ( ! ((getScruSet(2)).hasMember(mem2)))) {
		throw new AboraAssertionException("hasMember failed");
	}
	if ( ! ( ! ((getScruSet(5)).hasMember(mem2)))) {
		throw new AboraAssertionException("hasMember failed");
	}
	oo.print("end of hasMember and theOne tests\n"+
"");
/*
udanax-top.st:60282:ScruSetTester methodsFor: 'tests'!
{void} testhasMember: oo {ostream reference}
	| mem1 {Heaper} mem2 {Heaper} |
	oo << 'testing hasMember and theOne
'.
	mem1 _ (self getScruSet: 2) theOne.
	((self getScruSet: 2) hasMember: mem1) assert: 'hasMember failed'.
	((self getScruSet: 8) hasMember: mem1) assert: 'hasMember failed'.
	((self getScruSet: 4) hasMember: mem1) assert: 'hasMember failed'.
	mem2 _ (self getScruSet: 3) theOne.
	((self getScruSet: 3) hasMember: mem2) assert: 'hasMember failed'.
	((self getScruSet: 2) hasMember: mem2) not assert: 'hasMember failed'.
	((self getScruSet: 5) hasMember: mem2) not assert: 'hasMember failed'.
	oo << 'end of hasMember and theOne tests
'!
*/
}
public void testIntersects(PrintWriter oo) {
	oo.print("testing intersects\n"+
"");
	if ( ! ( ! ((getScruSet(2)).intersects((getScruSet(1)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(1)).intersects((getScruSet(2)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(2)).intersects((getScruSet(3)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(3)).intersects((getScruSet(2)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(5)).intersects((getScruSet(4)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(5)).intersects((getScruSet(3)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(5)).intersects((getScruSet(2)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(5)).intersects((getScruSet(1)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(4)).intersects((getScruSet(5)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(3)).intersects((getScruSet(5)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(2)).intersects((getScruSet(5)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ( ! ((getScruSet(1)).intersects((getScruSet(5)))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ((getScruSet(2)).intersects((getScruSet(6))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ((getScruSet(6)).intersects((getScruSet(2))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ((getScruSet(7)).intersects((getScruSet(5))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ((getScruSet(5)).intersects((getScruSet(7))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ((getScruSet(2)).intersects((getScruSet(8))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ((getScruSet(8)).intersects((getScruSet(2))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ((getScruSet(9)).intersects((getScruSet(5))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	if ( ! ((getScruSet(5)).intersects((getScruSet(9))))) {
		throw new AboraAssertionException("failed intersects: test");
	}
	oo.print("end of intersects tests\n"+
"");
/*
udanax-top.st:60299:ScruSetTester methodsFor: 'tests'!
{void} testIntersects: oo {ostream reference}
	oo << 'testing intersects
'.
	((self getScruSet: 2) intersects: (self getScruSet: 1)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 1) intersects: (self getScruSet: 2)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 2) intersects: (self getScruSet: 3)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 3) intersects: (self getScruSet: 2)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 5) intersects: (self getScruSet: 4)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 5) intersects: (self getScruSet: 3)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 5) intersects: (self getScruSet: 2)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 5) intersects: (self getScruSet: 1)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 4) intersects: (self getScruSet: 5)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 3) intersects: (self getScruSet: 5)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 2) intersects: (self getScruSet: 5)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 1) intersects: (self getScruSet: 5)) not
		assert: 'failed intersects: test'.
	((self getScruSet: 2) intersects: (self getScruSet: 6)) 
		assert: 'failed intersects: test'.
	((self getScruSet: 6) intersects: (self getScruSet: 2)) 
		assert: 'failed intersects: test'.
	((self getScruSet: 7) intersects: (self getScruSet: 5)) 
		assert: 'failed intersects: test'.
	((self getScruSet: 5) intersects: (self getScruSet: 7)) 
		assert: 'failed intersects: test'.
	((self getScruSet: 2) intersects: (self getScruSet: 8)) 
		assert: 'failed intersects: test'.
	((self getScruSet: 8) intersects: (self getScruSet: 2)) 
		assert: 'failed intersects: test'.
	((self getScruSet: 9) intersects: (self getScruSet: 5)) 
		assert: 'failed intersects: test'.
	((self getScruSet: 5) intersects: (self getScruSet: 9)) 
		assert: 'failed intersects: test'.
	oo << 'end of intersects tests
'!
*/
}
public void testIsEmpty(PrintWriter oo) {
	oo.print("testing isEmpty\n"+
"");
	if ( ! ((getScruSet(1)).isEmpty())) {
		throw new AboraAssertionException("failed isEmpty test");
	}
	if ( ! ( ! (getScruSet(2)).isEmpty())) {
		throw new AboraAssertionException("failed isEmpty test");
	}
	if ( ! ( ! (getScruSet(3)).isEmpty())) {
		throw new AboraAssertionException("failed isEmpty test");
	}
	if ( ! ( ! (getScruSet(4)).isEmpty())) {
		throw new AboraAssertionException("failed isEmpty test");
	}
	if ( ! ( ! (getScruSet(5)).isEmpty())) {
		throw new AboraAssertionException("failed isEmpty test");
	}
	if ( ! ( ! (getScruSet(6)).isEmpty())) {
		throw new AboraAssertionException("failed isEmpty test");
	}
	if ( ! ( ! (getScruSet(7)).isEmpty())) {
		throw new AboraAssertionException("failed isEmpty test");
	}
	if ( ! ( ! (getScruSet(8)).isEmpty())) {
		throw new AboraAssertionException("failed isEmpty test");
	}
	if ( ! ( ! (getScruSet(9)).isEmpty())) {
		throw new AboraAssertionException("failed isEmpty test");
	}
	oo.print("end of isEmpty tests\n"+
"");
/*
udanax-top.st:60346:ScruSetTester methodsFor: 'tests'!
{void} testIsEmpty: oo {ostream reference}
	oo << 'testing isEmpty
'.
	(self getScruSet: 1) isEmpty assert: 'failed isEmpty test'.
	(self getScruSet: 2) isEmpty not assert: 'failed isEmpty test'.
	(self getScruSet: 3) isEmpty not assert: 'failed isEmpty test'.
	(self getScruSet: 4) isEmpty not assert: 'failed isEmpty test'.
	(self getScruSet: 5) isEmpty not assert: 'failed isEmpty test'.
	(self getScruSet: 6) isEmpty not assert: 'failed isEmpty test'.
	(self getScruSet: 7) isEmpty not assert: 'failed isEmpty test'.
	(self getScruSet: 8) isEmpty not assert: 'failed isEmpty test'.
	(self getScruSet: 9) isEmpty not assert: 'failed isEmpty test'.
	oo << 'end of isEmpty tests
'!
*/
}
public void testIsSubsetOf(PrintWriter oo) {
	oo.print("testing isSubsetOf\n"+
"");
	if ( ! ( ! ((getScruSet(3)).isSubsetOf((getScruSet(2)))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ( ! ((getScruSet(2)).isSubsetOf((getScruSet(3)))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ((getScruSet(2)).isSubsetOf((getScruSet(4))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ( ! ((getScruSet(4)).isSubsetOf((getScruSet(2)))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ((getScruSet(8)).isSubsetOf((getScruSet(4))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ( ! ((getScruSet(4)).isSubsetOf((getScruSet(8)))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ((getScruSet(5)).isSubsetOf((getScruSet(9))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ((getScruSet(9)).isSubsetOf((getScruSet(5))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ( ! ((getScruSet(5)).isSubsetOf((getScruSet(4)))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ( ! ((getScruSet(4)).isSubsetOf((getScruSet(5)))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ((getScruSet(6)).isSubsetOf((getScruSet(2))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	if ( ! ((getScruSet(2)).isSubsetOf((getScruSet(6))))) {
		throw new AboraAssertionException("failed isSubsetOf test");
	}
	oo.print("end of isSubsetOf tests\n"+
"");
/*
udanax-top.st:60362:ScruSetTester methodsFor: 'tests'!
{void} testIsSubsetOf: oo {ostream reference}
	oo << 'testing isSubsetOf
'.
	((self getScruSet: 3) isSubsetOf: (self getScruSet: 2)) not 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 2) isSubsetOf: (self getScruSet: 3)) not 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 2) isSubsetOf: (self getScruSet: 4)) 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 4) isSubsetOf: (self getScruSet: 2)) not 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 8) isSubsetOf: (self getScruSet: 4)) 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 4) isSubsetOf: (self getScruSet: 8)) not 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 5) isSubsetOf: (self getScruSet: 9)) 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 9) isSubsetOf: (self getScruSet: 5)) 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 5) isSubsetOf: (self getScruSet: 4)) not 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 4) isSubsetOf: (self getScruSet: 5)) not 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 6) isSubsetOf: (self getScruSet: 2)) 
		assert: 'failed isSubsetOf test'.
	((self getScruSet: 2) isSubsetOf: (self getScruSet: 6)) 
		assert: 'failed isSubsetOf test'.
	oo << 'end of isSubsetOf tests
'!
*/
}
public void unaryCheck(ScruSet a) {
	if ( ! (a.contentsEqual(a))) {
		throw new AboraAssertionException("identity test failed.");
	}
	if ( ! ( ! a.isEmpty() == (a.intersects(a)))) {
		throw new AboraAssertionException("intersects test failed.");
	}
	if ( ! (a.isSubsetOf(a))) {
		throw new AboraAssertionException("self subset test failed.");
	}
	if ( ! (a.isEqual(a))) {
		throw new AboraAssertionException("isEqual test failed");
	}
	Stepper stomper = 
	/* verify that the set knows about all members that it can deliver through a stepper */
	a.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper elem = (Heaper) stomper.fetch();
		if (elem == null) {
			continue ;
		}
		if ( ! (a.hasMember(elem))) {
			throw new AboraAssertionException("failed hasMember for a stepped element");
		}
	}
	stomper.destroy();
/*
udanax-top.st:60393:ScruSetTester methodsFor: 'tests'!
{void} unaryCheck: a {ScruSet} 
	(a contentsEqual: a) assert: 'identity test failed.'.
	a isEmpty not == (a intersects: a) assert: 'intersects test failed.'.
	(a isSubsetOf: a) assert: 'self subset test failed.'.
	(a isEqual: a) assert: 'isEqual test failed'.
	"verify that the set knows about all members that it can deliver through a stepper"
	a stepper forEach: [:elem {Heaper} |
		(a hasMember: elem) assert: 'failed hasMember for a stepped element'].!
*/
}
public void restartScruSetTester(Rcvr rcvr) {
	myTestSets = null;
/*
udanax-top.st:60405:ScruSetTester methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartScruSetTester: rcvr {Rcvr}
	myTestSets _ NULL!
*/
}
public ScruSet generateSet() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:60410:ScruSetTester methodsFor: 'deferred: initialization'!
{ScruSet} generateSet
	self subclassResponsibility!
*/
}
public ScruSet generateSetContaining(Stepper stuff) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:60414:ScruSetTester methodsFor: 'deferred: initialization'!
{ScruSet} generateSetContaining: stuff {Stepper}
	self subclassResponsibility!
*/
}
public ScruSet getScruSet(int number) {
	return (ScruSet) (myTestSets.get(IntegerPos.make((number - 1))));
/*
udanax-top.st:60420:ScruSetTester methodsFor: 'protected: accessing'!
{ScruSet} getScruSet: number {IntegerVar}
	[IntegerPos] USES.
	^ (myTestSets get: (number - 1) integer) cast: ScruSet!
*/
}
public void setTestSets(ScruTable table) {
	myTestSets = table;
/*
udanax-top.st:60424:ScruSetTester methodsFor: 'protected: accessing'!
{void} setTestSets: table {ScruTable of: ScruSet}
	myTestSets _ table!
*/
}
public ScruTable testScruSets() {
	if (myTestSets == null) {
		myTestSets = generateScruSets();
	}
	return myTestSets;
/*
udanax-top.st:60428:ScruSetTester methodsFor: 'protected: accessing'!
{ScruTable of: ScruSet} testScruSets
	
	myTestSets == NULL ifTrue: [myTestSets _ self generateScruSets].
	^ myTestSets!
*/
}
/**
 * generateScruSets must generate a table of sets in the following order:
 * 1) an empty set
 * 2) a set containing one element
 * 3) a set containing one element which is different from that in set 2
 * 4) a set containing at least two elements, one equal to the element in set 2
 * 5) a set containing at least two elements, different from all previous elements
 * 6) a set with the same contents as set 2 not generated by copy()
 * 7) a set with the same contents as set 5 not generated by copy()
 * 8) a set generated by set 2 copy
 * 9) a set generated by set 5 copy
 * other sets are optional and will only be tested with general tests (check).
 */
public ScruTable generateScruSets() {
	Accumulator settab;
	Accumulator idTab1;
	Accumulator idTab2;
	ScruTable idTab3;
	/* settab is an accumulator for a table of sets to return.
	idTab1,2 are accumulators on tables used to generate a set containing
	desired elements for testing. */
	settab = MuArray.arrayAccumulator();
	idTab1 = MuArray.arrayAccumulator();
	idTab2 = MuArray.arrayAccumulator();
	settab.step(generateSet());
	/* #1 */
	idTab1.step((UInt8Array.string("One")));
	idTab3 = ((ScruTable) idTab1.value()).asImmuTable();
	settab.step((generateSetContaining(((ScruTable) idTab1.value()).stepper())));
	/* #2 */
	idTab2.step((UInt8Array.string("Two")));
	settab.step((generateSetContaining(((ScruTable) idTab2.value()).stepper())));
	/* #3 */
	idTab1.step((UInt8Array.string("Three")));
	settab.step((generateSetContaining(((ScruTable) idTab1.value()).stepper())));
	/* #4 */
	idTab2.destroy();
	idTab2 = MuArray.arrayAccumulator();
	idTab2.step((UInt8Array.string("Four")));
	idTab2.step((UInt8Array.string("Five")));
	idTab2.step((UInt8Array.string("Six")));
	settab.step((generateSetContaining(((ScruTable) idTab2.value()).stepper())));
	/* #5 */
	settab.step((generateSetContaining(idTab3.stepper())));
	/* #6 */
	settab.step((generateSetContaining(((ScruTable) idTab2.value()).stepper())));
	/* #7 */
	settab.step(((ScruSet) (((ScruTable) settab.value()).get(IntegerPos.make(1)))).copy());
	/* #8 */
	settab.step(((ScruSet) (((ScruTable) settab.value()).get(IntegerPos.make(4)))).copy());
	/* #9 */
	return (ScruTable) settab.value();
/*
udanax-top.st:60435:ScruSetTester methodsFor: 'private: scruset generation'!
{ScruTable of: ScruSet} generateScruSets
	"generateScruSets must generate a table of sets in the following order:
	  1) an empty set
	  2) a set containing one element
	  3) a set containing one element which is different from that in set 2
	  4) a set containing at least two elements, one equal to the element in set 2
	  5) a set containing at least two elements, different from all previous elements
	  6) a set with the same contents as set 2 not generated by copy()
	  7) a set with the same contents as set 5 not generated by copy()
	  8) a set generated by set 2 copy
	  9) a set generated by set 5 copy
	  other sets are optional and will only be tested with general tests (check).
	  "
	
	| settab {Accumulator} idTab1 {Accumulator} idTab2 {Accumulator}
	 idTab3 {ScruTable} |
	"settab is an accumulator for a table of sets to return.
	idTab1,2 are accumulators on tables used to generate a set containing
	desired elements for testing."
	
	settab _ MuArray arrayAccumulator.
	idTab1 _ MuArray arrayAccumulator.
	idTab2 _ MuArray arrayAccumulator.
	
	settab step: self generateSet. "#1"
	idTab1 step: (UInt8Array string: 'One').
	idTab3 _ (idTab1 value cast: ScruTable) asImmuTable.
	settab step: (self generateSetContaining: (idTab1 value cast: ScruTable) stepper). "#2"
	idTab2 step: (UInt8Array string: 'Two').
	settab step: (self generateSetContaining: (idTab2 value cast: ScruTable) stepper). "#3"
	idTab1 step: (UInt8Array string: 'Three').
	settab step: (self generateSetContaining: (idTab1 value cast: ScruTable) stepper). "#4"
	idTab2 destroy.
	idTab2 _ MuArray arrayAccumulator.
	idTab2 step: (UInt8Array string: 'Four').
	idTab2 step: (UInt8Array string: 'Five').
	idTab2 step: (UInt8Array string: 'Six').
	settab step: (self generateSetContaining: (idTab2 value cast: ScruTable) stepper). "#5"
	settab step: (self generateSetContaining: idTab3 stepper). "#6"
	settab step: (self generateSetContaining: (idTab2 value cast: ScruTable)  stepper). "#7"
	settab step: (((settab value cast: ScruTable) get: 1 integer) cast: ScruSet) copy. "#8"
	settab step: (((settab value cast: ScruTable) get: 4 integer) cast: ScruSet) copy. "#9"
	^ settab value cast: ScruTable!
*/
}
public ScruSetTester(Rcvr receiver) {
	super(receiver);
	restartScruSetTester(receiver);
/*
udanax-top.st:60482:ScruSetTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	self restartScruSetTester: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:60486:ScruSetTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static void suppressInitTimeInherited() {
/*
udanax-top.st:60498:ScruSetTester class methodsFor: 'smalltalk: smalltalk initialization'!
suppressInitTimeInherited!
*/
}
public static void suppressLinkTimeInherited() {
/*
udanax-top.st:60500:ScruSetTester class methodsFor: 'smalltalk: smalltalk initialization'!
suppressLinkTimeInherited!
*/
}
}
