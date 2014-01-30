/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.testing;

import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.testing.MuSetTester;
import info.dgjones.abora.gold.testing.ScruSetTester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class MuSetTester extends ScruSetTester {

/*
udanax-top.st:60540:
ScruSetTester subclass: #MuSetTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Testing'!
*/
/*
udanax-top.st:60544:
(MuSetTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #(COPY boot ); add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:60631:
MuSetTester class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:60634:
(MuSetTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #(COPY boot ); add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(MuSetTester.class).setAttributes( new Set().add( new String[]
	{"COPY", "boot"}).add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public ScruSet generateSet() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:60549:MuSetTester methodsFor: 'deferred: initialization'!
{ScruSet} generateSet
	self subclassResponsibility!
*/
}
public ScruSet generateSetContaining(Stepper stuff) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:60553:MuSetTester methodsFor: 'deferred: initialization'!
{ScruSet} generateSetContaining: stuff {Stepper}
	self subclassResponsibility!
*/
}
public void binaryCheck(MuSet a, MuSet b) {
	MuSet anb;
	MuSet bna;
	MuSet amb;
	MuSet aub;
	MuSet bua;
	anb = (MuSet) a.copy();
	anb.restrictTo(b);
	bna = (MuSet) b.copy();
	bna.restrictTo(a);
	if ( ! (anb.contentsEqual(bna))) {
		throw new AboraAssertionException("intersect test failed.");
	}
	if ( ! (anb.isSubsetOf(a))) {
		throw new AboraAssertionException("intersect/subset test failed.");
	}
	if ( ! (anb.isSubsetOf(b))) {
		throw new AboraAssertionException("intersect/subset test failed.");
	}
	if ( ! (bna.isSubsetOf(a))) {
		throw new AboraAssertionException("intersect/subset test failed.");
	}
	if ( ! (bna.isSubsetOf(b))) {
		throw new AboraAssertionException("intersect/subset test failed.");
	}
	if ( ! ((a.intersects(b)) == ( ! anb.isEmpty()))) {
		throw new AboraAssertionException("intersects test failed.");
	}
	amb = (MuSet) a.copy();
	amb.wipeAll(b);
	if ( ! ( ! (amb.intersects(b)))) {
		throw new AboraAssertionException("minus/intersect test failed.");
	}
	if ( ! (amb.isSubsetOf(a))) {
		throw new AboraAssertionException("minus/subset test failed.");
	}
	aub = (MuSet) a.copy();
	aub.storeAll(b);
	bua = (MuSet) b.copy();
	bua.storeAll(a);
	if ( ! (aub.contentsEqual(bua))) {
		throw new AboraAssertionException("unionWith test failed.");
	}
	if ( ! (a.isSubsetOf(aub))) {
		throw new AboraAssertionException("union/subset test failed.");
	}
	if ( ! (b.isSubsetOf(aub))) {
		throw new AboraAssertionException("union/subset test failed.");
	}
	if ( ! (((a.isSubsetOf(b)) && (b.isSubsetOf(a))) == (a.contentsEqual(b)))) {
		throw new AboraAssertionException("subset/equals test failed.");
	}
/*
udanax-top.st:60559:MuSetTester methodsFor: 'tests'!
{void} binaryCheck: a {MuSet} with: b {MuSet}
	| anb {MuSet} bna {MuSet} amb {MuSet} aub {MuSet} bua {MuSet} |
	anb _ a copy cast: MuSet.
	anb restrictTo: b.
	bna _ b copy cast: MuSet.
	bna restrictTo: a.
	(anb contentsEqual: bna) assert: 'intersect test failed.'.
	(anb isSubsetOf: a) assert: 'intersect/subset test failed.'.
	(anb isSubsetOf: b) assert: 'intersect/subset test failed.'.
	(bna isSubsetOf: a) assert: 'intersect/subset test failed.'.
	(bna isSubsetOf: b) assert: 'intersect/subset test failed.'.
	(a intersects: b) == (anb isEmpty not) assert: 'intersects test failed.'.
	amb _ a copy cast: MuSet.
	amb wipeAll: b.
	(amb intersects: b) not assert: 'minus/intersect test failed.'.
	(amb isSubsetOf: a) assert: 'minus/subset test failed.'.
	aub _ a copy cast: MuSet.
	aub storeAll: b.
	bua _ b copy cast: MuSet.
	bua storeAll: a.
	(aub contentsEqual: bua) assert: 'unionWith test failed.'.
	(a isSubsetOf: aub) assert: 'union/subset test failed.'.
	(b isSubsetOf: aub) assert: 'union/subset test failed.'.
	
	(((a isSubsetOf: b) and: [b isSubsetOf: a]) == (a contentsEqual: b)) assert: 'subset/equals test failed.'.!
*/
}
public void binarySetTestsOn(PrintWriter oo) {
	oo.print("start binary tests\n"+
"");
	Stepper stomper = testMuSets().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		MuSet setone = (MuSet) stomper.fetch();
		if (setone == null) {
			continue ;
		}
		Stepper stomper2 = testMuSets().stepper();
		for (; stomper2.hasValue(); stomper2.step()) {
			MuSet settwo = (MuSet) stomper2.fetch();
			if (settwo == null) {
				continue ;
			}
			binaryCheck(setone, settwo);
		}
		stomper2.destroy();
	}
	stomper.destroy();
	oo.print("end binary tests\n"+
"");
/*
udanax-top.st:60586:MuSetTester methodsFor: 'tests'!
{void} binarySetTestsOn: oo {ostream reference} 
	oo << 'start binary tests
'. 
	self testMuSets stepper forEach: [:setone {MuSet} | 
		self testMuSets stepper forEach: [:settwo {MuSet} | 
			self binaryCheck: setone with: settwo]].
	oo << 'end binary tests
'!
*/
}
public void unaryCheck(ScruSet a) {
	MuSet ta;
	super.unaryCheck(a);
	ta = (MuSet) a.copy();
	ta.wipeAll(a);
	if ( ! (ta.isEmpty())) {
		throw new AboraAssertionException("self minus/isEmpty failed.");
	}
	ta = (MuSet) a.copy();
	ta.restrictTo(a);
	if ( ! (ta.contentsEqual(a))) {
		throw new AboraAssertionException("intersect/isEqual test failed.");
	}
/*
udanax-top.st:60595:MuSetTester methodsFor: 'tests'!
{void} unaryCheck: a {ScruSet} 
	| ta {MuSet} |
	
	super unaryCheck: a.
	ta _ a copy cast: MuSet.
	ta wipeAll: a.
	ta isEmpty assert: 'self minus/isEmpty failed.'.
	ta _ a copy cast: MuSet.
	ta restrictTo: a.
	(ta contentsEqual: a) assert: 'intersect/isEqual test failed.'!
*/
}
public void allTestsOn(PrintWriter oo) {
	oo.print("MuSet testing\n"+
"");
	super.allTestsOn(oo);
	binarySetTestsOn(oo);
	oo.print("End of MuSet testing\n"+
"");
/*
udanax-top.st:60608:MuSetTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	oo << 'MuSet testing
'.
	super allTestsOn: oo.
	self binarySetTestsOn: oo.
	oo << 'End of MuSet testing
'!
*/
}
public ScruTable testMuSets() {
	return testScruSets();
/*
udanax-top.st:60618:MuSetTester methodsFor: 'protected: accessing'!
{ScruTable of: MuSet} testMuSets
	^ self testScruSets!
*/
}
public MuSetTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:60624:MuSetTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:60627:MuSetTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static void suppressInitTimeInherited() {
/*
udanax-top.st:60639:MuSetTester class methodsFor: 'smalltalk: smalltalk initialization'!
suppressInitTimeInherited!
*/
}
public static void suppressLinkTimeInherited() {
/*
udanax-top.st:60641:MuSetTester class methodsFor: 'smalltalk: smalltalk initialization'!
suppressLinkTimeInherited!
*/
}
public MuSetTester() {
/*

Generated during transformation
*/
}
}
