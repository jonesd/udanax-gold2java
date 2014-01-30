/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.integers;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.Problem;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.RegionTester;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import java.io.PrintWriter;

public class RegionTester extends Tester {

	protected ImmuSet myExampleRegions;
/*
udanax-top.st:59681:
Tester subclass: #RegionTester
	instanceVariableNames: 'myExampleRegions {ImmuSet NOCOPY of: XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Integers'!
*/
/*
udanax-top.st:59685:
(RegionTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:59792:
RegionTester class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:59795:
(RegionTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RegionTester.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public ImmuSet initExamples() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:59690:RegionTester methodsFor: 'deferred: init'!
{ImmuSet of: XnRegion} initExamples
	self subclassResponsibility!
*/
}
public void allTestsOn(PrintWriter oo) {
	myExampleRegions = initExamples();
	testExtraOn(oo);
	testUnaryRegionOpsOn(oo);
	testBinaryRegionOpsOn(oo);
/*
udanax-top.st:59695:RegionTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference}
	myExampleRegions _ self initExamples.
	self testExtraOn: oo.
	self testUnaryRegionOpsOn: oo.
	self testBinaryRegionOpsOn: oo.!
*/
}
public void binaryCheck(XnRegion a, XnRegion b) {
	XnRegion anb;
	XnRegion amb;
	XnRegion aub;
	anb = a.intersect(b);
	if ( ! (anb.isEqual((b.intersect(a))))) {
		throw new AboraAssertionException("intersect test failed.");
	}
	if ( ! (anb.isSubsetOf(a))) {
		throw new AboraAssertionException("intersect/subset test failed.");
	}
	if ( ! (anb.isSubsetOf(b))) {
		throw new AboraAssertionException("intersect/subset test failed.");
	}
	if ( ! ((a.intersects(b)) == ! anb.isEmpty())) {
		throw new AboraAssertionException("intersects test failed.");
	}
	amb = a.minus(b);
	if ( ! ( ! (amb.intersects(b)))) {
		throw new AboraAssertionException("minus/intersect test failed.");
	}
	if ( ! (amb.isSubsetOf(a))) {
		throw new AboraAssertionException("minus/subset test failed.");
	}
	aub = a.unionWith(b);
	if ( ! (aub.isEqual((b.unionWith(a))))) {
		throw new AboraAssertionException("unionWith test failed.");
	}
	if ( ! (a.isSubsetOf(aub))) {
		throw new AboraAssertionException("union/subset test failed.");
	}
	if ( ! (b.isSubsetOf(aub))) {
		throw new AboraAssertionException("union/subset test failed.");
	}
	if ( ! (((a.isSubsetOf(b)) && (b.isSubsetOf(a))) == (a.isEqual(b)))) {
		throw new AboraAssertionException("subset/equals test failed.");
	}
/*
udanax-top.st:59701:RegionTester methodsFor: 'testing'!
{void} binaryCheck: a {XnRegion} with: b {XnRegion}
	| anb {XnRegion} amb {XnRegion} aub {XnRegion} |
	anb _ a intersect: b.
	(anb isEqual: (b intersect: a)) assert: 'intersect test failed.'.
	(anb isSubsetOf: a) assert: 'intersect/subset test failed.'.
	(anb isSubsetOf: b) assert: 'intersect/subset test failed.'.
	(a intersects: b) == anb isEmpty not assert: 'intersects test failed.'.
	amb _ a minus: b.
	(amb intersects: b) not assert: 'minus/intersect test failed.'.
	(amb isSubsetOf: a) assert: 'minus/subset test failed.'.
	
	aub _ a unionWith: b.
	(aub isEqual: (b unionWith: a)) assert: 'unionWith test failed.'.
	(a isSubsetOf: aub) assert: 'union/subset test failed.'.
	(b isSubsetOf: aub) assert: 'union/subset test failed.'.
	
	(((a isSubsetOf: b) and: [b isSubsetOf: a]) == (a isEqual: b)) assert: 'subset/equals test failed.'.!
*/
}
public void testBinaryRegionOpsOn(PrintWriter oo) {
	Stepper stomper = myExampleRegions.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion one = (XnRegion) stomper.fetch();
		if (one == null) {
			continue ;
		}
		Stepper stomper2 = myExampleRegions.stepper();
		for (; stomper2.hasValue(); stomper2.step()) {
			XnRegion two = (XnRegion) stomper2.fetch();
			if (two == null) {
				continue ;
			}
			if (one.hashForEqual() <= two.hashForEqual()) {
				try {
					binaryCheck(one, two);
				}
				catch (AboraRuntimeException ex) {
					Problem prob;
					/* Removed translateOnly */
					/* Removed smalltalkOnly */
					/* Removed smalltalkOnly */
					/* Removed translateOnly */
					AboraSupport.logger.print("\n"+
"");
					oo.print("problem checking binary ops of ");
					oo.print(one);
					oo.print(" and ");
					oo.print(two);
					oo.print("\n"+
"");
				}
			}
		}
		stomper2.destroy();
	}
	stomper.destroy();
	oo.print("binary regions tests succeeded\n"+
"");
/*
udanax-top.st:59720:RegionTester methodsFor: 'testing'!
{void} testBinaryRegionOpsOn: oo {ostream reference}
	myExampleRegions stepper forEach: [:one {XnRegion} |
		myExampleRegions stepper forEach: [:two {XnRegion} |
			one hashForEqual <= two hashForEqual ifTrue: [
				Heaper problems.AllBlasts
					handle: [ :ex |
						| prob {Problem} |
						'prob = &PROBLEM(ex);' translateOnly.
						[prob _ Problem create: ex PROBLEM with: ex parameter
									with: ex initialContext sender printString with: 0] smalltalkOnly.
						[cerr <<prob] smalltalkOnly .
						'operator<<(cerr,prob);'translateOnly.
						cerr << '
'.
						oo << 'problem checking binary ops of ' << one << ' and ' << two << '
'.
						ex return]
					do: [self binaryCheck: one with: two]]]].
	oo << 'binary regions tests succeeded
'!
*/
}
public void testExtraOn(PrintWriter oo) {
/*
udanax-top.st:59741:RegionTester methodsFor: 'testing'!
{void} testExtraOn: oo {ostream reference}!
*/
}
public void testUnaryRegionOpsOn(PrintWriter oo) {
	Stepper stomper = myExampleRegions.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion one = (XnRegion) stomper.fetch();
		if (one == null) {
			continue ;
		}
		try {
			unaryCheck(one);
		}
		catch (AboraRuntimeException ex) {
			Problem prob;
			/* Removed translateOnly */
			/* Removed smalltalkOnly */
			/* Removed smalltalkOnly */
			/* Removed translateOnly */
			AboraSupport.logger.print("\n"+
"");
			oo.print("problem checking unary ops of ");
			oo.print(one);
			oo.print("\n"+
"");
		}
	}
	stomper.destroy();
	oo.print("unary regions tests succeeded\n"+
"");
/*
udanax-top.st:59743:RegionTester methodsFor: 'testing'!
{void} testUnaryRegionOpsOn: oo {ostream reference}
	myExampleRegions stepper forEach: [:one {XnRegion} |
		Heaper problems.AllBlasts
			handle: [ :ex |
				| prob {Problem} |
				'prob = &PROBLEM(ex);' translateOnly.
				[prob _ Problem create: ex PROBLEM with: ex parameter
							with: ex initialContext sender printString with: 0] smalltalkOnly.
					[cerr <<prob] smalltalkOnly .
						'operator<<(cerr,prob);'translateOnly.
						cerr << '
'.
				oo << 'problem checking unary ops of ' << one << '
'.
				ex return]
			do: [self unaryCheck: one]].
	oo << 'unary regions tests succeeded
'!
*/
}
public void unaryCheck(XnRegion a) {
	if ( ! (a.isEqual(a))) {
		throw new AboraAssertionException("identity test failed.");
	}
	if ( ! ( ! a.isEmpty() == (a.intersects(a)))) {
		throw new AboraAssertionException("intersects test failed.");
	}
	if ( ! ((a.minus(a)).isEmpty())) {
		throw new AboraAssertionException("self minus/isEmpty failed.");
	}
	if ( ! (a.isSubsetOf(a))) {
		throw new AboraAssertionException("self subset test failed.");
	}
	if ( ! ((a.intersect(a)).isEqual(a))) {
		throw new AboraAssertionException("intersect/isEqual test failed.");
	}
	if ( ! (a.isFull() == a.complement().isEmpty())) {
		throw new AboraAssertionException("infinity inverse test failed.");
	}
	if ( ! ((a.intersect(a.complement())).isEmpty())) {
		throw new AboraAssertionException("intersect/complement test failed.");
	}
	if ( ! ((a.minus(a.complement())).isEqual(a))) {
		throw new AboraAssertionException("minus/complement test failed.");
	}
	if ( ! (a.complement().complement().isEqual(a))) {
		throw new AboraAssertionException("double complement test failed.");
	}
	if ( ! ((a.unionWith(a.complement())).isFull())) {
		throw new AboraAssertionException("union/complement test failed.");
	}
/*
udanax-top.st:59763:RegionTester methodsFor: 'testing'!
{void} unaryCheck: a {XnRegion}
	(a isEqual: a) assert: 'identity test failed.'.
	a isEmpty not == (a intersects: a) assert: 'intersects test failed.'.
	(a minus: a) isEmpty assert: 'self minus/isEmpty failed.'.
	(a isSubsetOf: a) assert: 'self subset test failed.'.
	((a intersect: a) isEqual: a) assert: 'intersect/isEqual test failed.'.
	a isFull == a complement isEmpty assert: 'infinity inverse test failed.'.
	(a intersect: a complement) isEmpty assert: 'intersect/complement test failed.'.
	((a minus: a complement) isEqual: a) assert: 'minus/complement test failed.'.
	(a complement complement isEqual: a) assert: 'double complement test failed.'.
	(a unionWith: a complement) isFull assert: 'union/complement test failed.'.!
*/
}
public ImmuSet exampleRegions() {
	return myExampleRegions;
/*
udanax-top.st:59777:RegionTester methodsFor: 'protected: accessing'!
{ImmuSet of: XnRegion} exampleRegions
	^myExampleRegions!
*/
}
public RegionTester() {
	super();
	myExampleRegions = null;
/*
udanax-top.st:59782:RegionTester methodsFor: 'protected: creation'!
create
	super create.
	myExampleRegions _ NULL.!
*/
}
public void restartRegionTester(Rcvr rcvr) {
	myExampleRegions = null;
/*
udanax-top.st:59788:RegionTester methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartRegionTester: rcvr {Rcvr unused default: NULL}
	myExampleRegions _ NULL.!
*/
}
public static void suppressInitTimeInherited() {
/*
udanax-top.st:59800:RegionTester class methodsFor: 'smalltalk: initialization'!
suppressInitTimeInherited!
*/
}
public static void suppressLinkTimeInherited() {
/*
udanax-top.st:59802:RegionTester class methodsFor: 'smalltalk: initialization'!
suppressLinkTimeInherited!
*/
}
public RegionTester(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
