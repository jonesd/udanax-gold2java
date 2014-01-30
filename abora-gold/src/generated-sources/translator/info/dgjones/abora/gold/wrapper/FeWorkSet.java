/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.wrapper;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.wrapper.FeWorkSet;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import java.io.PrintWriter;

/**
 * An undifferentiated set of Works.
 * Last minute bulletin: This will probably be changed to be a set of any kind of
 * RangeElements, with protocol for testing types.
 */
public class FeWorkSet extends FeWrapper {

	protected static FeWrapperSpec TheWorkSetSpec;
/*
udanax-top.st:25549:
FeWrapper subclass: #FeWorkSet
	instanceVariableNames: ''
	classVariableNames: 'TheWorkSetSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:25553:
FeWorkSet comment:
'An undifferentiated set of Works.
Last minute bulletin: This will probably be changed to be a set of any kind of RangeElements, with protocol for testing types.'!
*/
/*
udanax-top.st:25557:
(FeWorkSet getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #OBSOLETE; add: #SMALLTALK.ONLY; yourself)!
*/
/*
udanax-top.st:25631:
FeWorkSet class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:25634:
(FeWorkSet getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #OBSOLETE; add: #SMALLTALK.ONLY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeWorkSet.class).setAttributes( new Set().add("CONCRETE").add("OBSOLETE").add("SMALLTALKONLY"));
/*

Generated during transformation: AddMethod
*/
}
public IDSpace iDSpace() {
	return (IDSpace) edition().coordinateSpace();
/*
udanax-top.st:25562:FeWorkSet methodsFor: 'private:'!
{IDSpace} iDSpace
	^self edition coordinateSpace cast: IDSpace!
*/
}
/**
 * The current global IDs of all of the Works contained
 */
public IDRegion iDs() {
	return FeServer.iDsOfRange(edition());
/*
udanax-top.st:25567:FeWorkSet methodsFor: 'accessing'!
{IDRegio} iDs
	"The current global IDs of all of the Works contained"
	
	^FeServer iDsOfRange: self edition!
*/
}
/**
 * Whether the set includes the given Work
 */
public boolean includes(FeWork work) {
	return ! (edition().keysOf(work)).isEmpty();
/*
udanax-top.st:25572:FeWorkSet methodsFor: 'accessing'!
{BooleanVar} includes: work {FeWork}
	"Whether the set includes the given Work"
	
	^(self edition keysOf: work) isEmpty not!
*/
}
/**
 * Return those which are in both sets
 */
public FeWorkSet intersect(FeWorkSet other) {
	return FeWorkSet.construct((edition().sharedWith(other.edition())));
/*
udanax-top.st:25577:FeWorkSet methodsFor: 'accessing'!
{FeWorkSet} intersect: other {FeWorkSet}
	"Return those which are in both sets"
	
	^FeWorkSet construct: (self edition
		sharedWith: other edition)!
*/
}
/**
 * Remove some Works from the set
 */
public FeWorkSet minus(FeWorkSet other) {
	return FeWorkSet.construct((edition().notSharedWith(other.edition())));
/*
udanax-top.st:25583:FeWorkSet methodsFor: 'accessing'!
{FeWorkSet} minus: other {FeWorkSet}
	"Remove some Works from the set"
	
	^FeWorkSet construct: (self edition
		notSharedWith: other edition)!
*/
}
/**
 * Return those which are in either set
 */
public FeWorkSet unionWith(FeWorkSet other) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:25589:FeWorkSet methodsFor: 'accessing'!
{FeWorkSet} unionWith: other {FeWorkSet}
	"Return those which are in either set"
	
	| added {FeEdition} |
	"Need to assign new IDs to avoid collisions"
	added := other notSharedWIth: self.
	added isEmpty ifTrue: [^self].
	^FeWorkSet construct: (self edition combine:
		(FeEdition fromArray: added retrieve
			with: (self iDSpace newIDs: added count)))!
*/
}
/**
 * Add a Work to the set
 */
public FeWorkSet with(FeWork work) {
	if (includes(work)) {
		return this;
	}
	else {
		return FeWorkSet.construct((edition().with(iDSpace().newID(), work)));
	}
/*
udanax-top.st:25600:FeWorkSet methodsFor: 'accessing'!
{FeWorkSet} with: work {FeWork}
	"Add a Work to the set"
	
	(self includes: work)
		ifTrue: [^self]
		ifFalse: [^FeWorkSet construct: (self edition
			with: self iDSpace newID
			with: work)]!
*/
}
/**
 * Remove a Work from the set
 */
public FeWorkSet without(FeWork work) {
	return FeWorkSet.construct((edition().notSharedWith((FeEdition.fromOne(IntegerPos.make(0), work)))));
/*
udanax-top.st:25609:FeWorkSet methodsFor: 'accessing'!
{FeWorkSet} without: work {FeWork}
	"Remove a Work from the set"
	
	^FeWorkSet construct: 
		(self edition notSharedWith: 
			(FeEdition fromOne: IntegerVar0 integer with: work))!
*/
}
/**
 * The Works in the set
 */
public PtrArray works() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:25616:FeWorkSet methodsFor: 'accessing'!
{PtrArray of: FeWork} works
	"The Works in the set"
	
	^self edition retrieve!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print('(');
	Stepper stomper = edition().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		FeWork work = (FeWork) stomper.fetch();
		if (work == null) {
			continue ;
		}
		oo.print(' ');
		oo.print(work);
	}
	stomper.destroy();
	oo.print(')');
/*
udanax-top.st:25623:FeWorkSet methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << $(.
	self edition stepper forEach: [ :work {FeWork} |
		oo << $  << work].
	oo << $)!
*/
}
public static FeWorkSet make() {
	return construct((FeEdition.empty(IDSpace.unique())));
/*
udanax-top.st:25639:FeWorkSet class methodsFor: 'pseudo constructors'!
{FeWorkSet} make
	^self construct: (FeEdition empty: IDSpace unique)!
*/
}
public static FeWorkSet make(PtrArray works) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:25643:FeWorkSet class methodsFor: 'pseudo constructors'!
{FeWorkSet} make: works {PtrArray of: FeWork}
	^self spec wrap: (FeEdition
		fromArray: works
		with: (IDSpace unique newIDs: works count))!
*/
}
public static FeWrapperSpec spec() {
	return TheWorkSetSpec;
/*
udanax-top.st:25649:FeWorkSet class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheWorkSetSpec!
*/
}
public static boolean check(FeEdition edition) {
	Ravi.hack();
	/* zones stuff */
	return (edition.coordinateSpace().isKindOf(AboraSupport.findCategory(IDSpace.class))) && (edition.isFinite()
	/* and: [edition count = (edition zoneOf: FeWork spec) count] */
	);
/*
udanax-top.st:25655:FeWorkSet class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	Ravi hack. "zones stuff"
	^(edition coordinateSpace isKindOf: IDSpace)
		and: [edition isFinite
		"and: [edition count = (edition zoneOf: FeWork spec) count]"]!
*/
}
public static FeWorkSet construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeWorkSet) (makeWrapper(edition));
/*
udanax-top.st:25662:FeWorkSet class methodsFor: 'private: wrapping'!
{FeWorkSet} construct: edition {FeEdition}
	self spec endorse: edition.
	^(self makeWrapper: edition) cast: FeWorkSet!
*/
}
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeWorkSet(edition, spec());
/*
udanax-top.st:25667:FeWorkSet class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheWorkSetSpec = wrap;
/*
udanax-top.st:25671:FeWorkSet class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheWorkSetSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("WorkSet", "Wrapper", FE_WORK_SET);
/*
udanax-top.st:25677:FeWorkSet class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'WorkSet' with: 'Wrapper' with: #FeWorkSet.!
*/
}
public static void linkTimeNonInherited() {
	TheWorkSetSpec = null;
/*
udanax-top.st:25681:FeWorkSet class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheWorkSetSpec := NULL.!
*/
}
public FeWorkSet() {
/*

Generated during transformation
*/
}
public FeWorkSet(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public FeWorkSet(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*

Generated during transformation: AddMethod
*/
}
}
