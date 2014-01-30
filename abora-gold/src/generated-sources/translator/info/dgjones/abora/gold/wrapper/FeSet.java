/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.wrapper;

import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.wrapper.FeSet;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import java.io.PrintWriter;

/**
 * An undifferentiated set of RangeElements.
 */
public class FeSet extends FeWrapper {

	protected static FeWrapperSpec TheSetSpec;
/*
udanax-top.st:25180:
FeWrapper subclass: #FeSet
	instanceVariableNames: ''
	classVariableNames: 'TheSetSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:25184:
FeSet comment:
'An undifferentiated set of RangeElements.'!
*/
/*
udanax-top.st:25186:
(FeSet getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:25314:
FeSet class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:25317:
(FeSet getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeSet.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public IDSpace iDSpace() {
	return (IDSpace) edition().coordinateSpace();
/*
udanax-top.st:25191:FeSet methodsFor: 'private:'!
{IDSpace} iDSpace
	^self edition coordinateSpace cast: IDSpace!
*/
}
/**
 * The number of elements in the set
 */
public int count() {
	return edition().count();
/*
udanax-top.st:25196:FeSet methodsFor: 'accessing'!
{IntegerVar CLIENT} count
	"The number of elements in the set"
	
	^self edition count!
*/
}
/**
 * Whether the set includes the given RangeElement
 */
public boolean includes(FeRangeElement value) {
	return ! (edition().keysOf(value)).isEmpty();
/*
udanax-top.st:25201:FeSet methodsFor: 'accessing'!
{BooleanVar CLIENT} includes: value {FeRangeElement}
	"Whether the set includes the given RangeElement"
	
	^(self edition keysOf: value) isEmpty not!
*/
}
/**
 * Return those elements which are in both sets
 */
public FeSet intersect(FeSet other) {
	return FeSet.construct((edition().sharedWith(other.edition())));
/*
udanax-top.st:25206:FeSet methodsFor: 'accessing'!
{FeSet CLIENT} intersect: other {FeSet}
	"Return those elements which are in both sets"
	
	^FeSet construct: (self edition
		sharedWith: other edition)!
*/
}
/**
 * Remove some RangeElements from the set
 */
public FeSet minus(FeSet other) {
	return FeSet.construct((edition().notSharedWith(other.edition())));
/*
udanax-top.st:25212:FeSet methodsFor: 'accessing'!
{FeSet CLIENT} minus: other {FeSet}
	"Remove some RangeElements from the set"
	
	^FeSet construct: (self edition
		notSharedWith: other edition)!
*/
}
/**
 * A stepper over the elements in the set
 */
public Stepper stepper() {
	return edition().stepper();
/*
udanax-top.st:25218:FeSet methodsFor: 'accessing'!
{Stepper of: FeRangeElement} stepper
	"A stepper over the elements in the set"
	
	^self edition stepper!
*/
}
/**
 * If there is exactly one element, then return it
 */
public FeRangeElement theOne() {
	return edition().theOne();
/*
udanax-top.st:25223:FeSet methodsFor: 'accessing'!
{FeRangeElement CLIENT} theOne
	"If there is exactly one element, then return it"
	
	^self edition theOne!
*/
}
/**
 * Return those elements which are in either set
 */
public FeSet unionWith(FeSet other) {
	FeEdition added;
	FeEdition result;
	Stepper stepper;
	PrimArray more;
	/* Need to assign new IDs to avoid collisions */
	added = other.edition().notSharedWith(edition());
	if (added.isEmpty()) {
		return this;
	}
	result = edition();
	stepper = added.stepper();
	while (stepper.hasValue()) {
		more = stepper.stepMany();
		result = result.combine((FeEdition.fromArray(more, (((IDSpace) edition().coordinateSpace()).newIDs(more.count())))));
	}
	return FeSet.construct(result);
/*
udanax-top.st:25228:FeSet methodsFor: 'accessing'!
{FeSet CLIENT} unionWith: other {FeSet}
	"Return those elements which are in either set"
	
	| added {FeEdition} result {FeEdition} stepper {Stepper} more {PrimArray} |
	"Need to assign new IDs to avoid collisions"
	added := other edition notSharedWith: self edition.
	added isEmpty ifTrue: [^self].
	result := self edition.
	stepper := added stepper.
	[stepper hasValue] whileTrue:
		[more := stepper stepMany.
		result := result combine: (FeEdition
			fromArray: more
			with: ((self edition coordinateSpace cast: IDSpace)
				newIDs: more count))].
	^FeSet construct: result!
*/
}
/**
 * Add a RangeElement to the set
 */
public FeSet with(FeRangeElement value) {
	if (includes(value)) {
		return this;
	}
	else {
		return FeSet.construct((edition().with(iDSpace().newID(), value)));
	}
/*
udanax-top.st:25245:FeSet methodsFor: 'accessing'!
{FeSet CLIENT} with: value {FeRangeElement}
	"Add a RangeElement to the set"
	
	(self includes: value)
		ifTrue: [^self]
		ifFalse: [^FeSet construct: (self edition
			with: self iDSpace newID
			with: value)]!
*/
}
/**
 * Remove a RangeElement from the set
 */
public FeSet without(FeRangeElement value) {
	return FeSet.construct((edition().notSharedWith((FeEdition.fromOne(IntegerPos.make(0), value)))));
/*
udanax-top.st:25254:FeSet methodsFor: 'accessing'!
{FeSet CLIENT} without: value {FeRangeElement}
	"Remove a RangeElement from the set"
	
	^FeSet construct: 
		(self edition notSharedWith: 
			(FeEdition fromOne: IntegerVar0 integer with: value))!
*/
}
/**
 * How many elements in the set; if a spec is given, then how many elements of the given spec
 * are in the set
 * @deprecated
 */
public int count(PrimSpec spec) {
	throw new PasseException();
/*
udanax-top.st:25263:FeSet methodsFor: 'smalltalk: passe'!
{IntegerVar} count: spec {PrimSpec default: NULL}
	"How many elements in the set; if a spec is given, then how many elements of the given spec are in the set"
	
	self passe.
	
	spec == NULL
		ifTrue: [^self edition count]
		ifFalse: [^(self edition zoneOf: spec) count]!
*/
}
/**
 * How many elements in the set are Editions; if a spec is given, then how many of them
 * satisfy the given spec
 * @deprecated
 */
public int countEditions(FeWrapperSpec spec) {
	throw new PasseException();
/*
udanax-top.st:25272:FeSet methodsFor: 'smalltalk: passe'!
{IntegerVar} countEditions: spec {FeWrapperSpec default: NULL}
	"How many elements in the set are Editions; if a spec is given, then how many of them satisfy the given spec"
	
	| editions {FeEdition} result {IntegerVar} |
	self passe.
	
	result := IntegerVarZero.
	editions := self edition zoneOf: (PrimSpec pointer: FeEdition).
	spec == NULL
		ifTrue: [^editions count].
	editions stepper forEach: [ :sub {FeEdition} |
		(spec certify: sub) ifTrue:
			[result := result + 1]].
	^result!
*/
}
/**
 * @deprecated
 */
public IDRegion iDs() {
	throw new PasseException();
/*
udanax-top.st:25287:FeSet methodsFor: 'smalltalk: passe'!
{IDRegion} iDs
	self passe "globalIDs"!
*/
}
public FeSet(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:25293:FeSet methodsFor: 'protected: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
public void printOn(PrintWriter oo) {
	int count;
	oo.print(getAboraClass().name());
	oo.print("(");
	count = 0;
	Stepper stomper = stepper();
	for (; stomper.hasValue(); stomper.step()) {
		FeRangeElement object = (FeRangeElement) stomper.fetch();
		if (object == null) {
			continue ;
		}
		if (count > 0) {
			oo.print(", ");
			if (count > 5) {
				oo.print("...)");
				return ;
			}
		}
		oo.print(object);
	}
	stomper.destroy();
	oo.print(")");
/*
udanax-top.st:25299:FeSet methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	| count {IntegerVar} |
	oo << self getCategory name << '('.
	count := IntegerVarZero.
	self stepper forEach: [ :object {FeRangeElement} |
		count > IntegerVarZero ifTrue:
			[oo << ', '.
			count > 5 ifTrue:
				[oo << '...)'.
				^VOID]].
		oo << object].
	oo << ')'!
*/
}
public static FeSet make() {
	return construct((FeEdition.empty(IDSpace.unique())));
/*
udanax-top.st:25322:FeSet class methodsFor: 'pseudo constructors'!
{FeSet CLIENT} make
	^self construct: (FeEdition empty: IDSpace unique)!
*/
}
public static FeSet make(PtrArray works) {
	return (FeSet) (spec().wrap((FeEdition.fromArray(works, (IDSpace.unique().newIDs(works.count()))))));
/*
udanax-top.st:25326:FeSet class methodsFor: 'pseudo constructors'!
{FeSet CLIENT} make: works {PtrArray of: FeRangeElement}
	^(self spec wrap: (FeEdition
		fromArray: works
		with: (IDSpace unique newIDs: works count))) cast: FeSet!
*/
}
public static FeWrapperSpec spec() {
	return TheSetSpec;
/*
udanax-top.st:25332:FeSet class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheSetSpec!
*/
}
public static boolean check(FeEdition edition) {
	return (edition.coordinateSpace().isKindOf(AboraSupport.findCategory(IDSpace.class))) && (edition.isFinite());
/*
udanax-top.st:25338:FeSet class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	^(edition coordinateSpace isKindOf: IDSpace) and: [edition isFinite]!
*/
}
public static FeSet construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeSet) (makeWrapper(edition));
/*
udanax-top.st:25342:FeSet class methodsFor: 'private: wrapping'!
{FeSet} construct: edition {FeEdition}
	self spec endorse: edition.
	^(self makeWrapper: edition) cast: FeSet!
*/
}
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeSet(edition, spec());
/*
udanax-top.st:25347:FeSet class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheSetSpec = wrap;
/*
udanax-top.st:25351:FeSet class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheSetSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("Set", "Wrapper", FE_SET);
/*
udanax-top.st:25357:FeSet class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'Set' with: 'Wrapper' with: #FeSet.!
*/
}
public static void linkTimeNonInherited() {
	TheSetSpec = null;
/*
udanax-top.st:25361:FeSet class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheSetSpec := NULL.!
*/
}
/**
 * {IntegerVar CLIENT} count
 * {BooleanVar CLIENT} includes: value {FeRangeElement}
 * {FeSet CLIENT} intersect: other {FeSet}
 * {FeSet CLIENT} minus: other {FeSet}
 * {FeRangeElement CLIENT} theOne
 * {FeSet CLIENT} unionWith: other {FeSet}
 * {FeSet CLIENT} with: value {FeRangeElement}
 * {FeSet CLIENT} without: value {FeRangeElement}
 */
public static void infostProtocol() {
/*
udanax-top.st:25367:FeSet class methodsFor: 'smalltalk: system'!
info.stProtocol
"{IntegerVar CLIENT} count
{BooleanVar CLIENT} includes: value {FeRangeElement}
{FeSet CLIENT} intersect: other {FeSet}
{FeSet CLIENT} minus: other {FeSet}
{FeRangeElement CLIENT} theOne
{FeSet CLIENT} unionWith: other {FeSet}
{FeSet CLIENT} with: value {FeRangeElement}
{FeSet CLIENT} without: value {FeRangeElement}
"!
*/
}
public FeSet() {
/*

Generated during transformation
*/
}
public FeSet(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
