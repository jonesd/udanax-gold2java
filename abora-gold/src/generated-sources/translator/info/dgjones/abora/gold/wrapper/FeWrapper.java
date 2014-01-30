/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.wrapper;

import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * An object which wraps an Edition, providing additional functionality for manipulating it
 * and enforcing invariants on the format.
 * Implementation note:
 * The fact that you cannot get the spec of a Wrapper is deliberate. You can merely check
 * that it is a kind of Edition you know, but no more; this makes it easy to compatibly add
 * new leaf classes below existing ones.
 */
public class FeWrapper extends Heaper {

	protected FeEdition myEdition;
	protected FeWrapper myInner;
	protected FeWrapperSpec mySpec;
	protected static FeWrapperSpec TheWrapperSpec;
/*
udanax-top.st:23526:
Heaper subclass: #FeWrapper
	instanceVariableNames: '
		myEdition {FeEdition}
		myInner {FeWrapper | NULL}
		mySpec {FeWrapperSpec}'
	classVariableNames: 'TheWrapperSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:23533:
FeWrapper comment:
'An object which wraps an Edition, providing additional functionality for manipulating it and enforcing invariants on the format.
Implementation note:
The fact that you cannot get the spec of a Wrapper is deliberate. You can merely check that it is a kind of Edition you know, but no more; this makes it easy to compatibly add new leaf classes below existing ones.'!
*/
/*
udanax-top.st:23539:
(FeWrapper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:23580:
FeWrapper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:23583:
(FeWrapper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeWrapper.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential. The primitive Edition this is wrapping.
 */
public FeEdition edition() {
	return myEdition;
/*
udanax-top.st:23544:FeWrapper methodsFor: 'accessing'!
{FeEdition CLIENT} edition
	"Essential. The primitive Edition this is wrapping."
	
	^myEdition!
*/
}
/**
 * Essential. The next Wrapper inside this one; blasts if this wraps an Edition directly.
 */
public FeWrapper inner() {
	if (myInner == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NO_INNER_WRAPPER);
	}
	return myInner;
/*
udanax-top.st:23549:FeWrapper methodsFor: 'accessing'!
{FeWrapper CLIENT} inner
	"Essential. The next Wrapper inside this one; blasts if this wraps an Edition directly."
	myInner == NULL ifTrue: [Heaper BLAST: #NoInnerWrapper].
	^myInner!
*/
}
/**
 * Essential. Return TRUE if this is wrapped as the given spec, or any one of its subtypes
 */
public boolean isWrapperOf(FeWrapperSpec spec) {
	return mySpec.isSubSpecOf(spec);
/*
udanax-top.st:23554:FeWrapper methodsFor: 'accessing'!
{BooleanVar} isWrapperOf: spec {FeWrapperSpec}
	"Essential. Return TRUE if this is wrapped as the given spec, or any one of its subtypes"
	
	^mySpec isSubSpecOf: spec!
*/
}
public FeWrapper(FeEdition edition, FeWrapperSpec spec) {
	super();
	myEdition = edition;
	myInner = null;
	mySpec = spec;
/*
udanax-top.st:23561:FeWrapper methodsFor: 'protected: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create.
	myEdition := edition.
	myInner := NULL.
	mySpec := spec.!
*/
}
public FeWrapper(FeEdition edition, FeWrapper inner, FeWrapperSpec spec) {
	super();
	myEdition = edition;
	myInner = inner;
	mySpec = spec;
/*
udanax-top.st:23567:FeWrapper methodsFor: 'protected: create'!
create: edition {FeEdition} with: inner {FeWrapper} with: spec {FeWrapperSpec}
	super create.
	myEdition := edition.
	myInner := inner.
	mySpec := spec.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:23575:FeWrapper methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:23577:FeWrapper methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.ABSTRACTWRAPPER("Wrapper", null, FE_WRAPPER);
/*
udanax-top.st:23588:FeWrapper class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	FeWrapperSpec ABSTRACTWRAPPER: 'Wrapper' with: NULL with: #FeWrapper!
*/
}
public static void linkTimeNonInherited() {
	TheWrapperSpec = null;
/*
udanax-top.st:23592:FeWrapper class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	TheWrapperSpec := NULL.!
*/
}
public static void setSpec(FeWrapperSpec spec) {
	TheWrapperSpec = spec;
/*
udanax-top.st:23598:FeWrapper class methodsFor: 'private: wrapping'!
{void} setSpec: spec {FeWrapperSpec}
	TheWrapperSpec := spec.!
*/
}
public static FeWrapperSpec spec() {
	return TheWrapperSpec;
/*
udanax-top.st:23604:FeWrapper class methodsFor: 'accessing'!
{FeWrapperSpec} spec
	^TheWrapperSpec!
*/
}
/**
 * Checks that the domain is in the right coordinate space and is a superset of the given
 * region
 */
public static boolean checkDomainHas(FeEdition edition, XnRegion required) {
	return (edition.coordinateSpace().isEqual(required.coordinateSpace())) && (required.isSubsetOf(edition.domain()));
/*
udanax-top.st:23610:FeWrapper class methodsFor: 'protected: checking'!
{BooleanVar} checkDomainHas: edition {FeEdition}
	with: required {XnRegion}
	"Checks that the domain is in the right coordinate space and is a superset of the given region"
	
	^(edition coordinateSpace isEqual: required coordinateSpace)
		and: [required isSubsetOf: edition domain]!
*/
}
/**
 * Checks that the domain is in the right coordinate space and a subset of the given region
 */
public static boolean checkDomainIn(FeEdition edition, XnRegion limit) {
	return (edition.coordinateSpace().isEqual(limit.coordinateSpace())) && (edition.domain().isSubsetOf(limit));
/*
udanax-top.st:23617:FeWrapper class methodsFor: 'protected: checking'!
{BooleanVar} checkDomainIn: edition {FeEdition}
	with: limit {XnRegion}
	"Checks that the domain is in the right coordinate space and a subset of the given region"
	
	^(edition coordinateSpace isEqual: limit coordinateSpace)
		and: [edition domain isSubsetOf: limit]!
*/
}
/**
 * If there is a SubEdition at a key in an edition, and if a spec is supplied, that it can be
 * certified as the given type
 */
public static boolean checkSubEdition(FeEdition parent, Position key, FeWrapperSpec spec, boolean required) {
	FeRangeElement value;
	value = parent.fetch(key);
	if (value == null) {
		return ! required;
	}
	return (value instanceof FeEdition) && (spec == null || (spec.certify(((FeEdition) value))));
/*
udanax-top.st:23624:FeWrapper class methodsFor: 'protected: checking'!
{BooleanVar} checkSubEdition: parent {FeEdition}
	with: key {Position}
	with: spec {FeWrapperSpec | NULL}
	with: required {BooleanVar}
	"If there is a SubEdition at a key in an edition, and if a spec is supplied, that it can be certified as the given type"
	
	| value {FeRangeElement} |
	value := parent fetch: key.
	value == NULL ifTrue: [^required not].
	^(value isKindOf: FeEdition)
		and: [spec == NULL
			or: [spec certify: (value cast: FeEdition)]]!
*/
}
/**
 * Check that everything in the region is an Edition, which can be certified with the given
 * type
 */
public static boolean checkSubEditions(FeEdition parent, XnRegion keys, FeWrapperSpec spec, boolean required) {
	Stepper stomper = keys.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Position key = (Position) stomper.fetch();
		if (key == null) {
			continue ;
		}
		if ( ! (checkSubEdition(parent, key, spec, required))) {
			return false;
		}
	}
	stomper.destroy();
	return true;
/*
udanax-top.st:23637:FeWrapper class methodsFor: 'protected: checking'!
{BooleanVar} checkSubEditions: parent {FeEdition}
	with: keys {XnRegion}
	with: spec {FeWrapperSpec}
	with: required {BooleanVar}
	"Check that everything in the region is an Edition, which can be certified with the given type"
	
	keys stepper forEach:
		[ :key {Position} |
		(self checkSubEdition: parent with: key with: spec with: required) ifFalse:
			[^false]].
	^true!
*/
}
/**
 * Whether there is an Edition there which can be successfully converted into a zero based
 * Sequence
 */
public static boolean checkSubSequence(FeEdition edition, Position key, boolean required) {
	FeRangeElement value;
	Ravi.hack();
	/* zones */
	value = edition.fetch(key);
	if (value == null) {
		return ! required;
	}
	return (value instanceof FeEdition) && ((((FeEdition) value).coordinateSpace().isEqual(IntegerSpace.make())) && (((IntegerRegion) ((FeEdition) value).domain()).isCompacted()
	/* and: [((value cast: FeEdition) zoneOf: PrimSpec uInt8) domain
			isEqual: (value cast: FeEdition) domain] */
	));
/*
udanax-top.st:23649:FeWrapper class methodsFor: 'protected: checking'!
{BooleanVar} checkSubSequence: edition {FeEdition} with: key {Position} with: required {BooleanVar}
	"Whether there is an Edition there which can be successfully converted into a zero based Sequence"
	
	| value {FeRangeElement} |
	Ravi hack. "zones"
	value := edition fetch: key.
	value == NULL ifTrue: [^required not].
	^(value isKindOf: FeEdition)
		and: [((value cast: FeEdition) coordinateSpace isEqual: IntegerSpace make)
		and: [((value cast: FeEdition) domain cast: IntegerRegion) isCompacted
		"and: [((value cast: FeEdition) zoneOf: PrimSpec uInt8) domain
			isEqual: (value cast: FeEdition) domain]"]]!
*/
}
/**
 * If there is a SubWork at a key in an edition
 */
public static boolean checkSubWork(FeEdition parent, Position key, boolean required) {
	FeRangeElement value;
	value = parent.fetch(key);
	if (value == null) {
		return ! required;
	}
	return value != null && (value instanceof FeWork);
/*
udanax-top.st:23662:FeWrapper class methodsFor: 'protected: checking'!
{BooleanVar} checkSubWork: parent {FeEdition} with: key {Position} with: required {BooleanVar}
	"If there is a SubWork at a key in an edition"
	
	| value {FeRangeElement} |
	value := parent fetch: key.
	value == NULL ifTrue: [^required not].
	^value ~~ NULL and: [value isKindOf: FeWork]!
*/
}
/**
 * If there is a SubEdition at a key in an edition, that it can be wrapped as a Set, and if a
 * spec is supplied, that it only contains the given type
 * @deprecated
 */
public static boolean checkSubSetEdition(FeEdition parent, Position key, FeWrapperSpec spec, boolean required) {
	throw new PasseException();
/*
udanax-top.st:23672:FeWrapper class methodsFor: 'smalltalk: passe'!
{BooleanVar} checkSubSetEdition: parent {FeEdition}
	with: key {Position}
	with: spec {FeWrapperSpec | NULL}
	with: required {BooleanVar}
	"If there is a SubEdition at a key in an edition, that it can be wrapped as a Set, and if a spec is supplied, that it only contains the given type"
	
	| value {FeRangeElement} set {FeSet} |
	self passe.
	
	value := parent fetch: key.
	value == NULL ifTrue: [^required not].
	((value isKindOf: FeEdition) and: [FeSet spec certify: (value cast: FeEdition)])
		ifFalse: [^false].
	set := (FeSet spec wrap: (value cast: FeEdition)) cast: FeSet.
	^spec == NULL or: [set count = (set count: spec)]!
*/
}
/**
 * {FeEdition CLIENT} edition
 * {FeWrapper CLIENT} inner
 * {BooleanVar CLIENT} isWrappedAs: spec {FeWrapperSpec}
 */
public static void infostProtocol() {
/*
udanax-top.st:23690:FeWrapper class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeEdition CLIENT} edition
{FeWrapper CLIENT} inner
{BooleanVar CLIENT} isWrappedAs: spec {FeWrapperSpec}
"!
*/
}
public FeWrapper() {
/*

Generated during transformation
*/
}
public FeWrapper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
