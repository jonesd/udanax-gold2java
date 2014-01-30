/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nlinks;

import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nlinks.FeHyperRef;
import info.dgjones.abora.gold.nlinks.FeMultiRef;
import info.dgjones.abora.gold.nlinks.FePath;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * An undifferentiated set of HyperRefs
 */
public class FeMultiRef extends FeHyperRef {

	protected static FeWrapperSpec TheMultiRefSpec;
/*
udanax-top.st:24172:
FeHyperRef subclass: #FeMultiRef
	instanceVariableNames: ''
	classVariableNames: 'TheMultiRefSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nlinks'!
*/
/*
udanax-top.st:24176:
FeMultiRef comment:
'An undifferentiated set of HyperRefs'!
*/
/*
udanax-top.st:24178:
(FeMultiRef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:24261:
FeMultiRef class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:24264:
(FeMultiRef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeMultiRef.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The Edition holding the HyperRefs
 */
public FeEdition refsEdition() {
	return (FeEdition) (edition().get((Sequence.string("MultiRef:Refs"))));
/*
udanax-top.st:24183:FeMultiRef methodsFor: 'private:'!
{FeEdition} refsEdition
	"The Edition holding the HyperRefs"
	
	^(self edition get: (Sequence string: 'MultiRef:Refs')) cast: FeEdition!
*/
}
/**
 * With a different refs Edition
 */
public FeMultiRef withRefsEdition(FeEdition edition) {
	Ravi.thingToDo();
	/* check about preserving labels */
	return FeMultiRef.construct((edition().with((Sequence.string("MultiRef:Refs")), edition)));
/*
udanax-top.st:24188:FeMultiRef methodsFor: 'private:'!
{FeMultiRef} withRefsEdition: edition {FeEdition}
	"With a different refs Edition"
	
	Ravi thingToDo. "check about preserving labels"
	^FeMultiRef construct: (self edition
		with: (Sequence string: 'MultiRef:Refs')
		with: edition)!
*/
}
/**
 * Remove those not in the other Refs from the set.
 */
public FeMultiRef intersect(FeMultiRef other) {
	return withRefsEdition((refsEdition().sharedWith(other.refsEdition())));
/*
udanax-top.st:24198:FeMultiRef methodsFor: 'accessing'!
{FeMultiRef CLIENT} intersect: other {FeMultiRef}
	"Remove those not in the other Refs from the set."
	^self withRefsEdition: (self refsEdition sharedWith: other refsEdition)!
*/
}
/**
 * Remove the other Refs from the set.
 */
public FeMultiRef minus(FeMultiRef other) {
	return withRefsEdition((refsEdition().notSharedWith(other.refsEdition())));
/*
udanax-top.st:24203:FeMultiRef methodsFor: 'accessing'!
{FeMultiRef CLIENT} minus: other {FeMultiRef}
	"Remove the other Refs from the set."
	^self withRefsEdition: (self refsEdition notSharedWith: other refsEdition)!
*/
}
/**
 * All the HyperRefs in the collection
 */
public Stepper refs() {
	Ravi.shouldImplement();
	return null;
/*
udanax-top.st:24208:FeMultiRef methodsFor: 'accessing'!
{Stepper CLIENT of: FeHyperRef} refs
	"All the HyperRefs in the collection"
	Ravi shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * Add the other Refs into the set.
 */
public FeMultiRef unionWith(FeMultiRef other) {
	FeEdition added;
	FeEdition result;
	Stepper stepper;
	PrimArray more;
	added = other.refsEdition().notSharedWith(refsEdition());
	if (added.isEmpty()) {
		return this;
	}
	result = refsEdition();
	stepper = added.stepper();
	while (stepper.hasValue()) {
		more = stepper.stepMany();
		result = result.combine((FeEdition.fromArray(more, (((IDSpace) refsEdition().coordinateSpace()).newIDs(more.count())))));
	}
	return withRefsEdition(result);
/*
udanax-top.st:24214:FeMultiRef methodsFor: 'accessing'!
{FeMultiRef CLIENT} unionWith: other {FeMultiRef}
	"Add the other Refs into the set."
	
	| added {FeEdition} result {FeEdition} stepper {Stepper} more {PrimArray} |
	added := other refsEdition notSharedWith: self refsEdition.
	added isEmpty ifTrue: [^self].
	result := self refsEdition.
	stepper := added stepper.
	[stepper hasValue] whileTrue:
		[more := stepper stepMany.
		result := result combine: (FeEdition
			fromArray: more
			with: ((self refsEdition coordinateSpace cast: IDSpace)
				newIDs: more count))].
	^self withRefsEdition: result!
*/
}
/**
 * Add a Ref to the set
 */
public FeMultiRef with(FeHyperRef ref) {
	if ((refsEdition().positionsOf(ref.edition())).isEmpty()) {
		return withRefsEdition((refsEdition().with(((IDSpace) refsEdition().coordinateSpace()).newID(), ref.edition())));
	}
	else {
		return this;
	}
/*
udanax-top.st:24230:FeMultiRef methodsFor: 'accessing'!
{FeMultiRef CLIENT} with: ref {FeHyperRef}
	"Add a Ref to the set"
	
	(self refsEdition positionsOf: ref edition) isEmpty
		ifTrue: [^self withRefsEdition: (self refsEdition
			with: (self refsEdition coordinateSpace cast: IDSpace) newID
			with: ref edition)]
		ifFalse: [^self]!
*/
}
/**
 * Add a Ref to the set
 */
public FeMultiRef without(FeHyperRef ref) {
	XnRegion keys;
	if ((keys = refsEdition().positionsOf(ref.edition())).isEmpty()) {
		return this;
	}
	else {
		return withRefsEdition((refsEdition().copy(keys.complement())));
	}
/*
udanax-top.st:24239:FeMultiRef methodsFor: 'accessing'!
{FeMultiRef CLIENT} without: ref {FeHyperRef}
	"Add a Ref to the set"
	
	| keys {XnRegion} |
	(keys := self refsEdition positionsOf: ref edition) isEmpty
		ifTrue: [^self]
		ifFalse: [^self withRefsEdition: (self refsEdition copy: keys complement)]!
*/
}
/**
 * Make a new HyperRef of the same type with different contents
 */
public FeHyperRef makeNew(FeEdition edition) {
	return FeMultiRef.construct(edition);
/*
udanax-top.st:24249:FeMultiRef methodsFor: 'protected:'!
{FeHyperRef} makeNew: edition {FeEdition}
	"Make a new HyperRef of the same type with different contents"
	
	^FeMultiRef construct: edition!
*/
}
public FeMultiRef(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:24256:FeMultiRef methodsFor: 'private: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
/**
 * Check that it has the right fields in the right places. Ignore other contents.
 */
public static boolean check(FeEdition edition) {
	FeEdition refs;
	return (FeHyperRef.check(edition)) && ((FeWrapper.checkSubEdition(edition, (Sequence.string("MultiRef:Refs")), null, true)) && (((refs = (FeEdition) (edition.get((Sequence.string("MultiRef:Refs"))))).coordinateSpace().isKindOf(AboraSupport.findCategory(IDSpace.class))) && (FeWrapper.checkSubEditions(refs, refs.domain(), FeHyperRef.spec(), true))));
/*
udanax-top.st:24269:FeMultiRef class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	"Check that it has the right fields in the right places. Ignore other contents."
	
	| refs {FeEdition} |
	^(FeHyperRef check: edition)
		and: [(FeWrapper checkSubEdition: edition
			with: (Sequence string: 'MultiRef:Refs')
			with: NULL
			with: true)
		and: [((refs := (edition get: (Sequence string: 'MultiRef:Refs')) cast: FeEdition) coordinateSpace isKindOf: IDSpace)
		and: [FeWrapper checkSubEditions: refs
			with: refs domain
			with: FeHyperRef spec
			with: true]]]!
*/
}
/**
 * Create a new wrapper and endorse it
 */
public static FeMultiRef construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeMultiRef) (makeWrapper(edition));
/*
udanax-top.st:24284:FeMultiRef class methodsFor: 'private: wrapping'!
{FeMultiRef} construct: edition {FeEdition}
	"Create a new wrapper and endorse it"
	
	self spec endorse: edition.
	^(self makeWrapper: edition) cast: FeMultiRef.!
*/
}
/**
 * Just create a new wrapper
 */
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeMultiRef(edition, spec());
/*
udanax-top.st:24290:FeMultiRef class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	"Just create a new wrapper"
	
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheMultiRefSpec = wrap;
/*
udanax-top.st:24295:FeMultiRef class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheMultiRefSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("MultiRef", "HyperRef", FE_MULTI_REF);
/*
udanax-top.st:24301:FeMultiRef class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'MultiRef'
		with: 'HyperRef'
		with: #FeMultiRef.!
*/
}
public static void linkTimeNonInherited() {
	TheMultiRefSpec = null;
/*
udanax-top.st:24307:FeMultiRef class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheMultiRefSpec := NULL.!
*/
}
/**
 * Make a new MultiRef. At least one of the parameters must be non-NULL. The originalContext,
 * if supplied,  must be a frozen Work.
 */
public static FeMultiRef make(PtrArray refs, FeWork workContext, FeWork originalContext, FePath pathContext) {
	FeEdition result;
	FeEdition refEdition;
	if (refs == null && (workContext == null && (originalContext == null && (pathContext == null)))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_SUPPLY_SOME_HYPER_REF_INFORMATION);
	}
	if (originalContext != null && (((BeWork) originalContext.fetchBe()).fetchEditClub() != null)) {
		throw new AboraRuntimeException(AboraRuntimeException.ORIGINAL_CONTEXT_MUST_BE_FROZEN);
	}
	if (refs == null) {
		refEdition = FeEdition.empty(IDSpace.unique());
	}
	else {
		PtrArray array;
		array = PtrArray.nulls(refs.count());
		for (int i = 0; i < refs.count(); i ++ ) {
			array.store(i, ((FeHyperRef) (refs.get(i))).edition());
		}
		refEdition = FeEdition.fromArray(array, (IDSpace.unique().newIDs(array.count())));
	}
	result = FeEdition.fromOne((Sequence.string("MultiRef:Refs")), refEdition);
	if (workContext != null) {
		result = result.with((Sequence.string("HyperRef:WorkContext")), workContext);
	}
	if (originalContext != null) {
		result = result.with((Sequence.string("HyperRef:OriginalContext")), originalContext);
	}
	if (pathContext != null) {
		result = result.with((Sequence.string("HyperRef:PathContext")), pathContext.edition());
	}
	return construct(result);
/*
udanax-top.st:24313:FeMultiRef class methodsFor: 'creation'!
{FeMultiRef CLIENT} make: refs {PtrArray | NULL of: FeHyperRef}
	with: workContext {FeWork default: NULL}
	with: originalContext {FeWork default: NULL}
	with: pathContext {FePath default: NULL}
	"Make a new MultiRef. At least one of the parameters must be non-NULL. The originalContext, if supplied,  must be a frozen Work."
	
	| result {FeEdition} refEdition {FeEdition} |
	(refs == NULL and: [workContext == NULL
			and: [originalContext == NULL and: [pathContext == NULL]]])
		ifTrue: [Heaper BLAST: #MustSupplySomeHyperRefInformation].
	
	(originalContext ~~ NULL
			and: [(originalContext fetchBe cast: BeWork) fetchEditClub ~~ NULL])
		ifTrue: [Heaper BLAST: #OriginalContextMustBeFrozen].
	refs == NULL ifTrue:
		[refEdition := FeEdition empty: IDSpace unique]
	ifFalse:
		[ | array {PtrArray of: FeEdition} |
		array := PtrArray nulls: refs count.
		Int32Zero almostTo: refs count do: [ :i {Int32} |
			array at: i store: ((refs get: i) cast: FeHyperRef) edition].
		refEdition := FeEdition fromArray: array
			with: (IDSpace unique newIDs: array count)].
	result := FeEdition fromOne: (Sequence string: 'MultiRef:Refs') with: refEdition.
	workContext ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'HyperRef:WorkContext')
			with: workContext].
	originalContext ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'HyperRef:OriginalContext')
			with: originalContext].
	pathContext ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'HyperRef:PathContext')
			with: pathContext edition].
	^self construct: result!
*/
}
public static FeWrapperSpec spec() {
	return TheMultiRefSpec;
/*
udanax-top.st:24348:FeMultiRef class methodsFor: 'creation'!
{FeWrapperSpec} spec
	^TheMultiRefSpec!
*/
}
/**
 * {FeMultiRef CLIENT} intersect: other {FeMultiRef}
 * {FeMultiRef CLIENT} minus: other {FeMultiRef}
 * {Stepper CLIENT of: FeHyperRef} refs
 * {FeMultiRef CLIENT} unionWith: other {FeMultiRef}
 * {FeMultiRef CLIENT} with: ref {FeHyperRef}
 * {FeMultiRef CLIENT} without: ref {FeHyperRef}
 */
public static void infostProtocol() {
/*
udanax-top.st:24354:FeMultiRef class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeMultiRef CLIENT} intersect: other {FeMultiRef}
{FeMultiRef CLIENT} minus: other {FeMultiRef}
{Stepper CLIENT of: FeHyperRef} refs
{FeMultiRef CLIENT} unionWith: other {FeMultiRef}
{FeMultiRef CLIENT} with: ref {FeHyperRef}
{FeMultiRef CLIENT} without: ref {FeHyperRef}
"!
*/
}
/**
 * Make a new SingleRef. At least one of the parameters must be non-NULL. The
 * originalContext, if supplied,  must be a frozen Work.
 */
public static FeMultiRef make(PtrArray refs) {
	return make(refs, null, null, null);
/*
udanax-top.st:24365:FeMultiRef class methodsFor: 'smalltalk: defaults'!
{FeMultiRef CLIENT} make: refs {PtrArray | NULL of: FeHyperRef}
	"Make a new SingleRef. At least one of the parameters must be non-NULL. The originalContext, if supplied,  must be a frozen Work."
	^self make: refs with: NULL with: NULL with: NULL!
*/
}
/**
 * Make a new SingleRef. At least one of the parameters must be non-NULL. The
 * originalContext, if supplied,  must be a frozen Work.
 */
public static FeMultiRef make(PtrArray refs, FeWork workContext) {
	return make(refs, workContext, null, null);
/*
udanax-top.st:24370:FeMultiRef class methodsFor: 'smalltalk: defaults'!
{FeMultiRef CLIENT} make: refs {PtrArray | NULL of: FeHyperRef}
	with: workContext {FeWork default: NULL}
	"Make a new SingleRef. At least one of the parameters must be non-NULL. The originalContext, if supplied,  must be a frozen Work."
	^self make: refs with: workContext with: NULL with: NULL!
*/
}
/**
 * Make a new SingleRef. At least one of the parameters must be non-NULL. The
 * originalContext, if supplied,  must be a frozen Work.
 */
public static FeMultiRef make(PtrArray refs, FeWork workContext, FeWork originalContext) {
	return make(refs, workContext, originalContext, null);
/*
udanax-top.st:24376:FeMultiRef class methodsFor: 'smalltalk: defaults'!
{FeMultiRef CLIENT} make: refs {PtrArray | NULL of: FeHyperRef}
	with: workContext {FeWork default: NULL}
	with: originalContext {FeWork default: NULL}
	"Make a new SingleRef. At least one of the parameters must be non-NULL. The originalContext, if supplied,  must be a frozen Work."
	^self make: refs with: workContext with: originalContext with: NULL!
*/
}
public FeMultiRef() {
/*

Generated during transformation
*/
}
public FeMultiRef(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
