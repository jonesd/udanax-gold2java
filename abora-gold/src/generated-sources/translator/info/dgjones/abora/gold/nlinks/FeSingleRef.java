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
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nlinks.FeHyperRef;
import info.dgjones.abora.gold.nlinks.FePath;
import info.dgjones.abora.gold.nlinks.FeSingleRef;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Represents a single attachment to some material in the context of a Work, and maybe a Path
 * beneath it.
 */
public class FeSingleRef extends FeHyperRef {

	protected static FeWrapperSpec TheSingleRefSpec;
/*
udanax-top.st:24383:
FeHyperRef subclass: #FeSingleRef
	instanceVariableNames: ''
	classVariableNames: 'TheSingleRefSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nlinks'!
*/
/*
udanax-top.st:24387:
FeSingleRef comment:
'Represents a single attachment to some material in the context of a Work, and maybe a Path beneath it.'!
*/
/*
udanax-top.st:24389:
(FeSingleRef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:24418:
FeSingleRef class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:24421:
(FeSingleRef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeSingleRef.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The material to which this HyperRef is attached.
 */
public FeEdition excerpt() {
	return (FeEdition) (edition().get((Sequence.string("HyperRef:Excerpt"))));
/*
udanax-top.st:24394:FeSingleRef methodsFor: 'accessing'!
{FeEdition CLIENT} excerpt
	"The material to which this HyperRef is attached."
	^(self edition get: (Sequence string: 'HyperRef:Excerpt')) cast: FeEdition!
*/
}
/**
 * Make this Ref point at different material.
 */
public FeSingleRef withExcerpt(FeEdition excerpt) {
	return FeSingleRef.construct((edition().with((Sequence.string("HyperRef:Excerpt")), excerpt)));
/*
udanax-top.st:24399:FeSingleRef methodsFor: 'accessing'!
{FeSingleRef CLIENT} withExcerpt: excerpt {FeEdition}
	"Make this Ref point at different material."
	^FeSingleRef construct: (self edition with: (Sequence string: 'HyperRef:Excerpt') with: excerpt)!
*/
}
/**
 * Make a new HyperRef of the same type with different contents
 */
public FeHyperRef makeNew(FeEdition edition) {
	return FeSingleRef.construct(edition);
/*
udanax-top.st:24406:FeSingleRef methodsFor: 'protected:'!
{FeHyperRef} makeNew: edition {FeEdition}
	"Make a new HyperRef of the same type with different contents"
	
	^FeSingleRef construct: edition!
*/
}
public FeSingleRef(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:24413:FeSingleRef methodsFor: 'private: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
/**
 * Check that it has the right fields in the right places. Ignore other contents.
 */
public static boolean check(FeEdition edition) {
	return (FeHyperRef.check(edition)) && (FeWrapper.checkSubEdition(edition, (Sequence.string("HyperRef:AttachedMaterial")), null, false));
/*
udanax-top.st:24426:FeSingleRef class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	"Check that it has the right fields in the right places. Ignore other contents."
	
	^(FeHyperRef check: edition)
		and: [FeWrapper checkSubEdition: edition
			with: (Sequence string: 'HyperRef:AttachedMaterial')
			with: NULL
			with: false]!
*/
}
/**
 * Create a new wrapper and endorse it
 */
public static FeSingleRef construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeSingleRef) (makeWrapper(edition));
/*
udanax-top.st:24435:FeSingleRef class methodsFor: 'private: wrapping'!
{FeSingleRef} construct: edition {FeEdition}
	"Create a new wrapper and endorse it"
	
	self spec endorse: edition.
	^(self makeWrapper: edition) cast: FeSingleRef!
*/
}
/**
 * Just create a new wrapper
 */
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeSingleRef(edition, spec());
/*
udanax-top.st:24441:FeSingleRef class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	"Just create a new wrapper"
	
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheSingleRefSpec = wrap;
/*
udanax-top.st:24446:FeSingleRef class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheSingleRefSpec := wrap.!
*/
}
/**
 * Make a new SingleRef. At least one of the parameters must be non-NULL. The
 * originalContext, if supplied,  must be a frozen Work.
 */
public static FeSingleRef make(FeEdition material, FeWork workContext, FeWork originalContext, FePath pathContext) {
	FeEdition result;
	if (material == null && (workContext == null && (originalContext == null && (pathContext == null)))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_SUPPLY_SOME_HYPER_REF_INFORMATION);
	}
	if (originalContext != null && (((BeWork) originalContext.fetchBe()).fetchEditClub() != null)) {
		throw new AboraRuntimeException(AboraRuntimeException.ORIGINAL_CONTEXT_MUST_BE_FROZEN);
	}
	result = FeEdition.empty(SequenceSpace.make());
	if (workContext != null) {
		result = result.with((Sequence.string("HyperRef:WorkContext")), workContext);
	}
	if (originalContext != null) {
		result = result.with((Sequence.string("HyperRef:OriginalContext")), originalContext);
	}
	if (material != null) {
		result = result.with((Sequence.string("HyperRef:Excerpt")), material);
	}
	if (pathContext != null) {
		result = result.with((Sequence.string("HyperRef:PathContext")), pathContext.edition());
	}
	return construct(result);
/*
udanax-top.st:24452:FeSingleRef class methodsFor: 'creation'!
{FeSingleRef CLIENT} make: material {FeEdition | NULL}
	with: workContext {FeWork default: NULL}
	with: originalContext {FeWork default: NULL}
	with: pathContext {FePath default: NULL}
	"Make a new SingleRef. At least one of the parameters must be non-NULL. The originalContext, if supplied,  must be a frozen Work."
	
	| result {FeEdition} |
	(material == NULL and: [workContext == NULL
			and: [originalContext == NULL and: [pathContext == NULL]]])
		ifTrue: [Heaper BLAST: #MustSupplySomeHyperRefInformation].
	(originalContext ~~ NULL
			and: [(originalContext fetchBe cast: BeWork) fetchEditClub ~~ NULL])
		ifTrue: [Heaper BLAST: #OriginalContextMustBeFrozen].
	result := FeEdition empty: SequenceSpace make.
	workContext ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'HyperRef:WorkContext')
			with: workContext].
	originalContext ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'HyperRef:OriginalContext')
			with: originalContext].
	material ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'HyperRef:Excerpt')
			with: material].
	pathContext ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'HyperRef:PathContext')
			with: pathContext edition].
	^self construct: result!
*/
}
public static FeWrapperSpec spec() {
	return TheSingleRefSpec;
/*
udanax-top.st:24481:FeSingleRef class methodsFor: 'creation'!
{FeWrapperSpec} spec
	^TheSingleRefSpec!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("SingleRef", "HyperRef", FE_SINGLE_REF);
/*
udanax-top.st:24487:FeSingleRef class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'SingleRef'
		with: 'HyperRef'
		with: #FeSingleRef.!
*/
}
public static void linkTimeNonInherited() {
	TheSingleRefSpec = null;
/*
udanax-top.st:24493:FeSingleRef class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheSingleRefSpec := NULL.!
*/
}
/**
 * {FeEdition CLIENT} excerpt
 */
public static void infostProtocol() {
/*
udanax-top.st:24499:FeSingleRef class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeEdition CLIENT} excerpt
"!
*/
}
/**
 * Make a new SingleRef. At least one of the parameters must be non-NULL. The
 * originalContext, if supplied,  must be a frozen Work.
 */
public static FeSingleRef make(FeEdition material) {
	return make(material, null, null, null);
/*
udanax-top.st:24505:FeSingleRef class methodsFor: 'smalltalk: defaults'!
{FeSingleRef CLIENT} make: material {FeEdition | NULL}
	"Make a new SingleRef. At least one of the parameters must be non-NULL. The originalContext, if supplied,  must be a frozen Work."
	^self make: material with: NULL with: NULL with: NULL!
*/
}
/**
 * Make a new SingleRef. At least one of the parameters must be non-NULL. The
 * originalContext, if supplied,  must be a frozen Work.
 */
public static FeSingleRef make(FeEdition material, FeWork workContext) {
	return make(material, workContext, null, null);
/*
udanax-top.st:24510:FeSingleRef class methodsFor: 'smalltalk: defaults'!
{FeSingleRef CLIENT} make: material {FeEdition | NULL}
	with: workContext {FeWork default: NULL}
	"Make a new SingleRef. At least one of the parameters must be non-NULL. The originalContext, if supplied,  must be a frozen Work."
	^self make: material with: workContext with: NULL with: NULL!
*/
}
/**
 * Make a new SingleRef. At least one of the parameters must be non-NULL. The
 * originalContext, if supplied,  must be a frozen Work.
 */
public static FeSingleRef make(FeEdition material, FeWork workContext, FeWork originalContext) {
	return make(material, workContext, originalContext, null);
/*
udanax-top.st:24516:FeSingleRef class methodsFor: 'smalltalk: defaults'!
{FeSingleRef CLIENT} make: material {FeEdition | NULL}
	with: workContext {FeWork default: NULL}
	with: originalContext {FeWork default: NULL}
	"Make a new SingleRef. At least one of the parameters must be non-NULL. The originalContext, if supplied,  must be a frozen Work."
	^self make: material with: workContext with: originalContext with: NULL!
*/
}
public FeSingleRef() {
/*

Generated during transformation
*/
}
public FeSingleRef(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
