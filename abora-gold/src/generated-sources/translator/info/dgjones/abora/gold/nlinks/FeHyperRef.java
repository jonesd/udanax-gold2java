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
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nlinks.FeHyperRef;
import info.dgjones.abora.gold.nlinks.FePath;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Represents a single attachment to some material in context.
 */
public class FeHyperRef extends FeWrapper {

	protected static FeWrapperSpec TheHyperRefSpec;
/*
udanax-top.st:24043:
FeWrapper subclass: #FeHyperRef
	instanceVariableNames: ''
	classVariableNames: 'TheHyperRefSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nlinks'!
*/
/*
udanax-top.st:24047:
FeHyperRef comment:
'Represents a single attachment to some material in context.'!
*/
/*
udanax-top.st:24049:
(FeHyperRef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:24114:
FeHyperRef class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:24117:
(FeHyperRef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeHyperRef.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * A Work frozen on the contents of the Work at the time the HyperRef was made
 */
public FeWork originalContext() {
	return (FeWork) (edition().get((Sequence.string("HyperRef:OriginalContext"))));
/*
udanax-top.st:24054:FeHyperRef methodsFor: 'accessing'!
{FeWork CLIENT} originalContext
	"A Work frozen on the contents of the Work at the time the HyperRef was made"
	^(self edition get: (Sequence string: 'HyperRef:OriginalContext')) cast: FeWork!
*/
}
/**
 * The path of labels down from the top-level Edition
 */
public FePath pathContext() {
	return (FePath) (FePath.spec().wrap(((FeEdition) (edition().get((Sequence.string("HyperRef:PathContext")))))));
/*
udanax-top.st:24059:FeHyperRef methodsFor: 'accessing'!
{FePath CLIENT} pathContext
	"The path of labels down from the top-level Edition"
	^(FePath spec wrap: ((self edition
		get: (Sequence string: 'HyperRef:PathContext')) cast: FeEdition)) cast: FePath!
*/
}
/**
 * Change (or remove if NULL) the originalContext
 */
public FeHyperRef withOriginalContext(FeWork work) {
	if (work == null) {
		return makeNew((edition().without((Sequence.string("HyperRef:OriginalContext")))));
	}
	else {
		if (((BeWork) work.fetchBe()).fetchEditClub() != null) {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_FROZEN);
		}
		return makeNew((edition().with((Sequence.string("HyperRef:OriginalContext")), work)));
	}
/*
udanax-top.st:24065:FeHyperRef methodsFor: 'accessing'!
{FeHyperRef CLIENT} withOriginalContext: work {FeWork | NULL}
	"Change (or remove if NULL) the originalContext"
	work == NULL ifTrue:
		[^self makeNew: (self edition without: (Sequence string: 'HyperRef:OriginalContext'))]
	ifFalse:
		[(work fetchBe cast: BeWork) fetchEditClub ~~ NULL
			ifTrue: [Heaper BLAST: #MustBeFrozen].
		^self makeNew: (self edition
			with: (Sequence string: 'HyperRef:OriginalContext')
			with: work)]!
*/
}
/**
 * Change (or remove if NULL) the pathContext
 */
public FeHyperRef withPathContext(FePath path) {
	if (path == null) {
		return makeNew((edition().without((Sequence.string("HyperRef:PathContext")))));
	}
	else {
		return makeNew((edition().with((Sequence.string("HyperRef:PathContext")), path.edition())));
	}
/*
udanax-top.st:24077:FeHyperRef methodsFor: 'accessing'!
{FeHyperRef CLIENT} withPathContext: path {FePath | NULL}
	"Change (or remove if NULL) the pathContext"
	path == NULL ifTrue:
		[^self makeNew: (self edition without: (Sequence string: 'HyperRef:PathContext'))]
	ifFalse:
		[^self makeNew: (self edition
			with: (Sequence string: 'HyperRef:PathContext')
			with: path edition)]!
*/
}
/**
 * Change (or remove if NULL) the workContext
 */
public FeHyperRef withWorkContext(FeWork work) {
	if (work == null) {
		return makeNew((edition().without((Sequence.string("HyperRef:WorkContext")))));
	}
	else {
		return makeNew((edition().with((Sequence.string("HyperRef:WorkContext")), work)));
	}
/*
udanax-top.st:24087:FeHyperRef methodsFor: 'accessing'!
{FeHyperRef CLIENT} withWorkContext: work {FeWork | NULL}
	"Change (or remove if NULL) the workContext"
	work == NULL ifTrue:
		[^self makeNew: (self edition without: (Sequence string: 'HyperRef:WorkContext'))]
	ifFalse:
		[^self makeNew: (self edition
			with: (Sequence string: 'HyperRef:WorkContext')
			with: work)]!
*/
}
/**
 * The Work whose state this is attached to.
 */
public FeWork workContext() {
	return (FeWork) (edition().get((Sequence.string("HyperRef:WorkContext"))));
/*
udanax-top.st:24097:FeHyperRef methodsFor: 'accessing'!
{FeWork CLIENT} workContext
	"The Work whose state this is attached to."
	^(self edition get: (Sequence string: 'HyperRef:WorkContext')) cast: FeWork!
*/
}
public FeHyperRef(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:24104:FeHyperRef methodsFor: 'protected: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
/**
 * Make a new HyperRef of the same type with different contents
 */
public FeHyperRef makeNew(FeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:24108:FeHyperRef methodsFor: 'protected: create'!
{FeHyperRef} makeNew: edition {FeEdition}
	"Make a new HyperRef of the same type with different contents"
	
	self subclassResponsibility!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.ABSTRACTWRAPPER("HyperRef", "Wrapper", FE_HYPER_REF);
/*
udanax-top.st:24122:FeHyperRef class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	FeWrapperSpec ABSTRACTWRAPPER: 'HyperRef' with: 'Wrapper' with: #FeHyperRef!
*/
}
public static void linkTimeNonInherited() {
	TheHyperRefSpec = null;
/*
udanax-top.st:24126:FeHyperRef class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	TheHyperRefSpec := NULL.!
*/
}
/**
 * Check that it has the right fields in the right places. Ignore other contents.
 */
public static boolean check(FeEdition edition) {
	return (edition.coordinateSpace().isEqual(SequenceSpace.make())) && ((edition.domain().intersects((((Sequence.string("HyperRef:PathContext")).asRegion().with((Sequence.string("HyperRef:WorkContext")))).with((Sequence.string("HyperRef:OriginalContext")))))) && ((FeWrapper.checkSubWork(edition, (Sequence.string("HyperRef:WorkContext")), false)) && ((FeWrapper.checkSubWork(edition, (Sequence.string("HyperRef:OriginalContext")), false)) && ((FeWrapper.checkSubEdition(edition, (Sequence.string("HyperRef:PathContext")), FePath.spec(), false))))));
/*
udanax-top.st:24132:FeHyperRef class methodsFor: 'protected: wrapping'!
{BooleanVar} check: edition {FeEdition}
	"Check that it has the right fields in the right places. Ignore other contents."
	
	^(edition coordinateSpace isEqual: SequenceSpace make)
		and: [(edition domain
			intersects: (((Sequence string: 'HyperRef:PathContext') asRegion
				with: (Sequence string: 'HyperRef:WorkContext'))
				with: (Sequence string: 'HyperRef:OriginalContext')))
		and: [(FeWrapper checkSubWork: edition
			with: (Sequence string: 'HyperRef:WorkContext')
			with: false)
		and: [(FeWrapper checkSubWork: edition
			with: (Sequence string: 'HyperRef:OriginalContext')
			with: false)
		and: [(FeWrapper checkSubEdition: edition
			with: (Sequence string: 'HyperRef:PathContext')
			with: FePath spec
			with: false)]]]]!
*/
}
public static void setSpec(FeWrapperSpec spec) {
	TheHyperRefSpec = spec;
/*
udanax-top.st:24151:FeHyperRef class methodsFor: 'protected: wrapping'!
{void} setSpec: spec {FeWrapperSpec}
	TheHyperRefSpec := spec.!
*/
}
public static FeWrapperSpec spec() {
	return TheHyperRefSpec;
/*
udanax-top.st:24157:FeHyperRef class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheHyperRefSpec!
*/
}
/**
 * {FeWork CLIENT} originalContext
 * {FePath CLIENT} pathContext
 * {FeHyperRef CLIENT} withOriginalContext: work {FeWork | NULL}
 * {FeHyperRef CLIENT} withPathContext: path {FePath | NULL}
 * {FeHyperRef CLIENT} withWorkContext: work {FeWork | NULL}
 * {FeWork CLIENT} workContext
 */
public static void infostProtocol() {
/*
udanax-top.st:24163:FeHyperRef class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeWork CLIENT} originalContext
{FePath CLIENT} pathContext
{FeHyperRef CLIENT} withOriginalContext: work {FeWork | NULL}
{FeHyperRef CLIENT} withPathContext: path {FePath | NULL}
{FeHyperRef CLIENT} withWorkContext: work {FeWork | NULL}
{FeWork CLIENT} workContext
"!
*/
}
public FeHyperRef() {
/*

Generated during transformation
*/
}
public FeHyperRef(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
