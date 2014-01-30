/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nlinks;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nlinks.FeHyperLink;
import info.dgjones.abora.gold.nlinks.FeHyperRef;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.wrapper.FeSet;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Contains a named table of HyperRefs and a set of Works which describe the usage and/or
 * format of the link.
 */
public class FeHyperLink extends FeWrapper {

	protected static FeWrapperSpec TheHyperLinkSpec;
/*
udanax-top.st:23883:
FeWrapper subclass: #FeHyperLink
	instanceVariableNames: ''
	classVariableNames: 'TheHyperLinkSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nlinks'!
*/
/*
udanax-top.st:23887:
FeHyperLink comment:
'Contains a named table of HyperRefs and a set of Works which describe the usage and/or format of the link.'!
*/
/*
udanax-top.st:23889:
(FeHyperLink getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:23942:
FeHyperLink class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:23945:
(FeHyperLink getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeHyperLink.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Get the HyperRef at the given name; blast if none there
 */
public FeHyperRef endAt(Sequence name) {
	if (name.isEqual((Sequence.string("Link:LinkTypes")))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_USE_DIFFERENT_LINK_END_KEY);
	}
	return (FeHyperRef) (FeHyperRef.spec().wrap(((FeEdition) (edition().get(name)))));
/*
udanax-top.st:23894:FeHyperLink methodsFor: 'accessing'!
{FeHyperRef CLIENT} endAt: name {Sequence}
	"Get the HyperRef at the given name; blast if none there"
	(name isEqual: (Sequence string: 'Link:LinkTypes')) ifTrue:
		[Heaper BLAST: #MustUseDifferentLinkEndKey].
	^(FeHyperRef spec wrap: ((self edition get: name) cast: FeEdition)) cast: FeHyperRef!
*/
}
/**
 * The names of all of the ends of this link
 */
public SequenceRegion endNames() {
	return (SequenceRegion) (edition().domain().without((Sequence.string("HyperLink:LinkTypes"))));
/*
udanax-top.st:23901:FeHyperLink methodsFor: 'accessing'!
{SequenceRegion CLIENT} endNames
	"The names of all of the ends of this link"
	
	^(self edition domain without: (Sequence string: 'HyperLink:LinkTypes')) cast: SequenceRegion!
*/
}
/**
 * The various type documents describing this kind of Link. These documents are typically
 * Editions with descriptions at each linkEnd key describing what is at that Link End.
 * The reason for having several is to allow type hierarchies to be constructed and searched
 * for, by including all super types of a link in its link type list.
 * The Link should be endorsed with all the IDs of all the types.
 * What if someone endorses it further (or unendorses it?)
 */
public FeSet linkTypes() {
	return (FeSet) (FeSet.spec().wrap(((FeEdition) (edition().get((Sequence.string("Link:LinkTypes")))))));
/*
udanax-top.st:23906:FeHyperLink methodsFor: 'accessing'!
{FeSet CLIENT of: FeWork} linkTypes
	"The various type documents describing this kind of Link. These documents are typically Editions with descriptions at each linkEnd key describing what is at that Link End.
	The reason for having several is to allow type hierarchies to be constructed and searched for, by including all super types of a link in its link type list.
	The Link should be endorsed with all the IDs of all the types.
	What if someone endorses it further (or unendorses it?)"
	^(FeSet spec wrap: ((self edition
		get: (Sequence string: 'Link:LinkTypes')) cast: FeEdition)) cast: FeSet!
*/
}
/**
 * Change/add a Link end
 */
public FeHyperLink withEnd(Sequence name, FeHyperRef linkEnd) {
	if (name.isEqual((Sequence.string("Link:LinkTypes")))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_USE_DIFFERENT_LINK_END_NAME);
	}
	return FeHyperLink.construct((edition().with(name, linkEnd.edition())));
/*
udanax-top.st:23915:FeHyperLink methodsFor: 'accessing'!
{FeHyperLink CLIENT} withEnd: name {Sequence} with: linkEnd {FeHyperRef}
	"Change/add a Link end"
	(name isEqual: (Sequence string: 'Link:LinkTypes')) ifTrue:
		[Heaper BLAST: #MustUseDifferentLinkEndName].
	^FeHyperLink construct: (self edition with: name with: linkEnd edition)!
*/
}
/**
 * Replace the set of type documents describing this kind of Link
 */
public FeHyperLink withLinkTypes(FeSet types) {
	return FeHyperLink.construct((edition().with((Sequence.string("Link:LinkTypes")), types.edition())));
/*
udanax-top.st:23922:FeHyperLink methodsFor: 'accessing'!
{FeHyperLink CLIENT} withLinkTypes: types {FeSet of: FeWork}
	"Replace the set of type documents describing this kind of Link"
	^FeHyperLink construct: (self edition
		with: (Sequence string: 'Link:LinkTypes') with: types edition)!
*/
}
/**
 * Remove a Link end
 */
public FeHyperLink withoutEnd(Sequence name) {
	if (name.isEqual((Sequence.string("Link:LinkTypes")))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_USE_DIFFERENT_LINK_END_NAME);
	}
	return FeHyperLink.construct((edition().without(name)));
/*
udanax-top.st:23928:FeHyperLink methodsFor: 'accessing'!
{FeHyperLink CLIENT} withoutEnd: name {Sequence}
	"Remove a Link end"
	(name isEqual: (Sequence string: 'Link:LinkTypes')) ifTrue:
		[Heaper BLAST: #MustUseDifferentLinkEndName].
	^FeHyperLink construct: (self edition without: name)!
*/
}
public FeHyperLink(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:23937:FeHyperLink methodsFor: 'private: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
/**
 * Check that it has the right fields in the right places. Ignore other contents.
 */
public static boolean check(FeEdition edition) {
	if ( ! ((FeWrapper.checkDomainHas(edition, (Sequence.string("Link:LinkTypes")).asRegion())) && ((FeWrapper.checkSubEdition(edition, (Sequence.string("Link:LinkTypes")), FeSet.spec(), false)) && (FeWrapper.checkSubEditions(edition, (edition.domain().without((Sequence.string("Link:LinkTypes")))), FeHyperRef.spec(), true))))) {
		return false;
	}
	if (edition.includesKey((Sequence.string("Link:LinkTypes")))) {
		FeEdition sub;
		sub = (FeEdition) (edition.get((Sequence.string("Link:LinkTypes"))));
		Stepper stomper = sub.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			FeRangeElement r = (FeRangeElement) stomper.fetch();
			if (r == null) {
				continue ;
			}
			if ( ! ((r instanceof FeEdition) && (FeHyperRef.spec().certify(((FeEdition) r))))) {
				return false;
			}
		}
		stomper.destroy();
	}
	return true;
/*
udanax-top.st:23950:FeHyperLink class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	"Check that it has the right fields in the right places. Ignore other contents."
	
	((FeWrapper checkDomainHas: edition
			with: (Sequence string: 'Link:LinkTypes') asRegion)
		and: [(FeWrapper checkSubEdition: edition
			with: (Sequence string: 'Link:LinkTypes')
			with: FeSet spec
			with: false)
		and: [FeWrapper checkSubEditions: edition
			with: (edition domain without: (Sequence string: 'Link:LinkTypes'))
			with: FeHyperRef spec
			with: true]])
		ifFalse: [^false].
	(edition includesKey: (Sequence string: 'Link:LinkTypes')) ifTrue:
		[ | sub {FeEdition} |
		sub := (edition get: (Sequence string: 'Link:LinkTypes')) cast: FeEdition.
		sub stepper forEach: [ :r {FeRangeElement} |
			((r isKindOf: FeEdition) and: [FeHyperRef spec certify: (r cast: FeEdition)])
				ifFalse: [^false]]].
	^true!
*/
}
public static FeHyperLink construct(FeEdition edition) {
	spec().endorse(edition);
	edition.endorse((FeServer.endorsementRegion(((IDRegion) ((ID) CurrentAuthor.fluidGet()).asRegion()), (FeServer.iDsOfRange(((FeEdition) (edition.get((Sequence.string("Link:LinkTypes"))))))))));
	return (FeHyperLink) (makeWrapper(edition));
/*
udanax-top.st:23972:FeHyperLink class methodsFor: 'private: wrapping'!
{FeHyperLink} construct: edition {FeEdition}
	self spec endorse: edition.
	edition endorse: (FeServer
		endorsementRegion: (CurrentAuthor fluidGet asRegion cast: IDRegion)
		with: (FeServer iDsOfRange: ((edition get: (Sequence string: 'Link:LinkTypes')) cast: FeEdition))).
	^(self makeWrapper: edition) cast: FeHyperLink!
*/
}
/**
 * Just create a new wrapper
 */
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeHyperLink(edition, spec());
/*
udanax-top.st:23980:FeHyperLink class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	"Just create a new wrapper"
	
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheHyperLinkSpec = wrap;
/*
udanax-top.st:23985:FeHyperLink class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheHyperLinkSpec := wrap.!
*/
}
/**
 * A Filter for links of the specified types
 */
public static Filter linkFilter(IDRegion types) {
	throw new UnimplementedException();
/*
udanax-top.st:23991:FeHyperLink class methodsFor: 'pseudo constructors'!
{Filter} linkFilter: types {IDRegion}
	"A Filter for links of the specified types"
	
	self unimplemented.
	^NULL "fodder"!
*/
}
/**
 * Make a standard two-ended link
 */
public static FeHyperLink make(FeSet types, FeHyperRef leftEnd, FeHyperRef rightEnd) {
	PtrArray values;
	Stepper stomper = types.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		FeRangeElement t = (FeRangeElement) stomper.fetch();
		if (t == null) {
			continue ;
		}
		if ( ! (t instanceof FeWork)) {
			throw new AboraRuntimeException(AboraRuntimeException.INVALID_PARAMETER);
		}
	}
	stomper.destroy();
	values = PtrArray.nulls(3);
	/* Put the values in the array in alphabetical order of keys */
	values.store(0, leftEnd.edition());
	values.store(1, types.edition());
	values.store(2, rightEnd.edition());
	return construct((FeEdition.fromArray(values, (((Sequence.string("Link:LinkTypes")).asRegion().with((Sequence.string("Link:LeftEnd")))).with((Sequence.string("Link:RightEnd")))), SequenceSpace.make().getAscending())));
/*
udanax-top.st:23997:FeHyperLink class methodsFor: 'pseudo constructors'!
{FeHyperLink CLIENT} make: types {FeSet}
	with: leftEnd {FeHyperRef}
	with: rightEnd {FeHyperRef}
	"Make a standard two-ended link"
	
	| values {PtrArray of: FeEdition} |
	types stepper forEach: [ :t {FeRangeElement} |
		(t isKindOf: FeWork) ifFalse:
			[Heaper BLAST: #InvalidParameter]].
	values := PtrArray nulls: 3.
	"Put the values in the array in alphabetical order of keys"
	values at: Int32Zero store: leftEnd edition.
	values at: 1 store: types edition.
	values at: 2 store: rightEnd edition.
	^self construct: (FeEdition
		fromArray: values
		with: (((Sequence string: 'Link:LinkTypes') asRegion
			with: (Sequence string: 'Link:LeftEnd'))
			with: (Sequence string: 'Link:RightEnd'))
		with: SequenceSpace make getAscending)!
*/
}
public static FeWrapperSpec spec() {
	return TheHyperLinkSpec;
/*
udanax-top.st:24018:FeHyperLink class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheHyperLinkSpec!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("HyperLink", "Wrapper", FE_HYPER_LINK);
/*
udanax-top.st:24024:FeHyperLink class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'HyperLink' with: 'Wrapper' with: #FeHyperLink.!
*/
}
public static void linkTimeNonInherited() {
	TheHyperLinkSpec = null;
/*
udanax-top.st:24028:FeHyperLink class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheHyperLinkSpec := NULL.!
*/
}
/**
 * {FeHyperRef CLIENT} endAt: name {Sequence}
 * {SequenceRegion CLIENT} endNames
 * {FeSet CLIENT of: FeWork} linkTypes
 * {FeHyperLink CLIENT} withEnd: name {Sequence} with: linkEnd {FeHyperRef}
 * {FeHyperLink CLIENT} withLinkTypes: types {FeSet of: FeWork}
 * {FeHyperLink CLIENT} withoutEnd: name {Sequence}
 */
public static void infostProtocol() {
/*
udanax-top.st:24034:FeHyperLink class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeHyperRef CLIENT} endAt: name {Sequence}
{SequenceRegion CLIENT} endNames
{FeSet CLIENT of: FeWork} linkTypes
{FeHyperLink CLIENT} withEnd: name {Sequence} with: linkEnd {FeHyperRef}
{FeHyperLink CLIENT} withLinkTypes: types {FeSet of: FeWork}
{FeHyperLink CLIENT} withoutEnd: name {Sequence}
"!
*/
}
public FeHyperLink() {
/*

Generated during transformation
*/
}
public FeHyperLink(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
