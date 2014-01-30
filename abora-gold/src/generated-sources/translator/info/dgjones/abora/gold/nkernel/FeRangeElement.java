/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BeCarrier;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeLabel;
import info.dgjones.abora.gold.nkernel.FePlaceHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeVirtualDataHolder;
import info.dgjones.abora.gold.nkernel.FeVirtualPlaceHolder;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * The kinds of objects which can be in the range of Editions.
 */
public class FeRangeElement extends Heaper {

/*
udanax-top.st:20253:
Heaper subclass: #FeRangeElement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:20257:
FeRangeElement comment:
'The kinds of objects which can be in the range of Editions.'!
*/
/*
udanax-top.st:20259:
(FeRangeElement getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:20463:
FeRangeElement class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:20466:
(FeRangeElement getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeRangeElement.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public FeEdition works() {
	return works(null, 0, null);
/*
udanax-top.st:20264:FeRangeElement methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} works
	
	^self works: NULL with: 0 with: NULL!
*/
}
public FeEdition works(Filter filter) {
	return works(filter, 0, null);
/*
udanax-top.st:20268:FeRangeElement methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} works: filter {Filter default: NULL}
	
	^self works: filter with: 0 with: NULL!
*/
}
public FeEdition works(Filter filter, int flags) {
	return works(filter, flags, null);
/*
udanax-top.st:20272:FeRangeElement methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} works: filter {Filter default: NULL}
	with: flags {Int32 default: Int32Zero}
	
	^self works: filter with: flags with: NULL!
*/
}
/**
 * Essential.  When this PlaceHolder becomes any other kind of RangeElement, then the
 * Detector will be triggered with the new RangeElement. If this is already not a
 * PlaceHolder, then the Detector is triggered immediately with this RangeElement.
 * See FillRangeDetector::filled (RangeElement * newIdentity).
 */
public void addFillDetector(FeFillDetector detector) {
	detector.filled(this);
	/* default will be overridden in FePlaceHolder */
/*
udanax-top.st:20279:FeRangeElement methodsFor: 'accessing'!
{void} addFillDetector: detector {FeFillDetector}
	"Essential.  When this PlaceHolder becomes any other kind of RangeElement, then the Detector will be triggered with the new RangeElement. If this is already not a PlaceHolder, then the Detector is triggered immediately with this RangeElement.
	See FillRangeDetector::filled (RangeElement * newIdentity)."
	
	detector filled: self. "default will be overridden in FePlaceHolder"!
*/
}
/**
 * Essential.  An object reflecting the current identity of this object, in case it is a
 * PlaceHolder that has become something else since it was received from the Server.
 */
public FeRangeElement again() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:20285:FeRangeElement methodsFor: 'accessing'!
{FeRangeElement CLIENT} again
	"Essential.  An object reflecting the current identity of this object, in case it is a PlaceHolder that has become something else since it was received from the Server."
	
	self subclassResponsibility!
*/
}
/**
 * Essential.  Whether the identity of this object could be changed to the other.
 * Does not check whether the CurrentKeyMaster has authority to do it.
 * The restrictions on this operation depend on which subclass this is, but in general
 * (except for PlaceHolders) an object can only become another of the same type with the same
 * content.
 */
public boolean canMakeIdentical(FeRangeElement newIdentity) {
	RaviNow.shouldImplement();
	return false;
/*
udanax-top.st:20290:FeRangeElement methodsFor: 'accessing'!
{BooleanVar CLIENT} canMakeIdentical: newIdentity {FeRangeElement}
	"Essential.  Whether the identity of this object could be changed to the other.
	Does not check whether the CurrentKeyMaster has authority to do it.
	The restrictions on this operation depend on which subclass this is, but in general (except for PlaceHolders) an object can only become another of the same type with the same content."
	
	RaviNow shouldImplement.
	^false "fodder"!
*/
}
/**
 * Essential.  Return a FillDetector that will be triggered when this RangeElement becomes
 * something other than a PlaceHolder, or immeditely if this RangeElement is not currently a
 * PlaceHolder.
 * See FillRangeDetector::filled (RangeElement * newIdentity).
 */
public FeFillDetector fillDetector() {
	Dean.shouldImplement();
	addFillDetector(null);
	return null;
/*
udanax-top.st:20298:FeRangeElement methodsFor: 'accessing'!
{FeFillDetector CLIENT} fillDetector
	"Essential.  Return a FillDetector that will be triggered when this RangeElement becomes something other than a PlaceHolder, or immeditely if this RangeElement is not currently a PlaceHolder.
	See FillRangeDetector::filled (RangeElement * newIdentity)."
	
	Dean shouldImplement.
	self addFillDetector: NULL.
	^NULL "fodder"!
*/
}
/**
 * Essential.  Return whether two objects have the same identity on the Server.  Note that
 * this can change over time, if makeIdentical is used.  However, for a given pair of
 * FeRangeElements, it can only change from not being the same to being the same while you
 * are holding onto them.
 */
public boolean isIdentical(FeRangeElement other) {
	if (other instanceof FeVirtualDataHolder) {
		FeVirtualDataHolder vd = (FeVirtualDataHolder) other;
		return vd.isIdentical(this);
	}
	else if (other instanceof FeVirtualPlaceHolder) {
		FeVirtualPlaceHolder vp = (FeVirtualPlaceHolder) other;
		return vp.isIdentical(this);
	}
	else {
		/* This should be OK, since virtual subclasses override this anyway */
		return getOrMakeBe().isEqual(other.getOrMakeBe());
	}
/*
udanax-top.st:20306:FeRangeElement methodsFor: 'accessing'!
{BooleanVar CLIENT} isIdentical: other {FeRangeElement}
	"Essential.  Return whether two objects have the same identity on the Server.  Note that this can change over time, if makeIdentical is used.  However, for a given pair of FeRangeElements, it can only change from not being the same to being the same while you are holding onto them."
	
	other cast: FeVirtualDataHolder into: [ :vd |
		^vd isIdentical: self]
	cast: FeVirtualPlaceHolder into: [ :vp |
		^vp isIdentical: self]
	others:
		["This should be OK, since virtual subclasses override this anyway"
		^self getOrMakeBe isEqual: other getOrMakeBe].
	^false "fodder"!
*/
}
/**
 * Essential.  Change the identity of this object to the other. BLAST if unsuccessful.
 * Requires authority of the current owner; if the operation is successful, the owner will
 * appear to change to that of the other object.
 * Also requires enough permission on newIdentity to determine, by comparing content, whether
 * the operation would succeed.
 * The restrictions on this operation depend on which subclass this is, but in general
 * (except for PlaceHolders) an object can only become another of the same type with the same
 * content.
 */
public void makeIdentical(FeRangeElement newIdentity) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:20318:FeRangeElement methodsFor: 'accessing'!
{void CLIENT} makeIdentical: newIdentity {FeRangeElement}
	"Essential.  Change the identity of this object to the other. BLAST if unsuccessful.
	Requires authority of the current owner; if the operation is successful, the owner will appear to change to that of the other object.
	Also requires enough permission on newIdentity to determine, by comparing content, whether the operation would succeed.
	The restrictions on this operation depend on which subclass this is, but in general (except for PlaceHolders) an object can only become another of the same type with the same content."
	
	self subclassResponsibility!
*/
}
/**
 * Essential.  The Club which owns this RangeElement, and has the authority to make it become
 * something else, and to transfer ownership to someone else.
 */
public ID owner() {
	return getOrMakeBe().owner();
/*
udanax-top.st:20326:FeRangeElement methodsFor: 'accessing'!
{ID CLIENT} owner
	"Essential.  The Club which owns this RangeElement, and has the authority to make it become something else, and to transfer ownership to someone else."
	
	^self getOrMakeBe owner "virtuals should override"!
*/
}
/**
 * Essential.  Remove a Detector which had been added to this RangeElement. You should remove
 * every Detector you add, although they will go away automatically when a client session
 * terminates.
 */
public void removeFillDetector(FeFillDetector detector) {
	/* Do nothing. PlaceHolder overrides */
/*
udanax-top.st:20331:FeRangeElement methodsFor: 'accessing'!
{void} removeFillDetector: detector {FeFillDetector}
	"Essential.  Remove a Detector which had been added to this RangeElement. You should remove every Detector you add, although they will go away automatically when a client session terminates."
	
	||
	"Do nothing. PlaceHolder overrides"!
*/
}
/**
 * Essential.  Change the owner; must have the authority of the current owner.
 */
public void setOwner(ID clubID) {
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	/* Need to make it into a reified range element in order to have distinct ownership */
	((BeGrandMap) CurrentGrandMap.fluidGet()).getClub(clubID);
	/* Checks that it is a club. */
	getOrMakeBe().setOwner(clubID);
/*
udanax-top.st:20337:FeRangeElement methodsFor: 'accessing'!
{void CLIENT} setOwner: clubID {ID}
	"Essential.  Change the owner; must have the authority of the current owner."
	
	(CurrentKeyMaster fluidGet hasAuthority: self owner) ifFalse:
		[Heaper BLAST: #MustBeOwner].
	"Need to make it into a reified range element in order to have distinct ownership"
	
	CurrentGrandMap fluidGet getClub: clubID.  "Checks that it is a club."
	self getOrMakeBe setOwner: clubID!
*/
}
/**
 * All Editions which the CurrentKeyMaster can see, which transclude this RangeElement.
 * If a directFilter is given, then the visibleEndorsements on a Edition must match the
 * filter.
 * If an indirectFilter is given, then a resulting Edition must be contained in some readable
 * Edition whose visibleEndorsements match the filter.
 * If the directContainersOnly flag is set, then a resulting Edition must contain this
 * directly as a RangeElement; otherwise, indirect containment through Editions is allowed.
 * If the localPresentOnly flag is set, then only Editions currently known to this Server are
 * guaranteed to end up in the result; otherwise, Editions which come to satisfy the
 * conditions in the future, and those on other Servers, may also be found.
 * Equivalent to
 * FeServer::current ()->newEditionWith (<any position>, this)
 * ->rangeTranscluders (NULL, directFilter, indirectFilter, flags, otherTranscluders).
 */
public FeEdition transcluders(Filter directFilter, Filter indirectFilter, int flags, FeEdition otherTranscluders) {
	return (FeEdition.fromOne(IntegerPos.make(0), this)).rangeTranscluders(null, directFilter, indirectFilter, flags, otherTranscluders);
/*
udanax-top.st:20347:FeRangeElement methodsFor: 'accessing'!
{FeEdition CLIENT} transcluders: directFilter {Filter default: NULL}
	with: indirectFilter {Filter default: NULL}
	with: flags {Int32 default: Int32Zero}
	with: otherTranscluders {FeEdition default: NULL}
	"All Editions which the CurrentKeyMaster can see, which transclude this RangeElement.
	If a directFilter is given, then the visibleEndorsements on a Edition must match the filter.
	If an indirectFilter is given, then a resulting Edition must be contained in some readable Edition whose visibleEndorsements match the filter.
	If the directContainersOnly flag is set, then a resulting Edition must contain this directly as a RangeElement; otherwise, indirect containment through Editions is allowed.
	If the localPresentOnly flag is set, then only Editions currently known to this Server are guaranteed to end up in the result; otherwise, Editions which come to satisfy the conditions in the future, and those on other Servers, may also be found.
	Equivalent to
		FeServer::current ()->newEditionWith (<any position>, this)
			->rangeTranscluders (NULL, directFilter, indirectFilter, flags, otherTranscluders)."
	
	^(FeEdition fromOne: IntegerVarZero integer with: self)
		rangeTranscluders: NULL with: directFilter with: indirectFilter with: flags with: otherTranscluders!
*/
}
/**
 * Essential.  Works which contain this RangeElement and can be read by the CurrentKeyMaster.
 * Returns an IDSpace Edition full of PlaceHolders, which will be filled with Works as
 * results come in.
 * If a filter is given, then only Works whose endorsements pass the Filter are returned.
 * If localPresentOnly flag is set, then only Works currently known to this Server are
 * returned; otherwise, as new Works come to be known to the Server, they are filled into the
 * resulting Edition.
 * If directContainersOnly is set, and this is an Edition, then only Works which are directly
 * on this Edition are returned (and not Works which are on Editions which have this one as
 * sub-Editions).
 * { <k,l,w> | w's contains self, w passes filter}
 */
public FeEdition works(Filter filter, int flags, FeEdition otherTranscluders) {
	Filter theFilter;
	if (filter == null) {
		theFilter = (Filter) ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementFilterSpace().fullRegion();
	}
	else {
		theFilter = filter;
	}
	Dean.thingToDo();
	/* avoid reifying */
	return FeEdition.on((getOrMakeBe().works(((FeKeyMaster) CurrentKeyMaster.fluidGet()).actualAuthority(), theFilter, flags)));
/*
udanax-top.st:20363:FeRangeElement methodsFor: 'accessing'!
{FeEdition CLIENT} works: filter {Filter default: NULL}
	with: flags {Int32 default: Int32Zero}
	with: otherTranscluders {FeEdition default: NULL}
	"Essential.  Works which contain this RangeElement and can be read by the CurrentKeyMaster. Returns an IDSpace Edition full of PlaceHolders, which will be filled with Works as results come in.
	If a filter is given, then only Works whose endorsements pass the Filter are returned.
	If localPresentOnly flag is set, then only Works currently known to this Server are returned; otherwise, as new Works come to be known to the Server, they are filled into the resulting Edition.
	If directContainersOnly is set, and this is an Edition, then only Works which are directly on this Edition are returned (and not Works which are on Editions which have this one as sub-Editions).
	{ <k,l,w> | w's contains self, w passes filter}"
	
	| theFilter {Filter} |
	filter == NULL
		ifTrue: [theFilter := CurrentGrandMap fluidGet endorsementFilterSpace fullRegion cast: Filter]
		ifFalse: [theFilter := filter].
	Dean thingToDo. "avoid reifying"
	^FeEdition on: (self getOrMakeBe
		works: CurrentKeyMaster fluidGet actualAuthority
		with: theFilter
		with: flags)!
*/
}
/**
 * Return an object that wraps up any run-time state that might be needed inside the Be
 * system.  Right now that means labels.
 */
public BeCarrier carrier() {
	return BeCarrier.make(getOrMakeBe());
/*
udanax-top.st:20384:FeRangeElement methodsFor: 'server accessing'!
{BeCarrier} carrier
	"Return an object that wraps up any run-time state that might be needed inside the Be system.  Right now that means labels."
	
	^BeCarrier make: self getOrMakeBe!
*/
}
/**
 * If this has a reified Be object, then return it, else NULL
 */
public BeRangeElement fetchBe() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:20389:FeRangeElement methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	"If this has a reified Be object, then return it, else NULL"
	
	self subclassResponsibility!
*/
}
/**
 * An individual BeRangeElement for this identity. If the object is virtualized, then
 * de-virtualizes it.
 */
public BeRangeElement getOrMakeBe() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:20394:FeRangeElement methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	"An individual BeRangeElement for this identity. If the object is virtualized, then de-virtualizes it."
	
	self subclassResponsibility!
*/
}
/*
udanax-top.st:20401:FeRangeElement methodsFor: 'smalltalk:'!
inspect
	"Sensor leftShiftDown" true
		ifTrue: [self basicInspect]
		ifFalse: [EntView openOn: (TreeBarnacle new
					buildOn: self
					gettingChildren: [:elem | (elem respondsTo: #inspectPieces)
							ifTrue: [elem inspectPieces]
							ifFalse: [#()]]
					gettingImage: [:me | DisplayText text: me displayString asText textStyle: (TextStyle styleNamed: #small)]
					at: 0 @ 0
					vertical: true
					separation: 5 @ 10)]!
*/
public FeEdition transcluders() {
	return transcluders(null, null, 0, null);
/*
udanax-top.st:20414:FeRangeElement methodsFor: 'smalltalk:'!
{FeEdition CLIENT} transcluders
	^self transcluders: NULL with: NULL with: Int32Zero with: NULL!
*/
}
public FeEdition transcluders(Filter directFilter) {
	return transcluders(directFilter, null, 0, null);
/*
udanax-top.st:20417:FeRangeElement methodsFor: 'smalltalk:'!
{FeEdition CLIENT} transcluders: directFilter {Filter default: NULL}
	
	^self transcluders: directFilter with: NULL with: Int32Zero with: NULL!
*/
}
public FeEdition transcluders(Filter directFilter, Filter indirectFilter) {
	return transcluders(directFilter, indirectFilter, 0, null);
/*
udanax-top.st:20421:FeRangeElement methodsFor: 'smalltalk:'!
{FeEdition CLIENT} transcluders: directFilter {Filter default: NULL}
	with: indirectFilter {Filter default: NULL}
	
	^self transcluders: directFilter with: indirectFilter with: Int32Zero with: NULL!
*/
}
public FeEdition transcluders(Filter directFilter, Filter indirectFilter, int flags) {
	return transcluders(directFilter, indirectFilter, flags, null);
/*
udanax-top.st:20426:FeRangeElement methodsFor: 'smalltalk:'!
{FeEdition CLIENT} transcluders: directFilter {Filter default: NULL}
	with: indirectFilter {Filter default: NULL}
	with: flags {Int32 default: Int32Zero}
	
	^self transcluders: directFilter with: indirectFilter with: flags with: NULL!
*/
}
/**
 * Essential. Return the label attached to this FeRangeElement. (An FeRangeElement holds a
 * BeRangeElement and a label.)  All FeRangeElements have a label attached to them when they
 * are created (in the various Server::newRangeElement operations).  Derived Editions have
 * the same the label as the Edition they were derived from (e.g. the receiver of copy,
 * combine, replace, transformedBy, etc.)  Labels may be available only on Editions in 1.0.
 * (While this is in force, label() will blast if sent to other kinds of FeEditions.)
 */
public FeLabel label() {
	throw new UnimplementedException();
/*
udanax-top.st:20434:FeRangeElement methodsFor: 'labelling'!
{FeLabel CLIENT} label
	"Essential. Return the label attached to this FeRangeElement. (An FeRangeElement holds a BeRangeElement and a label.)  All FeRangeElements have a label attached to them when they are created (in the various Server::newRangeElement operations).  Derived Editions have the same the label as the Edition they were derived from (e.g. the receiver of copy, combine, replace, transformedBy, etc.)  Labels may be available only on Editions in 1.0.  (While this is in force, label() will blast if sent to other kinds of FeEditions.)"
	
	self unimplemented. "default"
	^NULL!
*/
}
/**
 * Essential. Return a new FeRangeElement with the same identity and contents (i.e. holding
 * the same BeRangeElement), but with a different label.  (Get new labels from
 * FeServer::newLabel())
 */
public FeRangeElement relabelled(FeLabel label) {
	throw new UnimplementedException();
/*
udanax-top.st:20440:FeRangeElement methodsFor: 'labelling'!
{FeRangeElement CLIENT} relabelled: label {FeLabel}
	"Essential. Return a new FeRangeElement with the same identity and contents (i.e. holding the same BeRangeElement), but with a different label.  (Get new labels from FeServer::newLabel())"
	
	self unimplemented. "default"
	^NULL!
*/
}
/**
 * @deprecated
 */
public boolean becomeOther(FeRangeElement newIdentity) {
	throw new PasseException();
/*
udanax-top.st:20448:FeRangeElement methodsFor: 'smalltalk: passe'!
{BooleanVar} becomeOther: newIdentity {FeRangeElement}
	self passe.	"renamed makeIdentical:"!
*/
}
/**
 * @deprecated
 */
public boolean isSameAs(FeRangeElement other) {
	throw new PasseException();
/*
udanax-top.st:20452:FeRangeElement methodsFor: 'smalltalk: passe'!
{BooleanVar} isSameAs: other {FeRangeElement}
	self passe "isIdentical"!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:20458:FeRangeElement methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:20460:FeRangeElement methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
/**
 * Check whether the endorsements are valid and authorized.
 * Blast appropriately if not.
 */
public static void validateEndorsement(CrossRegion endorsements, FeKeyMaster km) {
	if ( ! (endorsements.isFinite())) {
		throw new AboraRuntimeException(AboraRuntimeException.ENDORSEMENT_MUST_BE_FINITE);
	}
	validateSignature(((IDRegion) (endorsements.projection(0))), km);
/*
udanax-top.st:20471:FeRangeElement class methodsFor: 'protected:'!
{void} validateEndorsement: endorsements {CrossRegion} with: km {FeKeyMaster} 
	"Check whether the endorsements are valid and authorized.
	 Blast appropriately if not."
	endorsements isFinite ifFalse: [Heaper BLAST: #EndorsementMustBeFinite].
	self validateSignature: ((endorsements projection: Int32Zero) cast: IDRegion)
		with: km!
*/
}
/**
 * Check whether the signatures are valid and authorized.
 * Blast appropriately if not.
 */
public static void validateSignature(IDRegion clubs, FeKeyMaster km) {
	if ( ! (clubs.isFinite())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_SIGNATURE_AUTHORITY);
	}
	Stepper stomper = clubs.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ID clubID = (ID) stomper.fetch();
		if (clubID == null) {
			continue ;
		}
		if ( ! (km.hasSignatureAuthority(clubID))) {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_SIGNATURE_AUTHORITY);
		}
	}
	stomper.destroy();
/*
udanax-top.st:20479:FeRangeElement class methodsFor: 'protected:'!
{void} validateSignature: clubs {IDRegion} with: km {FeKeyMaster} 
	"Check whether the signatures are valid and authorized.
	 Blast appropriately if not."
	clubs isFinite ifFalse: [Heaper BLAST: #MustHaveSignatureAuthority].
	clubs stepper forEach: [ :clubID {ID} |
		(km hasSignatureAuthority: clubID)
			ifFalse: [Heaper BLAST: #MustHaveSignatureAuthority]]!
*/
}
/**
 * {void CLIENT} addFillDetector: detector {PrFillDetector}
 * {FeRangeElement CLIENT} again
 * {BooleanVar CLIENT} canMakeIdentical: newIdentity {FeRangeElement}
 * {BooleanVar CLIENT} isIdentical: other {FeRangeElement}
 * {FeLabel CLIENT} label
 * {void CLIENT} makeIdentical: newIdentity {FeRangeElement}
 * {ID CLIENT} owner
 * {FeRangeElement CLIENT} relabelled: label {FeLabel}
 * {void CLIENT} removeFillDetector: detector {PrFillDetector}
 * {void CLIENT} setOwner: clubID {ID}
 * {FeEdition CLIENT} transcluders
 * {FeEdition CLIENT} transcluders: directFilter {Filter default: NULL}
 * {FeEdition CLIENT} transcluders: directFilter {Filter default: NULL} with: indirectFilter
 * {Filter default: NULL}
 * {FeEdition CLIENT} transcluders: directFilter {Filter default: NULL} with: indirectFilter
 * {Filter default: NULL} with: flags {Int32 default: Int32Zero}
 * {FeEdition CLIENT} transcluders: directFilter {Filter default: NULL} with: indirectFilter
 * {Filter default: NULL} with: flags {Int32 default: Int32Zero} with: otherTrail {FeEdition
 * default: NULL}
 * {FeEdition CLIENT} works: filter {Filter default: NULL} with: flags {Int32 default:
 * Int32Zero} with: otherTrail {FeEdition default: NULL}
 */
public static void infostProtocol() {
/*
udanax-top.st:20490:FeRangeElement class methodsFor: 'smalltalk: system'!
info.stProtocol
"{void CLIENT} addFillDetector: detector {PrFillDetector}
{FeRangeElement CLIENT} again
{BooleanVar CLIENT} canMakeIdentical: newIdentity {FeRangeElement}
{BooleanVar CLIENT} isIdentical: other {FeRangeElement}
{FeLabel CLIENT} label
{void CLIENT} makeIdentical: newIdentity {FeRangeElement}
{ID CLIENT} owner
{FeRangeElement CLIENT} relabelled: label {FeLabel}
{void CLIENT} removeFillDetector: detector {PrFillDetector}
{void CLIENT} setOwner: clubID {ID}
{FeEdition CLIENT} transcluders
{FeEdition CLIENT} transcluders: directFilter {Filter default: NULL}
{FeEdition CLIENT} transcluders: directFilter {Filter default: NULL} with: indirectFilter {Filter default: NULL}
{FeEdition CLIENT} transcluders: directFilter {Filter default: NULL} with: indirectFilter {Filter default: NULL} with: flags {Int32 default: Int32Zero}
{FeEdition CLIENT} transcluders: directFilter {Filter default: NULL} with: indirectFilter {Filter default: NULL} with: flags {Int32 default: Int32Zero} with: otherTrail {FeEdition default: NULL}
{FeEdition CLIENT} works: filter {Filter default: NULL} with: flags {Int32 default: Int32Zero} with: otherTrail {FeEdition default: NULL}
"!
*/
}
/**
 * Make a single PlaceHolder.
 */
public static FeRangeElement placeHolder() {
	return FePlaceHolder.on(((BeGrandMap) CurrentGrandMap.fluidGet()).newPlaceHolder());
/*
udanax-top.st:20511:FeRangeElement class methodsFor: 'creation'!
{FeRangeElement CLIENT} placeHolder
	"Make a single PlaceHolder."
	
	^FePlaceHolder on: CurrentGrandMap fluidGet newPlaceHolder!
*/
}
public FeRangeElement() {
/*

Generated during transformation
*/
}
public FeRangeElement(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
