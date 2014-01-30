/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BeClub;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeClub;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * A persistent Club on the Server.
 */
public class FeClub extends FeWork {

/*
udanax-top.st:22647:
FeWork subclass: #FeClub
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:22651:
FeClub comment:
'A persistent Club on the Server.'!
*/
/*
udanax-top.st:22653:
(FeClub getOrMakeCxxClassDescription)
	friends:
'/- friends for class FeClub -/
friend class BeClub;
';
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:22730:
FeClub class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:22733:
(FeClub getOrMakeCxxClassDescription)
	friends:
'/- friends for class FeClub -/
friend class BeClub;
';
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeClub.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential.  Irrevocably remove signature authority for this Club. Requires ownership
 * authority.
 */
public void removeSignatureClub() {
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	beClub().setSignatureClub(null);
/*
udanax-top.st:22663:FeClub methodsFor: 'signing'!
{void CLIENT} removeSignatureClub
	"Essential.  Irrevocably remove signature authority for this Club. Requires ownership authority."
	
	(CurrentKeyMaster fluidGet hasAuthority: self owner) 
		ifFalse: [Heaper BLAST: #MustBeOwner].
	self beClub setSignatureClub: NULL!
*/
}
/**
 * Essential.  Change who has signature authority for this Club. Requires ownership
 * authority.
 * Aborts if the Work doesn't have a signature Club.
 */
public void setSignatureClub(ID club) {
	Ravi.knownBug();
	/* need to updateStatus on Works which are designating me as Author */
	if (club == null) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_NOT_BE_NULL);
	}
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	if (beClub().fetchSignatureClub() == null) {
		throw new AboraRuntimeException(AboraRuntimeException.SIGNATURE_CLUB_IRREVOCABLY_REMOVED);
	}
	beClub().setSignatureClub(club);
/*
udanax-top.st:22670:FeClub methodsFor: 'signing'!
{void CLIENT} setSignatureClub: club {ID | NULL}
	"Essential.  Change who has signature authority for this Club. Requires ownership authority.
	 Aborts if the Work doesn't have a signature Club."
	
	Ravi knownBug. "need to updateStatus on Works which are designating me as Author"
	club == NULL ifTrue: [Heaper BLAST: #MustNotBeNull].
	(CurrentKeyMaster fluidGet hasAuthority: self owner) 
		ifFalse: [Heaper BLAST: #MustBeOwner].
	self beClub fetchSignatureClub == NULL
		ifTrue: [Heaper BLAST: #SignatureClubIrrevocablyRemoved].
	self beClub setSignatureClub: club!
*/
}
/**
 * Essential. The Club which has 'signature authority' for this Club. Members of this Club
 * are allowed to endorse with the ID of this Club, and are allowed to use it to sponsor
 * resources. BLASTs if it has been removed
 */
public ID signatureClub() {
	ID result;
	result = beClub().fetchSignatureClub();
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.SIGNATURE_CLUB_IRREVOCABLY_REMOVED);
	}
	return result;
/*
udanax-top.st:22682:FeClub methodsFor: 'signing'!
{ID CLIENT} signatureClub
	"Essential. The Club which has 'signature authority' for this Club. Members of this Club are allowed to endorse with the ID of this Club, and are allowed to use it to sponsor resources. BLASTs if it has been removed"
	
	| result {ID} |
	result := self beClub fetchSignatureClub.
	result == NULL ifTrue: [Heaper BLAST: #SignatureClubIrrevocablyRemoved].
	^result!
*/
}
public BeClub beClub() {
	return (BeClub) fetchBe();
/*
udanax-top.st:22692:FeClub methodsFor: 'server'!
{BeClub} beClub
	^self fetchBe cast: BeClub!
*/
}
public FeEdition sponsoredWorks() {
	return sponsoredWorks(null);
/*
udanax-top.st:22698:FeClub methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} sponsoredWorks
	^self sponsoredWorks: NULL!
*/
}
/**
 * Essential.  All of the Works sponsored by this Club. If a Filter is given, then restricts
 * the result to Works which pass the filter. The result can be wrapped with a Set. This does
 * not require any permissions.
 */
public FeEdition sponsoredWorks(Filter filter) {
	IDSpace iDSpace;
	PtrArray array;
	int index;
	array = PtrArray.nulls(beClub().sponsored().count());
	index = 0;
	Stepper stomper = beClub().sponsored().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		BeWork be = (BeWork) stomper.fetch();
		if (be == null) {
			continue ;
		}
		if (filter == null || (filter.match(be.endorsements()))) {
			array.store(index, (FeWork.on(be)));
			index = index + 1;
		}
	}
	stomper.destroy();
	iDSpace = IDSpace.unique();
	if (index < array.count()) {
		array = (PtrArray) (array.copy(index));
	}
	return FeEdition.on((((BeGrandMap) CurrentGrandMap.fluidGet()).newValueEdition(array, (iDSpace.newIDs(array.count())), iDSpace.getAscending())));
/*
udanax-top.st:22704:FeClub methodsFor: 'managing storage'!
{FeEdition CLIENT} sponsoredWorks: filter {Filter default: NULL}
	"Essential.  All of the Works sponsored by this Club. If a Filter is given, then restricts the result to Works which pass the filter. The result can be wrapped with a Set. This does not require any permissions."
	
	| iDSpace {IDSpace} array {PtrArray of: FeWork} index {Int32} |
	ImmuSet USES.
	array := PtrArray nulls: self beClub sponsored count DOTasLong.
	index := Int32Zero.
	self beClub sponsored stepper forEach: [ :be {BeWork} |
		(filter == NULL or: [filter match: be endorsements]) ifTrue:
			[array at: index store: (FeWork on: be).
			index := index + 1]].
	iDSpace := IDSpace unique.
	index < array count ifTrue:
		[array := (array copy: index) cast: PtrArray].
	^FeEdition on: (CurrentGrandMap fluidGet
		newValueEdition: array
		with: (iDSpace newIDs: array count)
		with: iDSpace getAscending)!
*/
}
public FeClub(BeClub be) {
	super(be);
/*
udanax-top.st:22725:FeClub methodsFor: 'private: create'!
create: be {BeClub}
	super create: be.!
*/
}
/**
 * Essential.  Create a new Club whose initial status is described in the given
 * ClubDescription Edition. The reader, editor and owner are taken from the current settings.
 * If the KeyMaster has edit permission, then the Club Work is initially grabbed by it. The
 * Club Work is initially sponsored by the CurrentSponsor.
 * Note: Unlike ordinary Works, a newly created Club is assigned a global ID.
 */
public static FeWork make(FeEdition status) {
	FeKeyMaster.assertSponsorship();
	FeKeyMaster.assertSignatureAuthority();
	return (FeClub) (((BeGrandMap) CurrentGrandMap.fluidGet()).newClub(status)).makeLockedFeWork();
/*
udanax-top.st:22743:FeClub class methodsFor: 'creation'!
{FeClub CLIENT} make: status {FeEdition}
	"Essential.  Create a new Club whose initial status is described in the given ClubDescription Edition. The reader, editor and owner are taken from the current settings. If the KeyMaster has edit permission, then the Club Work is initially grabbed by it. The Club Work is initially sponsored by the CurrentSponsor.
	Note: Unlike ordinary Works, a newly created Club is assigned a global ID."
	
	FeKeyMaster assertSponsorship.
	FeKeyMaster assertSignatureAuthority.
	^(CurrentGrandMap fluidGet newClub: status) makeLockedFeWork cast: FeClub!
*/
}
public static FeClub on(BeClub be) {
	FeClub result;
	result = new FeClub(be);
	be.addFeRangeElement(result);
	return result;
/*
udanax-top.st:22751:FeClub class methodsFor: 'creation'!
{FeClub} on: be {BeClub}
	| result {FeClub} |
	result := self create: be.
	be addFeRangeElement: result.
	^result!
*/
}
/**
 * {void CLIENT} removeSignatureClub
 * {void CLIENT} setSignatureClub: club {ID}
 * {ID CLIENT} signatureClub
 * {FeEdition CLIENT} sponsoredWorks: filter {Filter default: NULL}
 */
public static void infostProtocol() {
/*
udanax-top.st:22760:FeClub class methodsFor: 'smalltalk: system'!
info.stProtocol
"{void CLIENT} removeSignatureClub
{void CLIENT} setSignatureClub: club {ID}
{ID CLIENT} signatureClub
{FeEdition CLIENT} sponsoredWorks: filter {Filter default: NULL}
"!
*/
}
public FeClub() {
/*

Generated during transformation
*/
}
public FeClub(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
