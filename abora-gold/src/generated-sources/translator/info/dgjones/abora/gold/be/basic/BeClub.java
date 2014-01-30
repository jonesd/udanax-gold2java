/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.be.basic.BeClub;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.brange2.UpdateTransitiveMemberIDs;
import info.dgjones.abora.gold.brange2.UpdateTransitiveSuperClubIDs;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeClubDescription;
import info.dgjones.abora.gold.nkernel.FeClub;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.rcmain.ServerChunk;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class BeClub extends BeWork {

	protected ID mySignatureClub;
	protected MuSet myMembers;
	protected MuSet myImmediateSuperClubs;
	protected MuSet mySponsored;
	protected boolean myWallFlag;
	protected IDRegion myTransitiveSuperClubIDs;
	protected IDRegion myTransitiveMemberIDs;
	protected MuSet myKeyMasters;
/*
udanax-top.st:3969:
BeWork subclass: #BeClub
	instanceVariableNames: '
		mySignatureClub {ID | NULL}
		myMembers {MuSet of: BeClub}
		myImmediateSuperClubs {MuSet of: BeClub}
		mySponsored {MuSet of: BeWork}
		myWallFlag {BooleanVar}
		myTransitiveSuperClubIDs {IDRegion}
		myTransitiveMemberIDs {IDRegion}
		myKeyMasters {MuSet NOCOPY | NULL of: NuKeyMaster}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:3981:
(BeClub getOrMakeCxxClassDescription)
	friends:
'/- friends for class BeClub -/
friend class UpdateTransitiveMemberIDs;
friend class UpdateTransitiveSuperClubIDs;
friend class UpdateClubKeyMasterAuthorities;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:4210:
BeClub class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:4213:
(BeClub getOrMakeCxxClassDescription)
	friends:
'/- friends for class BeClub -/
friend class UpdateTransitiveMemberIDs;
friend class UpdateTransitiveSuperClubIDs;
friend class UpdateClubKeyMasterAuthorities;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeClub.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Notify the KeyMaster when the transitive super Clubs of this Club change
 */
public void registerKeyMaster(FeKeyMaster km) {
	if (myKeyMasters == null) {
		myKeyMasters = MuSet.make();
		((MuSet) ActiveClubs.fluidGet()).introduce(this);
	}
	myKeyMasters.introduce(km);
/*
udanax-top.st:3992:BeClub methodsFor: 'dependents'!
{void} registerKeyMaster: km {FeKeyMaster}
	"Notify the KeyMaster when the transitive super Clubs of this Club change"
	
	myKeyMasters == NULL ifTrue:
		[myKeyMasters := MuSet make.
		ActiveClubs fluidGet introduce: self].
	myKeyMasters introduce: km!
*/
}
/**
 * Unregister a previously registered KeyMaster
 */
public void unregisterKeyMaster(FeKeyMaster km) {
	if (myKeyMasters == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NEVER_REGISTERED_KEY_MASTER);
	}
	myKeyMasters.remove(km);
	if (myKeyMasters.isEmpty()) {
		myKeyMasters = null;
		((MuSet) ActiveClubs.fluidGet()).remove(this);
	}
/*
udanax-top.st:4000:BeClub methodsFor: 'dependents'!
{void} unregisterKeyMaster: km {FeKeyMaster}
	"Unregister a previously registered KeyMaster"
	
	myKeyMasters == NULL ifTrue:
		[Heaper BLAST: #NeverRegisteredKeyMaster].
	myKeyMasters remove: km.
	myKeyMasters isEmpty ifTrue:
		[myKeyMasters := NULL.
		ActiveClubs fluidGet remove: self]!
*/
}
/**
 * Add a sponsored Work (sent from the Work)
 */
public void addSponsored(BeWork work) {
	AboraBlockSupport.enterInsistent(1);
	try {
		mySponsored.store(work);
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitInsistent();
	}
/*
udanax-top.st:4012:BeClub methodsFor: 'accessing'!
{void} addSponsored: work {BeWork}
	"Add a sponsored Work (sent from the Work)"
	
	DiskManager insistent: 1 with:
		[mySponsored store: work.
		self diskUpdate]!
*/
}
/**
 * The Club who can endorse and sponsor with this Club
 */
public ID fetchSignatureClub() {
	return mySignatureClub;
/*
udanax-top.st:4019:BeClub methodsFor: 'accessing'!
{ID | NULL} fetchSignatureClub
	"The Club who can endorse and sponsor with this Club"
	
	^mySignatureClub!
*/
}
public boolean isPurgeable() {
	return super.isPurgeable() && (myKeyMasters == null);
/*
udanax-top.st:4024:BeClub methodsFor: 'accessing'!
{BooleanVar} isPurgeable
	^super isPurgeable and: [myKeyMasters == NULL]!
*/
}
public FeRangeElement makeFe(BeLabel label) {
	return FeClub.on(this);
/*
udanax-top.st:4027:BeClub methodsFor: 'accessing'!
{FeRangeElement} makeFe: label {BeLabel | NULL}
	^FeClub on: self!
*/
}
/**
 * Whether the direct membership includes the given Club
 */
public boolean membershipIncludes(BeClub club) {
	return myMembers.hasMember(club);
/*
udanax-top.st:4031:BeClub methodsFor: 'accessing'!
{BooleanVar} membershipIncludes: club {BeClub}
	"Whether the direct membership includes the given Club"
	
	^myMembers hasMember: club!
*/
}
/**
 * Add a sponsored Work (sent from the Work)
 */
public void removeSponsored(BeWork work) {
	AboraBlockSupport.enterInsistent(1);
	try {
		mySponsored.wipe(work);
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitInsistent();
	}
/*
udanax-top.st:4036:BeClub methodsFor: 'accessing'!
{void} removeSponsored: work {BeWork}
	"Add a sponsored Work (sent from the Work)"
	
	DiskManager insistent: 1 with:
		[mySponsored wipe: work.
		self diskUpdate]!
*/
}
/**
 * Change the Club who can endorse and sponsor with this Club
 */
public void setSignatureClub(ID clubID) {
	mySignatureClub = clubID;
/*
udanax-top.st:4043:BeClub methodsFor: 'accessing'!
{void} setSignatureClub: clubID {ID | NULL}
	"Change the Club who can endorse and sponsor with this Club"
	
	mySignatureClub := clubID!
*/
}
public ImmuSet sponsored() {
	return mySponsored.asImmuSet();
/*
udanax-top.st:4048:BeClub methodsFor: 'accessing'!
{ImmuSet of: BeWork} sponsored
	^mySponsored asImmuSet!
*/
}
public IDRegion transitiveMemberIDs() {
	return myTransitiveMemberIDs;
/*
udanax-top.st:4052:BeClub methodsFor: 'accessing'!
{IDRegion} transitiveMemberIDs
	^myTransitiveMemberIDs!
*/
}
public IDRegion transitiveSuperClubIDs() {
	return myTransitiveSuperClubIDs;
/*
udanax-top.st:4055:BeClub methodsFor: 'accessing'!
{IDRegion} transitiveSuperClubIDs
	^myTransitiveSuperClubIDs!
*/
}
public void updateKeyMasters() {
	if (myKeyMasters != null) {
		Stepper stomper = 
		/* notify any KeyMasters who care that my transitive super clubs have changed */
		myKeyMasters.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			FeKeyMaster km = (FeKeyMaster) stomper.fetch();
			if (km == null) {
				continue ;
			}
			km.updateAuthority();
		}
		stomper.destroy();
	}
/*
udanax-top.st:4060:BeClub methodsFor: 'private: propagating'!
{void} updateKeyMasters
	
	myKeyMasters ~~ NULL ifTrue:
		["notify any KeyMasters who care that my transitive super clubs have changed"
		myKeyMasters stepper forEach: [ :km {FeKeyMaster} |
			km updateAuthority]]!
*/
}
public MuSet immediateSuperClubs() {
	return myImmediateSuperClubs;
/*
udanax-top.st:4069:BeClub methodsFor: 'private: accessing'!
{MuSet of: BeClub} immediateSuperClubs
	^ myImmediateSuperClubs!
*/
}
public MuSet members() {
	return myMembers;
/*
udanax-top.st:4072:BeClub methodsFor: 'private: accessing'!
{MuSet of: BeClub} members
	^ myMembers!
*/
}
/**
 * Update cached information
 */
public void revise(FeEdition contents) {
	MuSet oldMembers;
	FeEdition oldMembership;
	FeEdition newMembership;
	boolean memberTest;
	if ( ! (FeClubDescription.check(contents))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_CLUB_DESCRIPTION);
	}
	AboraBlockSupport.enterConsistent();
	try {
		oldMembership = (FeEdition) (edition().fetch((Sequence.string("ClubDescription:Membership"))));
		super.revise(contents);
		/* Do this first so that permissions will change after the revision */
		newMembership = (FeEdition) (contents.fetch((Sequence.string("ClubDescription:Membership"))));
		/* Update cached info if membership changes */
		if (oldMembership == null || (oldMembership.isEmpty())) {
			memberTest = newMembership == null || (newMembership.isEmpty());
		}
		else {
			memberTest = newMembership != null && (newMembership.isIdentical(oldMembership));
		}
		if ( ! (memberTest)) {
			oldMembers = myMembers;
			myMembers = MuSet.make();
			Stepper stomper = newMembership.stepper();
			for (; stomper.hasValue(); stomper.step()) {
				FeWork mem = (FeWork) stomper.fetch();
				if (mem == null) {
					continue ;
				}
				myMembers.introduce(((BeClub) mem.getOrMakeBe()));
			}
			stomper.destroy();
			Stepper stomper2 = 
			/* Update all new members */
			(myMembers.asImmuSet().minus(oldMembers)).stepper();
			for (; stomper2.hasValue(); stomper2.step()) {
				BeClub newMem = (BeClub) stomper2.fetch();
				if (newMem == null) {
					continue ;
				}
				newMem.addImmediateSuperClub(this);
			}
			stomper2.destroy();
			Stepper stomper3 = 
			/* Update all lost members */
			(oldMembers.asImmuSet().minus(myMembers)).stepper();
			for (; stomper3.hasValue(); stomper3.step()) {
				BeClub lostMem = (BeClub) stomper3.fetch();
				if (lostMem == null) {
					continue ;
				}
				lostMem.removeImmediateSuperClub(this);
			}
			stomper3.destroy();
			/* Update self and all parents with new membership list */
			updateTransitiveMemberIDs();
			diskUpdate();
		}
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:4077:BeClub methodsFor: 'contents'!
{void} revise: contents {FeEdition}
	"Update cached information"
	
	| oldMembers {MuSet of: BeClub} oldMembership {FeEdition} newMembership {FeEdition} memberTest {BooleanVar} |
	(FeClubDescription check: contents) ifFalse:
		[Heaper BLAST: #MustBeClubDescription].
	DiskManager consistent:
		[oldMembership := (self edition
			fetch: (Sequence string: 'ClubDescription:Membership')) cast: FeEdition.
		super revise: contents. "Do this first so that permissions will change after the revision"
		newMembership := (contents
			fetch: (Sequence string: 'ClubDescription:Membership')) cast: FeEdition.
		"Update cached info if membership changes"
		(oldMembership == NULL or: [oldMembership isEmpty])
				ifTrue: [memberTest _ newMembership == NULL or: [newMembership isEmpty]]
				ifFalse: [memberTest _ newMembership ~~ NULL and: [newMembership isIdentical: oldMembership]].
		memberTest ifFalse:
			[oldMembers := myMembers.
			myMembers := MuSet make.
			newMembership stepper forEach: [ :mem {FeWork} |
				myMembers introduce: (mem getOrMakeBe cast: BeClub)].
			"Update all new members"
			(myMembers asImmuSet minus: oldMembers) stepper forEach: [ :newMem {BeClub} |
				newMem addImmediateSuperClub: self].
			"Update all lost members"
			(oldMembers asImmuSet minus: myMembers) stepper forEach: [ :lostMem {BeClub} |
				lostMem removeImmediateSuperClub: self].
			"Update self and all parents with new membership list"
			self updateTransitiveMemberIDs.
			self diskUpdate]]!
*/
}
/**
 * Add an immediate super Club and update my cached information, and those of my members
 */
public void addImmediateSuperClub(BeClub parent) {
	myImmediateSuperClubs.store(parent);
	updateTransitiveSuperClubIDs();
/*
udanax-top.st:4110:BeClub methodsFor: 'propagating'!
{void} addImmediateSuperClub: parent {BeClub}
	"Add an immediate super Club and update my cached information, and those of my members"
	
	myImmediateSuperClubs store: parent.
	self updateTransitiveSuperClubIDs.!
*/
}
/**
 * Add an immediate super Club and update my cached information, and those of my members
 */
public void removeImmediateSuperClub(BeClub parent) {
	myImmediateSuperClubs.remove(parent);
	updateTransitiveSuperClubIDs();
/*
udanax-top.st:4116:BeClub methodsFor: 'propagating'!
{void} removeImmediateSuperClub: parent {BeClub}
	"Add an immediate super Club and update my cached information, and those of my members"
	
	myImmediateSuperClubs remove: parent.
	self updateTransitiveSuperClubIDs.!
*/
}
/**
 * Figure out result of changes in membership, then propagate upwards
 */
public void updateTransitiveMemberIDs() {
	XnRegion result;
	result = IDSpace.global().emptyRegion();
	Stepper stomper = myMembers.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		BeClub mem = (BeClub) stomper.fetch();
		if (mem == null) {
			continue ;
		}
		result = (result.unionWith(mem.transitiveMemberIDs()));
	}
	stomper.destroy();
	result = (result.with((((BeGrandMap) CurrentGrandMap.fluidGet()).iDOf(this))));
	if ( ! (result.isEqual(myTransitiveMemberIDs))) {
		AboraBlockSupport.enterInsistent(4);
		try {
			myTransitiveMemberIDs = (IDRegion) result;
			diskUpdate();
			if ( ! (myImmediateSuperClubs.isEmpty())) {
				(UpdateTransitiveMemberIDs.make(myImmediateSuperClubs.copy().asMuSet())).schedule();
			}
		}
		finally {
			AboraBlockSupport.exitInsistent();
		}
	}
/*
udanax-top.st:4122:BeClub methodsFor: 'propagating'!
{void} updateTransitiveMemberIDs
	"Figure out result of changes in membership, then propagate upwards"
	
	| result {XnRegion} |
	result := IDSpace global emptyRegion.
	myMembers stepper forEach: [ :mem {BeClub} |
		result := (result unionWith: mem transitiveMemberIDs)].
	result := (result with: (CurrentGrandMap fluidGet iDOf: self)).
	(result isEqual: myTransitiveMemberIDs) ifFalse:
		[DiskManager insistent: 4 with: 
			[myTransitiveMemberIDs := result cast: IDRegion.
			self diskUpdate.
			myImmediateSuperClubs isEmpty ifFalse:
				[(UpdateTransitiveMemberIDs make: myImmediateSuperClubs copy asMuSet) schedule]]]!
*/
}
/**
 * Figure out result of changes in membership, then propagate upwards
 */
public void updateTransitiveSuperClubIDs() {
	XnRegion result;
	result = IDSpace.global().emptyRegion();
	Stepper stomper = myImmediateSuperClubs.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		BeClub sup = (BeClub) stomper.fetch();
		if (sup == null) {
			continue ;
		}
		result = (result.unionWith(sup.transitiveSuperClubIDs()));
	}
	stomper.destroy();
	result = (result.with((((BeGrandMap) CurrentGrandMap.fluidGet()).iDOf(this))));
	if ( ! (result.isEqual(myTransitiveSuperClubIDs))) {
		AboraBlockSupport.enterInsistent(4);
		try {
			myTransitiveSuperClubIDs = (IDRegion) result;
			diskUpdate();
			if ( ! (myMembers.isEmpty())) {
				(UpdateTransitiveSuperClubIDs.make(myMembers.copy().asMuSet(), ((BeGrandMap) CurrentGrandMap.fluidGet()))).schedule();
			}
		}
		finally {
			AboraBlockSupport.exitInsistent();
		}
		/* notify any KeyMasters who care that my transitive super clubs have changed */
		if (myKeyMasters != null) {
			Stepper stomper2 = myKeyMasters.stepper();
			for (; stomper2.hasValue(); stomper2.step()) {
				FeKeyMaster km = (FeKeyMaster) stomper2.fetch();
				if (km == null) {
					continue ;
				}
				km.updateAuthority();
			}
			stomper2.destroy();
		}
	}
/*
udanax-top.st:4137:BeClub methodsFor: 'propagating'!
{void} updateTransitiveSuperClubIDs
	"Figure out result of changes in membership, then propagate upwards"
	
	| result {XnRegion} |
	result := IDSpace global emptyRegion.
	myImmediateSuperClubs stepper forEach: [ :sup {BeClub} |
		result := (result unionWith: sup transitiveSuperClubIDs)].
	result := (result with: (CurrentGrandMap fluidGet iDOf: self)).
	(result isEqual: myTransitiveSuperClubIDs) ifFalse:
		[DiskManager insistent: 4 with:
			[myTransitiveSuperClubIDs := result cast: IDRegion.
			self diskUpdate.
			myMembers isEmpty ifFalse:
				[(UpdateTransitiveSuperClubIDs make: myMembers copy asMuSet with: CurrentGrandMap fluidGet) schedule]].
		"notify any KeyMasters who care that my transitive super clubs have changed"
		myKeyMasters ~~ NULL ifTrue:
			[myKeyMasters stepper forEach: [ :km {FeKeyMaster} |
				km updateAuthority]]]!
*/
}
public void restartClub(Rcvr rcvr) {
	myKeyMasters = null;
/*
udanax-top.st:4158:BeClub methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartClub: rcvr {Rcvr}
	myKeyMasters _ NULL!
*/
}
public BeClub(FeEdition contents) {
	super(contents, true);
	FeEdition membership;
	mySignatureClub = ((ID) InitialOwner.fluidGet());
	myMembers = MuSet.make();
	membership = (FeEdition) (contents.fetch((Sequence.string("ClubDescription:Membership"))));
	if (membership != null) {
		Stepper stomper = membership.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			FeClub club = (FeClub) stomper.fetch();
			if (club == null) {
				continue ;
			}
			myMembers.introduce(club.beClub());
		}
		stomper.destroy();
	}
	myImmediateSuperClubs = MuSet.make();
	mySponsored = MuSet.make();
	Someone.knownBug();
	/* wall flag */
	myWallFlag = false;
	myTransitiveSuperClubIDs = (IDRegion) IDSpace.global().emptyRegion();
	myTransitiveMemberIDs = (IDRegion) IDSpace.global().emptyRegion();
	Stepper stomper2 = myMembers.stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		BeClub mem = (BeClub) stomper2.fetch();
		if (mem == null) {
			continue ;
		}
		myTransitiveMemberIDs = (IDRegion) (myTransitiveMemberIDs.unionWith(mem.transitiveMemberIDs()));
	}
	stomper2.destroy();
	restartClub(null);
	finishCreation();
/*
udanax-top.st:4163:BeClub methodsFor: 'creation'!
create: contents {FeEdition}
	
	| membership {FeEdition} |
	super create: contents with: true.
	mySignatureClub := InitialOwner fluidGet.
	myMembers := MuSet make.
	membership := (contents fetch: (Sequence
		string: 'ClubDescription:Membership')) cast: FeEdition.
	membership ~~ NULL ifTrue:
		[membership stepper forEach: [ :club {FeClub} |
			myMembers introduce: club beClub]].
	myImmediateSuperClubs := MuSet make.
	mySponsored := MuSet make.
	self knownBug. "wall flag"
	myWallFlag := false.
	myTransitiveSuperClubIDs := IDSpace global emptyRegion cast: IDRegion.
	myTransitiveMemberIDs := IDSpace global emptyRegion cast: IDRegion.
	myMembers stepper forEach: [ :mem {BeClub} |
		myTransitiveMemberIDs := (myTransitiveMemberIDs
			unionWith: mem transitiveMemberIDs) cast: IDRegion].
	self restartClub: NULL.
	self finishCreation.!
*/
}
public BeClub(Rcvr receiver) {
	super(receiver);
	mySignatureClub = (ID) receiver.receiveHeaper();
	myMembers = (MuSet) receiver.receiveHeaper();
	myImmediateSuperClubs = (MuSet) receiver.receiveHeaper();
	mySponsored = (MuSet) receiver.receiveHeaper();
	myWallFlag = receiver.receiveBooleanVar();
	myTransitiveSuperClubIDs = (IDRegion) receiver.receiveHeaper();
	myTransitiveMemberIDs = (IDRegion) receiver.receiveHeaper();
	restartClub(receiver);
/*
udanax-top.st:4188:BeClub methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	mySignatureClub _ receiver receiveHeaper.
	myMembers _ receiver receiveHeaper.
	myImmediateSuperClubs _ receiver receiveHeaper.
	mySponsored _ receiver receiveHeaper.
	myWallFlag _ receiver receiveBooleanVar.
	myTransitiveSuperClubIDs _ receiver receiveHeaper.
	myTransitiveMemberIDs _ receiver receiveHeaper.
	self restartClub: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(mySignatureClub);
	xmtr.sendHeaper(myMembers);
	xmtr.sendHeaper(myImmediateSuperClubs);
	xmtr.sendHeaper(mySponsored);
	xmtr.sendBooleanVar(myWallFlag);
	xmtr.sendHeaper(myTransitiveSuperClubIDs);
	xmtr.sendHeaper(myTransitiveMemberIDs);
/*
udanax-top.st:4199:BeClub methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: mySignatureClub.
	xmtr sendHeaper: myMembers.
	xmtr sendHeaper: myImmediateSuperClubs.
	xmtr sendHeaper: mySponsored.
	xmtr sendBooleanVar: myWallFlag.
	xmtr sendHeaper: myTransitiveSuperClubIDs.
	xmtr sendHeaper: myTransitiveMemberIDs.!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(BeClub.class, "CurrentOwner", ServerChunk.emulsion(), null);
	AboraSupport.defineFluid(MuSet.class, "ActiveClubs", DiskManager.emulsion(), MuSet.make());
/*
udanax-top.st:4224:BeClub class methodsFor: 'smalltalk: init'!
staticTimeNonInherited
	BeClub defineFluid: #CurrentOwner with: ServerChunk emulsion with: [NULL].
	MuSet defineFluid: #ActiveClubs with: DiskManager emulsion with: [MuSet make]!
*/
}
public static BeWork make(FeEdition contents) {
	AboraBlockSupport.enterConsistent();
	try {
		return new BeClub(contents);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:4230:BeClub class methodsFor: 'creation'!
make: contents {FeEdition}
	DiskManager consistent:
		[^BeClub create: contents]!
*/
}
public BeClub() {
/*

Generated during transformation
*/
}
}
