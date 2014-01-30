/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.brange2.BeWorkLockExecutor;
import info.dgjones.abora.gold.brange2.RevisionWatcherExecutor;
import info.dgjones.abora.gold.collection.basic.WeakPtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeLabel;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.primtab.PrimSet;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

/**
 * This is the actual representation on disk; the Fe versions of these classes hide the
 * actual representation.�
 */
public class BeWork extends BeRangeElement {

	protected BeEdition myEdition;
	protected BeLabel myEditionLabel;
	protected ID myReadClub;
	protected ID myEditClub;
	protected BertProp myOwnProp;
	protected BeEdition myHistory;
	protected ID myHistoryClub;
	protected int myRevisionCount;
	protected int myRevisionTime;
	protected ID myReviser;
	protected IDRegion mySponsors;
	protected WeakPtrArray myLockingWork;
	protected PrimSet myRevisionWatchers;
/*
udanax-top.st:3544:
BeRangeElement subclass: #BeWork
	instanceVariableNames: '
		myEdition {BeEdition}
		myEditionLabel {BeLabel}
		myReadClub {ID | NULL}
		myEditClub {ID | NULL}
		myOwnProp {BertProp}
		myHistory {BeEdition | NULL}
		myHistoryClub {ID | NULL}
		myRevisionCount {IntegerVar}
		myRevisionTime {IntegerVar}
		myReviser {ID}
		mySponsors {IDRegion}
		myLockingWork {WeakPtrArray NOCOPY of: FeWork}
		myRevisionWatchers {PrimSet NOCOPY | NULL of: FeWork}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:3561:
BeWork comment:
'This is the actual representation on disk; the Fe versions of these classes hide the actual representation.�'!
*/
/*
udanax-top.st:3563:
(BeWork getOrMakeCxxClassDescription)
	friends:
'/- friends for class BeWork -/
friend class BeWorkLockExecutor;';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:3954:
BeWork class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:3957:
(BeWork getOrMakeCxxClassDescription)
	friends:
'/- friends for class BeWork -/
friend class BeWorkLockExecutor;';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeWork.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Answer whether the KeyMaster has the authority to edit this work.
 */
public boolean canBeEditedBy(FeKeyMaster km) {
	return myEditClub != null && (km.hasAuthority(myEditClub));
/*
udanax-top.st:3571:BeWork methodsFor: 'locking'!
{BooleanVar} canBeEditedBy: km {FeKeyMaster}
	"Answer whether the KeyMaster has the authority to edit this work."
	^myEditClub ~~ NULL and: [km hasAuthority: myEditClub]!
*/
}
/**
 * Return true if the KeyMaster has the authority to read this Work.
 */
public boolean canBeReadBy(FeKeyMaster km) {
	return (myReadClub != null && (km.hasAuthority(myReadClub))) || (canBeEditedBy(km));
/*
udanax-top.st:3576:BeWork methodsFor: 'locking'!
{BooleanVar} canBeReadBy: km {FeKeyMaster}
	"Return true if the KeyMaster has the authority to read this Work."
	
	^(myReadClub ~~ NULL and: [km hasAuthority: myReadClub])
		or: [self canBeEditedBy: km]!
*/
}
/**
 * The Work which has this locked, or NULL if noone does.
 */
public FeWork fetchLockingWork() {
	return (FeWork) (myLockingWork.fetch(0));
/*
udanax-top.st:3582:BeWork methodsFor: 'locking'!
{FeWork INLINE | NULL} fetchLockingWork
	"The Work which has this locked, or NULL if noone does."
	
	^(myLockingWork fetch: Int32Zero) cast: FeWork!
*/
}
/**
 * Make a frontend Work on me and lock it if possible.
 */
public FeWork makeLockedFeWork() {
	FeWork result;
	FeKeyMaster ckm;
	result = (FeWork) (makeFe(null));
	ckm = ((FeKeyMaster) CurrentKeyMaster.fluidGet());
	if (fetchLockingWork() == null && (canBeEditedBy(ckm))) {
		result.grab();
	}
	return result;
/*
udanax-top.st:3587:BeWork methodsFor: 'locking'!
{FeWork} makeLockedFeWork
	"Make a frontend Work on me and lock it if possible."
	
	| result {FeWork} ckm {FeKeyMaster} |
	result := (self makeFe: NULL) cast: FeWork.
	ckm := CurrentKeyMaster fluidGet.
	(self fetchLockingWork == NULL and: [self canBeEditedBy: ckm])
		ifTrue: [result grab].
	^result!
*/
}
/**
 * Try to lock with the give FE Work. Return TRUE if successful
 */
public boolean tryLock(FeWork work) {
	FeWork curLock;
	curLock = fetchLockingWork();
	if (curLock == null || (curLock.isEqual(work))) {
		myLockingWork.store(0, work);
		return true;
	}
	else {
		return false;
	}
/*
udanax-top.st:3597:BeWork methodsFor: 'locking'!
{BooleanVar} tryLock: work {FeWork}
	"Try to lock with the give FE Work. Return TRUE if successful"
	| curLock {FeWork} |
	curLock := self fetchLockingWork.
	(curLock == NULL or: [curLock isEqual: work]) ifTrue:
		[myLockingWork at: Int32Zero store: work.
		^true]
	ifFalse:
		[^false]!
*/
}
/**
 * If the given FE Work is locking, then unlock and return TRUE; else return FALSE with no
 * change in lock state
 */
public boolean tryUnlock(FeWork work) {
	if (fetchLockingWork() == work) {
		/* Unlock and tell everyone about the change */
		myLockingWork.store(0, null);
		updateFeStatus();
		return true;
	}
	else {
		return false;
	}
/*
udanax-top.st:3607:BeWork methodsFor: 'locking'!
{BooleanVar} tryUnlock: work {FeWork}
	"If the given FE Work is locking, then unlock and return TRUE; else return FALSE with no change in lock state"
	
	self fetchLockingWork == work ifTrue:
		["Unlock and tell everyone about the change"
		myLockingWork at: Int32Zero store: NULL.
		self updateFeStatus.
		^true]
	ifFalse:
		[^false]!
*/
}
/**
 * Tell the FE Work whenever this Work is revised
 */
public void addRevisionWatcher(FeWork work) {
	if (myRevisionWatchers == null) {
		myRevisionWatchers = PrimSet.weak(7, (RevisionWatcherExecutor.make(this)));
	}
	myRevisionWatchers.introduce(work);
/*
udanax-top.st:3620:BeWork methodsFor: 'contents'!
{void} addRevisionWatcher: work {FeWork}
	"Tell the FE Work whenever this Work is revised"
	
	myRevisionWatchers == NULL ifTrue:
		[myRevisionWatchers := PrimSet weak: 7 with: (RevisionWatcherExecutor make: self)].
	myRevisionWatchers introduce: work!
*/
}
/**
 * The current Edition.
 * Note: If this is an unsponsored Work, the Edition might have been discarded, and this
 * operation will blast.
 */
public FeEdition edition() {
	Someone.thingToDo();
	/* Cache this */
	return FeEdition.on(myEdition, (FeLabel.on(myEditionLabel)));
/*
udanax-top.st:3627:BeWork methodsFor: 'contents'!
{FeEdition} edition
	"The current Edition.
	Note: If this is an unsponsored Work, the Edition might have been discarded, and this operation will blast."
	
	self thingToDo. "Cache this"
	^FeEdition on: myEdition with: (FeLabel on: myEditionLabel)!
*/
}
/**
 * The Club who made the last revision
 */
public ID lastRevisionAuthor() {
	return myReviser;
/*
udanax-top.st:3634:BeWork methodsFor: 'contents'!
{ID} lastRevisionAuthor
	"The Club who made the last revision"
	
	^myReviser!
*/
}
/**
 * The sequence number of the last revision of this Work.
 */
public int lastRevisionNumber() {
	return myRevisionCount;
/*
udanax-top.st:3639:BeWork methodsFor: 'contents'!
{IntegerVar} lastRevisionNumber
	"The sequence number of the last revision of this Work."
	
	^myRevisionCount!
*/
}
/**
 * The time of the last revision of this Work.
 */
public int lastRevisionTime() {
	return myRevisionTime;
/*
udanax-top.st:3644:BeWork methodsFor: 'contents'!
{IntegerVar} lastRevisionTime
	"The time of the last revision of this Work."
	
	^myRevisionTime!
*/
}
/**
 * Change the current edition and notify anyone who cares about the revision
 */
public void recordHistory() {
	BeGrandMap gm;
	if (myHistoryClub == null) {
		return ;
	}
	gm = ((BeGrandMap) CurrentGrandMap.fluidGet());
	/* Bind all these because they not be set. */
	Object initialReadClubOldValue = AboraBlockSupport.enterFluidBindDuring(InitialReadClub, myHistoryClub);
	try {
		Object initialEditClubOldValue = AboraBlockSupport.enterFluidBindDuring(InitialEditClub, gm.emptyClubID());
		try {
			Object initialOwnerOldValue = AboraBlockSupport.enterFluidBindDuring(InitialOwner, owner());
			try {
				Object initialSponsorOldValue = AboraBlockSupport.enterFluidBindDuring(InitialSponsor, gm.emptyClubID());
				try 
				/* Don't sponsor the history. */
				{
					BeWork legacy;
					legacy = gm.newWork(edition());
					legacy.setEditClub(null);
					Someone.thingToDo();
					/* legacy endorse: (CurrentAuthor fluidGet with: #revised). */
					myHistory = revisions().with(IntegerPos.make(myRevisionCount), (gm.carrier(legacy)));
				}
				finally {
					AboraBlockSupport.exitFluidBindDuring(InitialSponsor, initialSponsorOldValue);
				}
			}
			finally {
				AboraBlockSupport.exitFluidBindDuring(InitialOwner, initialOwnerOldValue);
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(InitialEditClub, initialEditClubOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(InitialReadClub, initialReadClubOldValue);
	}
/*
udanax-top.st:3649:BeWork methodsFor: 'contents'!
{void} recordHistory
	"Change the current edition and notify anyone who cares about the revision"
	
	| gm {BeGrandMap} |
	myHistoryClub == NULL ifTrue: [^VOID].
	gm _ CurrentGrandMap fluidGet.
	"Bind all these because they not be set."
	InitialReadClub fluidBind: myHistoryClub during:
	[InitialEditClub fluidBind: gm emptyClubID during:
	[InitialOwner fluidBind: self owner during:
	[InitialSponsor fluidBind: gm emptyClubID during:  "Don't sponsor the history."
		[| legacy {BeWork} |
		legacy _ gm newWork: self edition.
		legacy setEditClub: NULL.
		self thingToDo.   "legacy endorse: (CurrentAuthor fluidGet with: #revised)."
		myHistory _ self revisions with: myRevisionCount integer with: (gm carrier: legacy)].
	]]]!
*/
}
/**
 * Inform the work that its last revision watcher is gone.
 */
public void removeLastRevisionWatcher() {
	myRevisionWatchers = null;
/*
udanax-top.st:3667:BeWork methodsFor: 'contents'!
{void} removeLastRevisionWatcher
	"Inform the work that its last revision watcher is gone."
	
	myRevisionWatchers := NULL!
*/
}
/**
 * Remove a previously added RevisionWatcher
 */
public void removeRevisionWatcher(FeWork work) {
	if (myRevisionWatchers == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NEVER_ADDED_REVISION_WATCHER);
	}
	myRevisionWatchers.remove(work);
	if (myRevisionWatchers.isEmpty()) {
		myRevisionWatchers = null;
	}
/*
udanax-top.st:3672:BeWork methodsFor: 'contents'!
{void} removeRevisionWatcher: work {FeWork}
	"Remove a previously added RevisionWatcher"
	
	myRevisionWatchers == NULL ifTrue:
		[Heaper BLAST: #NeverAddedRevisionWatcher].
	myRevisionWatchers remove: work.
	myRevisionWatchers isEmpty ifTrue:
		[myRevisionWatchers := NULL].!
*/
}
/**
 * Change the current edition and notify anyone who cares about the revision
 */
public void revise(FeEdition edition) {
	AboraBlockSupport.enterConsistent();
	try {
		Someone.knownBug();
		/* this may not be the right thing to do when not grabbed - it only happens during booting anyway */
		if (fetchLockingWork() == null) {
			myReviser = ((ID) CurrentAuthor.fluidGet());
		}
		else {
			myReviser = fetchLockingWork().getAuthor();
		}
		myEdition.removeWork(this);
		myEdition = edition.beEdition();
		myEditionLabel = (BeLabel) edition.label().getOrMakeBe();
		myEdition.introduceWork(this);
		myRevisionCount = myRevisionCount + 1;
		myRevisionTime = BeGrandMap.xuTime();
		/* Trigger immediate revisionDetectors */
		if (myRevisionWatchers != null) {
			Stepper stomper = myRevisionWatchers.stepper();
			for (; stomper.hasValue(); stomper.step()) {
				FeWork work = (FeWork) stomper.fetch();
				if (work == null) {
					continue ;
				}
				work.triggerRevisionDetectors(edition, myReviser, myRevisionTime, myRevisionCount);
			}
			stomper.destroy();
		}
		/* Record result into the trail */
		if (myHistoryClub != null) {
			recordHistory();
		}
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:3681:BeWork methodsFor: 'contents'!
{void} revise: edition {FeEdition}
	"Change the current edition and notify anyone who cares about the revision"
	
	DiskManager consistent:
		[self knownBug. "this may not be the right thing to do when not grabbed - it only happens during booting anyway"
		self fetchLockingWork == NULL
			ifTrue: [myReviser := CurrentAuthor fluidGet]
			ifFalse: [myReviser _ self fetchLockingWork getAuthor].
		myEdition removeWork: self.
		myEdition := edition beEdition.
		myEditionLabel _ edition label getOrMakeBe cast: BeLabel.
		myEdition introduceWork: self.
		myRevisionCount _ myRevisionCount + 1.
		myRevisionTime := BeGrandMap xuTime.
		"Trigger immediate revisionDetectors"
		myRevisionWatchers ~~ NULL ifTrue:
			[myRevisionWatchers stepper forEach: [ :work {FeWork} |
				work triggerRevisionDetectors: edition
					with: myReviser
					with: myRevisionTime
					with: myRevisionCount]].
		"Record result into the trail"
		myHistoryClub ~~ NULL ifTrue: [self recordHistory].
		self diskUpdate]!
*/
}
/**
 * If there isn't already a shared Trail on this Work, create a new one. Return it
 */
public BeEdition revisions() {
	if (myHistory == null) {
		AboraBlockSupport.enterConsistent();
		try {
			myHistory = ((BeGrandMap) CurrentGrandMap.fluidGet()).newEmptyEdition(IntegerSpace.make());
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return myHistory;
/*
udanax-top.st:3706:BeWork methodsFor: 'contents'!
{BeEdition} revisions
	"If there isn't already a shared Trail on this Work, create a new one. Return it"
	
	myHistory == NULL ifTrue: 
		[DiskManager consistent: 
			[myHistory _ CurrentGrandMap fluidGet newEmptyEdition: IntegerSpace make.
			self diskUpdate]].
	^myHistory!
*/
}
/**
 * The edit Club, or NULL if there is none
 */
public ID fetchEditClub() {
	return myEditClub;
/*
udanax-top.st:3717:BeWork methodsFor: 'permissions'!
{ID | NULL} fetchEditClub
	"The edit Club, or NULL if there is none"
	^myEditClub!
*/
}
/**
 * The history Club, or NULL if there is none
 */
public ID fetchHistoryClub() {
	return myHistoryClub;
/*
udanax-top.st:3722:BeWork methodsFor: 'permissions'!
{ID | NULL} fetchHistoryClub
	"The history Club, or NULL if there is none"
	^myHistoryClub!
*/
}
/**
 * The read Club, or NULL if there is none
 */
public ID fetchReadClub() {
	return myReadClub;
/*
udanax-top.st:3726:BeWork methodsFor: 'permissions'!
{ID | NULL} fetchReadClub
	"The read Club, or NULL if there is none"
	^myReadClub!
*/
}
/**
 * Change the edit Club (or remove it if NULL).
 */
public void setEditClub(ID club) {
	AboraBlockSupport.enterConsistent(1);
	try {
		myEditClub = club;
		Someone.knownBug();
		/* props */
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	updateFeStatus();
/*
udanax-top.st:3730:BeWork methodsFor: 'permissions'!
{void} setEditClub: club {ID | NULL}
	"Change the edit Club (or remove it if NULL)."
	
	DiskManager consistent: 1 with:
		[myEditClub := club.
		self knownBug. "props"
		self diskUpdate].
	self updateFeStatus.!
*/
}
/**
 * Change the history Club (or remove it if NULL).
 */
public void setHistoryClub(ID club) {
	AboraBlockSupport.enterConsistent();
	try {
		ID oldClub;
		oldClub = myHistoryClub;
		myHistoryClub = club;
		Someone.knownBug();
		/* What happens when you change the club. */
		if (oldClub == null && (myHistoryClub != null)) {
			recordHistory();
		}
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:3739:BeWork methodsFor: 'permissions'!
{void} setHistoryClub: club {ID | NULL}
	"Change the history Club (or remove it if NULL)."
	
	DiskManager consistent:
		[| oldClub {ID | NULL} |
		oldClub _ myHistoryClub.
		myHistoryClub := club.
		self knownBug.  "What happens when you change the club."
		(oldClub == NULL and: [myHistoryClub ~~ NULL])
			ifTrue: [self recordHistory].
		self diskUpdate].!
*/
}
/**
 * Change the read Club (or remove it if NULL).
 */
public void setReadClub(ID club) {
	AboraBlockSupport.enterConsistent();
	try {
		myReadClub = club;
		Someone.knownBug();
		/* props */
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	updateFeStatus();
/*
udanax-top.st:3751:BeWork methodsFor: 'permissions'!
{void} setReadClub: club {ID | NULL}
	"Change the read Club (or remove it if NULL)."
	
	DiskManager consistent:
		[myReadClub := club.
		self knownBug. "props"
		self diskUpdate].
	self updateFeStatus.!
*/
}
/**
 * Adds to the endorsements on this Work. The set of endorsements must be a finite number of
 * (club ID, token ID) pairs. This requires the authority of all of the Clubs used to
 * endorse. The token IDs must not be named IDs.
 */
public void endorse(CrossRegion endorsements) {
	if (endorsements.isEmpty()) {
		return ;
	}
	AboraBlockSupport.enterConsistent(8);
	try {
		propChange(PropChange.endorsementsChange(), (BertProp.endorsementsProp((endorsements.unionWith(myOwnProp.endorsements())))));
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:3762:BeWork methodsFor: 'props'!
{void} endorse: endorsements {CrossRegion}
	"Adds to the endorsements on this Work. The set of endorsements must be a finite number of (club ID, token ID) pairs. This requires the authority of all of the Clubs used to endorse. The token IDs must not be named IDs."
	
	endorsements isEmpty
		ifTrue: [^VOID].
	DiskManager consistent: 8 with:
		[self 
			propChange: PropChange endorsementsChange 
			with: (BertProp endorsementsProp: (endorsements unionWith: myOwnProp endorsements))]!
*/
}
/**
 * All endorsements which have been placed on this Work. The Edition::transclusions ()
 * operation will be able to find the current Edition of this Work by filtering for these
 * endorsements; they are also used to filter various other operations which directly return
 * sets of Works.
 */
public CrossRegion endorsements() {
	return (CrossRegion) myOwnProp.endorsements();
/*
udanax-top.st:3772:BeWork methodsFor: 'props'!
{CrossRegion} endorsements
	"All endorsements which have been placed on this Work. The Edition::transclusions () operation will be able to find the current Edition of this Work by filtering for these endorsements; they are also used to filter various other operations which directly return sets of Works."
	
	^myOwnProp endorsements cast: CrossRegion!
*/
}
public BertProp localProp() {
	return myOwnProp;
/*
udanax-top.st:3777:BeWork methodsFor: 'props'!
{BertProp} localProp
	^myOwnProp!
*/
}
public BertProp prop() {
	return myOwnProp;
/*
udanax-top.st:3781:BeWork methodsFor: 'props'!
{BertProp} prop
	^myOwnProp!
*/
}
public void propChange(PropChange change, Prop nw) {
	Prop old;
	old = myOwnProp;
	if ( ! (change.areEqualProps(old, nw))) {
		myOwnProp = (BertProp) (change.changed(old, nw));
		diskUpdate();
		myEdition.propChanged(change, old, nw, (change.fetchFinder(old, nw, this, null)));
	}
/*
udanax-top.st:3785:BeWork methodsFor: 'props'!
{void} propChange: change {PropChange} with: nw {Prop} 
	
	| old {Prop} |
	old _ myOwnProp.
	(change areEqualProps: old with: nw) not
		ifTrue: 
			[myOwnProp _ (change changed: old with: nw) cast: BertProp.
			self diskUpdate.
			myEdition propChanged: change
				with: old
				with: nw
				with: (change fetchFinder: old
					with: nw
					with: self
					with: NULL)]!
*/
}
/**
 * Removes endorsements from this Work. This requires the authority of all of the Clubs whose
 * endorsements are in the list. Ignores all endorsements which you could have removed, but
 * which don't happen to be there right now.
 */
public void retract(CrossRegion endorsements) {
	if (endorsements.isEmpty()) {
		return ;
	}
	AboraBlockSupport.enterConsistent(5);
	try {
		propChange(PropChange.endorsementsChange(), (BertProp.endorsementsProp((myOwnProp.endorsements().minus(endorsements)))));
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:3801:BeWork methodsFor: 'props'!
{void} retract: endorsements {CrossRegion}
	"Removes endorsements from this Work. This requires the authority of all of the Clubs whose endorsements are in the list. Ignores all endorsements which you could have removed, but which don't happen to be there right now."
	
	endorsements isEmpty
		ifTrue: [^VOID].
	DiskManager consistent: 5 with:
		[self 
			propChange: PropChange endorsementsChange 
			with: (BertProp endorsementsProp: (myOwnProp endorsements minus: endorsements))]!
*/
}
public boolean isPurgeable() {
	return super.isPurgeable() && (fetchLockingWork() == null && (myRevisionWatchers == null));
/*
udanax-top.st:3813:BeWork methodsFor: 'accessing'!
{BooleanVar} isPurgeable
	^super isPurgeable and: [self fetchLockingWork == NULL and: [myRevisionWatchers == NULL]]!
*/
}
public FeRangeElement makeFe(BeLabel label) {
	return FeWork.on(this);
/*
udanax-top.st:3816:BeWork methodsFor: 'accessing'!
{FeRangeElement} makeFe: label {BeLabel | NULL}
	^FeWork on: self!
*/
}
/**
 * Add new sponsors to the Work, and notify the Clubs
 */
public void sponsor(IDRegion clubs) {
	IDRegion newClubs;
	newClubs = (IDRegion) (clubs.minus(mySponsors));
	if ( ! (newClubs.isEmpty())) {
		AboraBlockSupport.enterConsistent(newClubs.count() + 1);
		try {
			Stepper stomper = newClubs.stepper();
			for (; stomper.hasValue(); stomper.step()) {
				ID clubID = (ID) stomper.fetch();
				if (clubID == null) {
					continue ;
				}
				(((BeGrandMap) CurrentGrandMap.fluidGet()).getClub(clubID)).addSponsored(this);
			}
			stomper.destroy();
			mySponsors = (IDRegion) (mySponsors.unionWith(newClubs));
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
/*
udanax-top.st:3820:BeWork methodsFor: 'accessing'!
{void} sponsor: clubs {IDRegion}
	"Add new sponsors to the Work, and notify the Clubs"
	
	| newClubs {IDRegion} |
	newClubs := (clubs minus: mySponsors) cast: IDRegion.
	newClubs isEmpty ifFalse:
		[DiskManager consistent: newClubs count + 1 with: 
			[newClubs stepper forEach: [ :clubID {ID} |
				(CurrentGrandMap fluidGet getClub: clubID)
					addSponsored: self].
			mySponsors := (mySponsors unionWith: newClubs) cast: IDRegion.
			self diskUpdate]]!
*/
}
public IDRegion sponsors() {
	return mySponsors;
/*
udanax-top.st:3833:BeWork methodsFor: 'accessing'!
{IDRegion} sponsors
	^mySponsors!
*/
}
/**
 * Remove sponsors from the Work, and notify the Clubs
 */
public void unsponsor(IDRegion clubs) {
	IDRegion lostClubs;
	Someone.thingToDo();
	/* Remove unsponsored clubs from the grandmap. */
	Someone.thingToDo();
	/* When Clubs can have multiple IDs, then it might still be in the set */
	lostClubs = (IDRegion) (clubs.intersect(mySponsors));
	if ( ! (lostClubs.isEmpty())) {
		AboraBlockSupport.enterConsistent(lostClubs.count() + 1);
		try {
			Stepper stomper = lostClubs.stepper();
			for (; stomper.hasValue(); stomper.step()) {
				ID clubID = (ID) stomper.fetch();
				if (clubID == null) {
					continue ;
				}
				(((BeGrandMap) CurrentGrandMap.fluidGet()).getClub(clubID)).removeSponsored(this);
			}
			stomper.destroy();
			mySponsors = (IDRegion) (mySponsors.minus(clubs));
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
/*
udanax-top.st:3837:BeWork methodsFor: 'accessing'!
{void} unsponsor: clubs {IDRegion}
	"Remove sponsors from the Work, and notify the Clubs"
	
	| lostClubs {IDRegion} |
	self thingToDo.  "Remove unsponsored clubs from the grandmap."
	self thingToDo. "When Clubs can have multiple IDs, then it might still be in the set"
	lostClubs := (clubs intersect: mySponsors) cast: IDRegion.
	lostClubs isEmpty ifFalse:
		[DiskManager consistent: lostClubs count + 1 with:
			[lostClubs stepper forEach: [ :clubID {ID} |
				(CurrentGrandMap fluidGet getClub: clubID) removeSponsored: self].
			mySponsors := (mySponsors minus: clubs) cast: IDRegion.
			self diskUpdate]]!
*/
}
/**
 * Tell all the FeWorks on this one to update their status
 */
public void updateFeStatus() {
	Stepper stomper = feRangeElements().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		FeWork work = (FeWork) stomper.fetch();
		if (work == null) {
			continue ;
		}
		work.updateStatus();
	}
	stomper.destroy();
/*
udanax-top.st:3853:BeWork methodsFor: 'private:'!
{void} updateFeStatus
	"Tell all the FeWorks on this one to update their status"
	[PrimSet] USES.
	self feRangeElements stepper forEach: [ :work {FeWork} | work updateStatus]!
*/
}
public void restartWork(Rcvr rcvr) {
	myLockingWork = (WeakPtrArray) WeakPtrArray.make((BeWorkLockExecutor.make(this)), 1);
	myRevisionWatchers = null;
/*
udanax-top.st:3860:BeWork methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartWork: rcvr {Rcvr unused}
	myLockingWork _ WeakPtrArray make: (BeWorkLockExecutor make: self) with: 1.
	myRevisionWatchers _ NULL!
*/
}
/**
 * @deprecated
 */
public void addSponsors(IDRegion clubs) {
	throw new PasseException();
/*
udanax-top.st:3866:BeWork methodsFor: 'smalltalk: passe'!
{void} addSponsors: clubs {IDRegion}
	self passe "sponsor"!
*/
}
/**
 * @deprecated
 */
public void removeSponsors(IDRegion clubs) {
	throw new PasseException();
/*
udanax-top.st:3870:BeWork methodsFor: 'smalltalk: passe'!
{void} removeSponsors: clubs {IDRegion}
	self passe!
*/
}
/**
 * @deprecated
 */
public void unendorse(CrossRegion endorsements) {
	throw new PasseException();
/*
udanax-top.st:3874:BeWork methodsFor: 'smalltalk: passe'!
{void} unendorse: endorsements {CrossRegion}
	self passe!
*/
}
public BeWork(FeEdition contents, boolean isClub) {
	super();
	XnRegion permissions;
	myEdition = contents.beEdition();
	myEditionLabel = (BeLabel) contents.label().getOrMakeBe();
	myReadClub = ((ID) InitialReadClub.fluidFetch());
	if (myReadClub == null) {
		permissions = ((BeGrandMap) CurrentGrandMap.fluidGet()).globalIDSpace().emptyRegion();
	}
	else {
		permissions = myReadClub.asRegion();
	}
	myEditClub = ((ID) InitialEditClub.fluidFetch());
	if (myEditClub != null) {
		permissions = permissions.with(myEditClub);
	}
	myOwnProp = BertProp.permissionsProp(permissions);
	myRevisionCount = 0;
	myRevisionTime = AboraSupport.xuTime();
	myReviser = ((ID) CurrentAuthor.fluidGet());
	myHistory = null;
	myHistoryClub = null;
	Someone.knownBug();
	/* Should public shut off sponsorship? */
	if (((ID) InitialSponsor.fluidGet()) == ((BeGrandMap) CurrentGrandMap.fluidGet()).emptyClubID()) {
		mySponsors = (IDRegion) IDSpace.global().emptyRegion();
	}
	else {
		mySponsors = (IDRegion) ((ID) InitialSponsor.fluidFetch()).asRegion();
	}
	restartWork(null);
	myEdition.introduceWork(this);
	Someone.knownBug();
	/* Is the above all right? */
	if ( ! (isClub)) {
		finishCreation();
	}
/*
udanax-top.st:3880:BeWork methodsFor: 'creation'!
create: contents {FeEdition} with: isClub {BooleanVar}
	
	| permissions {XnRegion} |
	super create.
	myEdition := contents beEdition.
	myEditionLabel _ contents label getOrMakeBe cast: BeLabel.
	myReadClub := InitialReadClub fluidFetch.
	myReadClub == NULL
		ifTrue: [permissions := CurrentGrandMap fluidGet globalIDSpace emptyRegion]
		ifFalse: [permissions := myReadClub asRegion].
	myEditClub := InitialEditClub fluidFetch.
	myEditClub ~~ NULL ifTrue:
		[permissions := permissions with: myEditClub].
	myOwnProp := BertProp permissionsProp: permissions.
	myRevisionCount _ IntegerVarZero.
	myRevisionTime _ Time xuTime.
	myReviser _ CurrentAuthor fluidGet.
	myHistory _ NULL.
	myHistoryClub _ NULL.
	self knownBug.  "Should public shut off sponsorship?"
	InitialSponsor fluidGet == CurrentGrandMap fluidGet emptyClubID
		ifTrue: [mySponsors := IDSpace global emptyRegion cast: IDRegion]
		ifFalse: [mySponsors := InitialSponsor fluidFetch asRegion cast: IDRegion].
	self restartWork: NULL.
	myEdition introduceWork: self.
	
	self knownBug.  "Is the above all right?"
	isClub ifFalse: [self finishCreation.]!
*/
}
/**
 * Gets called once the object is created, to finish up
 */
public void finishCreation() {
	Stepper stomper = mySponsors.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ID iD = (ID) stomper.fetch();
		if (iD == null) {
			continue ;
		}
		(((BeGrandMap) CurrentGrandMap.fluidGet()).getClub(iD)).addSponsored(this);
	}
	stomper.destroy();
	newShepherd();
/*
udanax-top.st:3909:BeWork methodsFor: 'creation'!
{void} finishCreation
	"Gets called once the object is created, to finish up"
	
	mySponsors stepper forEach: [ :iD {ID} |
		(CurrentGrandMap fluidGet getClub: iD) addSponsored: self].
	self newShepherd.!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print((((BeGrandMap) CurrentGrandMap.fluidGet()).iDsOf(this)));
	oo.print(")");
/*
udanax-top.st:3918:BeWork methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << (CurrentGrandMap fluidGet iDsOf: self) << ')'!
*/
}
public BeWork(Rcvr receiver) {
	super(receiver);
	myEdition = (BeEdition) receiver.receiveHeaper();
	myEditionLabel = (BeLabel) receiver.receiveHeaper();
	myReadClub = (ID) receiver.receiveHeaper();
	myEditClub = (ID) receiver.receiveHeaper();
	myOwnProp = (BertProp) receiver.receiveHeaper();
	myHistory = (BeEdition) receiver.receiveHeaper();
	myHistoryClub = (ID) receiver.receiveHeaper();
	myRevisionCount = receiver.receiveIntegerVar();
	myRevisionTime = receiver.receiveIntegerVar();
	myReviser = (ID) receiver.receiveHeaper();
	mySponsors = (IDRegion) receiver.receiveHeaper();
	restartWork(receiver);
/*
udanax-top.st:3924:BeWork methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myEdition _ receiver receiveHeaper.
	myEditionLabel _ receiver receiveHeaper.
	myReadClub _ receiver receiveHeaper.
	myEditClub _ receiver receiveHeaper.
	myOwnProp _ receiver receiveHeaper.
	myHistory _ receiver receiveHeaper.
	myHistoryClub _ receiver receiveHeaper.
	myRevisionCount _ receiver receiveIntegerVar.
	myRevisionTime _ receiver receiveIntegerVar.
	myReviser _ receiver receiveHeaper.
	mySponsors _ receiver receiveHeaper.
	self restartWork: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myEdition);
	xmtr.sendHeaper(myEditionLabel);
	xmtr.sendHeaper(myReadClub);
	xmtr.sendHeaper(myEditClub);
	xmtr.sendHeaper(myOwnProp);
	xmtr.sendHeaper(myHistory);
	xmtr.sendHeaper(myHistoryClub);
	xmtr.sendIntegerVar(myRevisionCount);
	xmtr.sendIntegerVar(myRevisionTime);
	xmtr.sendHeaper(myReviser);
	xmtr.sendHeaper(mySponsors);
/*
udanax-top.st:3939:BeWork methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myEdition.
	xmtr sendHeaper: myEditionLabel.
	xmtr sendHeaper: myReadClub.
	xmtr sendHeaper: myEditClub.
	xmtr sendHeaper: myOwnProp.
	xmtr sendHeaper: myHistory.
	xmtr sendHeaper: myHistoryClub.
	xmtr sendIntegerVar: myRevisionCount.
	xmtr sendIntegerVar: myRevisionTime.
	xmtr sendHeaper: myReviser.
	xmtr sendHeaper: mySponsors.!
*/
}
public static BeWork make(FeEdition edition) {
	AboraBlockSupport.enterConsistent();
	try {
		return new BeWork(edition, false);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:3965:BeWork class methodsFor: 'creation'!
make: edition {FeEdition}
	DiskManager consistent:
		[^self create: edition with: false]!
*/
}
public BeWork() {
/*

Generated during transformation
*/
}
}
