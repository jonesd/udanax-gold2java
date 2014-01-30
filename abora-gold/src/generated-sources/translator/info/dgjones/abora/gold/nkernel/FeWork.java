/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.detect.FeRevisionDetector;
import info.dgjones.abora.gold.detect.FeStatusDetector;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nkernel.RevisionDetectorExecutor;
import info.dgjones.abora.gold.nkernel.StatusDetectorExecutor;
import info.dgjones.abora.gold.primtab.PrimSet;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * A persistent identity for a changeable object.
 */
public class FeWork extends FeRangeElement {

	protected FeKeyMaster myKeyMaster;
	protected ID myAuthor;
	protected boolean amWaiting;
	protected BeWork myBeWork;
	protected PrimSet myStatusDetectors;
	protected PrimSet myRevisionDetectors;
/*
udanax-top.st:22063:
FeRangeElement subclass: #FeWork
	instanceVariableNames: '
		myKeyMaster {FeKeyMaster | NULL}
		myAuthor {ID}
		amWaiting {BooleanVar}
		myBeWork {BeWork}
		myStatusDetectors {PrimSet | NULL of: FeStatusDetector}
		myRevisionDetectors {PrimSet | NULL of: FeRevisionDetector}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:22073:
FeWork comment:
'A persistent identity for a changeable object.'!
*/
/*
udanax-top.st:22075:
(FeWork getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:22584:
FeWork class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:22587:
(FeWork getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeWork.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential.  Add a detector which will be notified whenever the locking status of this Work
 * object changes.
 * See FeStatusDetector::grabbed (Work *, ID *) / released (Work *).
 */
public void addStatusDetector(FeStatusDetector detector) {
	if (myStatusDetectors == null) {
		myStatusDetectors = PrimSet.weak(7, (StatusDetectorExecutor.make(this)));
	}
	myStatusDetectors.introduce(detector);
/*
udanax-top.st:22080:FeWork methodsFor: 'grab status'!
{void} addStatusDetector: detector {FeStatusDetector}
	"Essential.  Add a detector which will be notified whenever the locking status of this Work object changes.
	See FeStatusDetector::grabbed (Work *, ID *) / released (Work *)."
	
	myStatusDetectors == NULL ifTrue:
		[myStatusDetectors := PrimSet weak: 7 with: (StatusDetectorExecutor make: self)].
	myStatusDetectors introduce: detector!
*/
}
/**
 * Return whether you have read permission.  If grabbed, returns TRUE (because a grabber can
 * always read); if released, then returns whether the CurrentKeyMaster has sufficient
 * permission to read the work.  (Read or Edit permission is required.)  Does not check any
 * other KeyMasters you may be holding.
 * Note: Be careful of synchronization problems, since the permissions may change between
 * when you ask this question and when you try to actually read the Work.
 */
public boolean canRead() {
	FeKeyMaster ckm;
	ckm = ((FeKeyMaster) CurrentKeyMaster.fluidFetch());
	return canRevise() || (ckm != null && (myBeWork.canBeReadBy(ckm)));
/*
udanax-top.st:22088:FeWork methodsFor: 'grab status'!
{BooleanVar CLIENT} canRead
	"Return whether you have read permission.  If grabbed, returns TRUE (because a grabber can always read); if released, then returns whether the CurrentKeyMaster has sufficient permission to read the work.  (Read or Edit permission is required.)  Does not check any other KeyMasters you may be holding.
	Note: Be careful of synchronization problems, since the permissions may change between when you ask this question and when you try to actually read the Work."
	| ckm {FeKeyMaster} |
	ckm := CurrentKeyMaster fluidFetch.
	^self canRevise
		or: [ckm ~~ NULL
			and: [myBeWork canBeReadBy: ckm]]!
*/
}
/**
 * Return whether the BeWork is grabbed by you through this FeWork.
 * Note: Be careful of synchronization problems, since the permissions may change before you
 * try to actually revise it, causing you to lose your grab.
 */
public boolean canRevise() {
	return ((Heaper) myBeWork.fetchLockingWork()) == this;
/*
udanax-top.st:22097:FeWork methodsFor: 'grab status'!
{BooleanVar CLIENT} canRevise
	"Return whether the BeWork is grabbed by you through this FeWork.
	Note: Be careful of synchronization problems, since the permissions may change before you try to actually revise it, causing you to lose your grab."
	
	^(myBeWork fetchLockingWork basicCast: Heaper star) == self!
*/
}
/**
 * Essential.  Grab the Work to prevent other clients from revising it.  Requires edit
 * permission. Snapshots the CurrentKeyMaster and CurrentAuthor (to be used to maintain the
 * grab and report what was done with it). Fails if
 * - someone else has it grabbed
 * - the CurrentKeyMaster does not have edit permission
 * - the CurrentKeyMaster does not have signature authority of the CurrentAuthor
 * If this Work was already grabbed by you, then it updates the KeyMaster and Author it
 * holds. (If the regrab fails, the old grab will remain in effect.)
 * The grab will be released
 * - upon a release request
 * - if the KeyMaster loses authority to edit
 * - if the KeyMaster loses the signature authority of the Author
 * - at the end of the session
 * - when the FeWork object is deallocated (if an FeWork was dropped while grabbed, {by
 * destroying the promise for it, or by loss of connection} it will be deallocated
 * 'eventually')
 */
public void grab() {
	ID oldAuthor;
	/* Check that I have edit permissions */
	if ( ! (myBeWork.canBeEditedBy(((FeKeyMaster) CurrentKeyMaster.fluidGet())))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_EDIT_PERMISSION);
	}
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasSignatureAuthority(((ID) CurrentAuthor.fluidGet())))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_AUTHOR_SIGNATURE_AUTHORITY);
	}
	oldAuthor = myAuthor;
	myAuthor = ((ID) CurrentAuthor.fluidFetch());
	if (myKeyMaster != null) {
		myKeyMaster.unregisterWork(this);
	}
	myKeyMaster = ((FeKeyMaster) CurrentKeyMaster.fluidFetch());
	myKeyMaster.registerWork(this);
	/* Try to gain mutual exclusion */
	if ( ! (myBeWork.tryLock(this))) {
		myAuthor = null;
		myKeyMaster = null;
		throw new AboraRuntimeException(AboraRuntimeException.WORK_IS_LOCKED_BY_SOMEONE_ELSE);
	}
	if (amWaiting) {
		/* code has been changed in such a way as to allow a race condition */
		throw new AboraRuntimeException(AboraRuntimeException.FATAL_ERROR);
	}
	Ravi.thingToDo();
	/* register with author Club to find out when signature authority changes */
	/* Notify all the status detectors */
	if (myStatusDetectors != null && (oldAuthor == null || ( ! (oldAuthor.isEqual(myAuthor))))) {
		Stepper stomper = myStatusDetectors.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			FeStatusDetector stat = (FeStatusDetector) stomper.fetch();
			if (stat == null) {
				continue ;
			}
			Someone.thingToDo();
			/* reasons */
			stat.grabbed(this, myAuthor, 0);
		}
		stomper.destroy();
	}
/*
udanax-top.st:22103:FeWork methodsFor: 'grab status'!
{void CLIENT} grab
	"Essential.  Grab the Work to prevent other clients from revising it.  Requires edit permission. Snapshots the CurrentKeyMaster and CurrentAuthor (to be used to maintain the grab and report what was done with it). Fails if
		- someone else has it grabbed
		- the CurrentKeyMaster does not have edit permission
		- the CurrentKeyMaster does not have signature authority of the CurrentAuthor
	If this Work was already grabbed by you, then it updates the KeyMaster and Author it holds. (If the regrab fails, the old grab will remain in effect.)
	The grab will be released
		- upon a release request
		- if the KeyMaster loses authority to edit
		- if the KeyMaster loses the signature authority of the Author
		- at the end of the session
		- when the FeWork object is deallocated (if an FeWork was dropped while grabbed, {by destroying the promise for it, or by loss of connection} it will be deallocated 'eventually')"
	| oldAuthor {ID} |
	"Check that I have edit permissions"
	(myBeWork canBeEditedBy: CurrentKeyMaster fluidGet)
		ifFalse: [Heaper BLAST: #MustHaveEditPermission].
	(CurrentKeyMaster fluidGet hasSignatureAuthority: CurrentAuthor fluidGet)
		ifFalse: [Heaper BLAST: #MustHaveAuthorSignatureAuthority].
	oldAuthor := myAuthor.
	myAuthor := CurrentAuthor fluidFetch.
	myKeyMaster ~~ NULL ifTrue:
		[myKeyMaster unregisterWork: self].
	myKeyMaster := CurrentKeyMaster fluidFetch.
	myKeyMaster registerWork: self.
	"Try to gain mutual exclusion"
	(myBeWork tryLock: self) ifFalse:
		[myAuthor := NULL.
		myKeyMaster := NULL.
		Heaper BLAST: #WorkIsLockedBySomeoneElse].
	amWaiting ifTrue:
		["code has been changed in such a way as to allow a race condition"
		Heaper BLAST: #FatalError].
	Ravi thingToDo. "register with author Club to find out when signature authority changes"
	"Notify all the status detectors"
	(myStatusDetectors ~~ NULL and: [oldAuthor == NULL or: [(oldAuthor isEqual: myAuthor) not]]) ifTrue:
		[myStatusDetectors stepper forEach: [ :stat {FeStatusDetector} |
			self thingToDo. "reasons"
			stat grabbed:  self with: myAuthor with: IntegerVarZero]].!
*/
}
/**
 * Essential.  If you have edit authority, and someone has the BeWork grabbed, then return
 * the Club ID that was the value of his CurrentAuthor when he grabbed it; otherwise blast.
 * Requiring edit authority is appropriate here, because it is exactly editors who are
 * affected by competing grabs, and need to know who has the grab.  Once the BeWork is
 * revised, anyone who can read the current trail can see the revision, but the grab state
 * doesn't necessarily imply that the BeWork will be revised soon, or ever.
 */
public ID grabber() {
	FeWork grabber;
	FeKeyMaster ckm;
	if (canRevise()) {
		return myAuthor;
	}
	ckm = ((FeKeyMaster) CurrentKeyMaster.fluidGet());
	if ( ! (myBeWork.fetchEditClub() != null && (ckm.hasAuthority(myBeWork.fetchEditClub())))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_EDIT_AUTHORITY);
	}
	grabber = myBeWork.fetchLockingWork();
	if (grabber == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_GRABBED);
	}
	return grabber.getAuthor();
/*
udanax-top.st:22143:FeWork methodsFor: 'grab status'!
{ID CLIENT} grabber
	"Essential.  If you have edit authority, and someone has the BeWork grabbed, then return the Club ID that was the value of his CurrentAuthor when he grabbed it; otherwise blast.
	Requiring edit authority is appropriate here, because it is exactly editors who are affected by competing grabs, and need to know who has the grab.  Once the BeWork is revised, anyone who can read the current trail can see the revision, but the grab state doesn't necessarily imply that the BeWork will be revised soon, or ever."
	
	| grabber {FeWork} ckm {FeKeyMaster} |
	self canRevise ifTrue:
		[^myAuthor].
	ckm := CurrentKeyMaster fluidGet.
	(myBeWork fetchEditClub ~~ NULL
			and: [ckm hasAuthority: myBeWork fetchEditClub])
		ifFalse: [Heaper BLAST: #MustHaveEditAuthority].
	grabber := myBeWork fetchLockingWork.
	grabber == NULL ifTrue:
		[Heaper BLAST: #NotGrabbed].
	^grabber getAuthor!
*/
}
/**
 * Essential.  Release the grab on this Work; if a requestGrab had been pending, remove it.
 * Does nothing if it is already unlocked.
 */
public void release() {
	boolean becameUnlocked;
	if ( ! (amWaiting || (canRevise()))) {
		return ;
	}
	becameUnlocked = myBeWork.tryUnlock(this);
	myKeyMaster.unregisterWork(this);
	amWaiting = false;
	myKeyMaster = null;
	myAuthor = null;
	if (becameUnlocked) {
		/* Notify all the status detectors */
		if (myStatusDetectors != null) {
			Stepper stomper = myStatusDetectors.stepper();
			for (; stomper.hasValue(); stomper.step()) {
				FeStatusDetector stat = (FeStatusDetector) stomper.fetch();
				if (stat == null) {
					continue ;
				}
				stat.released(this, 0);
			}
			stomper.destroy();
		}
	}
/*
udanax-top.st:22159:FeWork methodsFor: 'grab status'!
{void CLIENT} release
	"Essential.  Release the grab on this Work; if a requestGrab had been pending, remove it. Does nothing if it is already unlocked."
	
	| becameUnlocked {BooleanVar} |
	(amWaiting or: [self canRevise]) ifFalse: [^VOID].
	becameUnlocked := myBeWork tryUnlock: self.
	myKeyMaster unregisterWork: self.
	amWaiting := false.
	myKeyMaster := NULL.
	myAuthor := NULL.
	becameUnlocked ifTrue:
		["Notify all the status detectors"
		myStatusDetectors ~~ NULL ifTrue:
			[myStatusDetectors stepper forEach: [ :stat {FeStatusDetector} |
				stat released: self with: IntegerVarZero]]].!
*/
}
/**
 * Essential.  Last detector has gone away
 */
public void removeLastStatusDetector() {
	myStatusDetectors = null;
/*
udanax-top.st:22175:FeWork methodsFor: 'grab status'!
{void} removeLastStatusDetector
	"Essential.  Last detector has gone away"
	
	myStatusDetectors := NULL!
*/
}
/**
 * Essential.  Registers a request so that the next time this Work would have been released
 * and no other grab requests are outstanding the CurrentKeyMaster (as of making the request)
 * has edit permission, and has signature authority of the CurrentAuthor (as of making the
 * request), it will be grabbed by this FeWork.  If this FeWork already has the Work grabbed,
 * then the request has no effect.  To find out when the grab succeeds, place Status
 * Detectors on the Work.  (If there are competing requestGrabs for a BeWork, the queueing of
 * the requests may not be FIFO, but is starvation-free.)  Note that if you have a
 * requestGrab outstanding on a BeWork through one FeWork, and release a grab you have
 * through another, your requestGrab has no special priority over those of other users.
 */
public void requestGrab() {
	if (canRevise()) {
		if ( ! (myBeWork.canBeEditedBy(((FeKeyMaster) CurrentKeyMaster.fluidGet())))) {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_EDIT_PERMISSION);
		}
		if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasSignatureAuthority(((ID) CurrentAuthor.fluidGet())))) {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_AUTHOR_SIGNATURE_AUTHORITY);
		}
		myAuthor = ((ID) CurrentAuthor.fluidFetch());
		myKeyMaster.unregisterWork(this);
		myKeyMaster = ((FeKeyMaster) CurrentKeyMaster.fluidFetch());
		myKeyMaster.registerWork(this);
		return ;
	}
	if (amWaiting) {
		myKeyMaster.unregisterWork(this);
	}
	amWaiting = true;
	myKeyMaster = ((FeKeyMaster) CurrentKeyMaster.fluidGet());
	myAuthor = ((ID) CurrentAuthor.fluidGet());
	updateStatus();
	myKeyMaster.registerWork(this);
/*
udanax-top.st:22180:FeWork methodsFor: 'grab status'!
{void CLIENT} requestGrab
	"Essential.  Registers a request so that the next time this Work would have been released and no other grab requests are outstanding the CurrentKeyMaster (as of making the request) has edit permission, and has signature authority of the CurrentAuthor (as of making the request), it will be grabbed by this FeWork.  If this FeWork already has the Work grabbed, then the request has no effect.  To find out when the grab succeeds, place Status Detectors on the Work.  (If there are competing requestGrabs for a BeWork, the queueing of the requests may not be FIFO, but is starvation-free.)  Note that if you have a requestGrab outstanding on a BeWork through one FeWork, and release a grab you have through another, your requestGrab has no special priority over those of other users."
	
	self canRevise ifTrue: 
		[(myBeWork canBeEditedBy: CurrentKeyMaster fluidGet)
			ifFalse: [Heaper BLAST: #MustHaveEditPermission].
		(CurrentKeyMaster fluidGet hasSignatureAuthority: CurrentAuthor fluidGet)
			ifFalse: [Heaper BLAST: #MustHaveAuthorSignatureAuthority].
		myAuthor := CurrentAuthor fluidFetch.
		myKeyMaster unregisterWork: self.
		myKeyMaster := CurrentKeyMaster fluidFetch.
		myKeyMaster registerWork: self.
		^VOID].
	amWaiting ifTrue:
		[myKeyMaster unregisterWork: self].
	amWaiting := true.
	myKeyMaster := CurrentKeyMaster fluidGet.
	myAuthor := CurrentAuthor fluidGet.
	self updateStatus.
	myKeyMaster registerWork: self.!
*/
}
/**
 * Essential.  Return a detector which will be notified whenever the locking status of this
 * Work changes.
 * See FeStatusDetector::grabbed (Work *, ID *) / released (Work *).
 */
public FeStatusDetector statusDetector() {
	Dean.shouldImplement();
	addStatusDetector(null);
	return null;
/*
udanax-top.st:22201:FeWork methodsFor: 'grab status'!
{FeStatusDetector CLIENT} statusDetector
	"Essential.  Return a detector which will be notified whenever the locking status of this Work changes.
	See FeStatusDetector::grabbed (Work *, ID *) / released (Work *)."
	
	Dean shouldImplement.
	self addStatusDetector: NULL.
	^NULL "fodder"!
*/
}
/**
 * Essential.  Return the current Edition.  Succeeds if the Work is already grabbed, or if
 * the CurrentKeyMaster has either Read or Edit permission.
 * Note: If this is an unsponsored Work, the Edition might have been discarded, in which case
 * this operation will blast.
 */
public FeEdition edition() {
	if ( ! (canRead())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_READ_PERMISSION);
	}
	return myBeWork.edition();
/*
udanax-top.st:22211:FeWork methodsFor: 'contents'!
{FeEdition CLIENT} edition
	"Essential.  Return the current Edition.  Succeeds if the Work is already grabbed, or if the CurrentKeyMaster has either Read or Edit permission.
	Note: If this is an unsponsored Work, the Edition might have been discarded, in which case this operation will blast."
	
	self canRead ifFalse:
		[Heaper BLAST: #MustHaveReadPermission].
	^myBeWork edition!
*/
}
/**
 * Essential.  Change the current Edition of this work to newEdition. The Work must be
 * grabbed  The grabber is recorded as the author who made the revision.
 * (This is the fundamental write operation.)
 */
public void revise(FeEdition newEdition) {
	if ( ! (canRevise())) {
		throw new AboraRuntimeException(AboraRuntimeException.WORK_MUST_BE_GRABBED);
	}
	Object currentKeyMasterOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentKeyMaster, myKeyMaster);
	try {
		Object currentAuthorOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentAuthor, myAuthor);
		try {
			myBeWork.revise(newEdition);
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentAuthor, currentAuthorOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentKeyMaster, currentKeyMasterOldValue);
	}
/*
udanax-top.st:22219:FeWork methodsFor: 'contents'!
{void CLIENT} revise: newEdition {FeEdition}
	"Essential.  Change the current Edition of this work to newEdition. The Work must be grabbed  The grabber is recorded as the author who made the revision.
	 (This is the fundamental write operation.)"
	
	self canRevise ifFalse:
		[Heaper BLAST: #WorkMustBeGrabbed].
	CurrentKeyMaster fluidBind: myKeyMaster during:
	[CurrentAuthor fluidBind: myAuthor during:
		[myBeWork revise: newEdition]]!
*/
}
/**
 * Essential.  Return the club which has permission to revise this Work.  Blasts if noone can
 * (i.e. editor has been removed).
 */
public ID editClub() {
	if (myBeWork.fetchEditClub() == null) {
		throw new AboraRuntimeException(AboraRuntimeException.EDITOR_REMOVED);
	}
	return myBeWork.fetchEditClub();
/*
udanax-top.st:22231:FeWork methodsFor: 'permissions'!
{ID CLIENT} editClub
	"Essential.  Return the club which has permission to revise this Work.  Blasts if noone can (i.e. editor has been removed)."
	
	myBeWork fetchEditClub == NULL ifTrue: [Heaper BLAST: #EditorRemoved].
	^myBeWork fetchEditClub!
*/
}
/**
 * Essential. Return the club which will be recorded as the initial club for frozen Works in
 * the history trail.  Blasts if there is no trail being generated.
 */
public ID historyClub() {
	ID result;
	result = myBeWork.fetchHistoryClub();
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NO_HISTORY_CLUB);
	}
	return result;
/*
udanax-top.st:22237:FeWork methodsFor: 'permissions'!
{ID CLIENT} historyClub
	"Essential. Return the club which will be recorded as the initial club for frozen Works in the history trail.  Blasts if there is no trail being generated."
	
	| result {ID} |
	result := myBeWork fetchHistoryClub.
	result == NULL ifTrue:
		[Heaper BLAST: #NoHistoryClub].
	^result!
*/
}
/**
 * Essential.  Return the club which has permission to read this Work.  Blasts if the read
 * Club has been removed (in that case, only those who have edit permission can read the
 * Work).
 */
public ID readClub() {
	if (myBeWork.fetchReadClub() == null) {
		throw new AboraRuntimeException(AboraRuntimeException.READ_CLUB_REMOVED);
	}
	return myBeWork.fetchReadClub();
/*
udanax-top.st:22246:FeWork methodsFor: 'permissions'!
{ID CLIENT} readClub
	"Essential.  Return the club which has permission to read this Work.  Blasts if the read Club has been removed (in that case, only those who have edit permission can read the Work)."
	
	myBeWork fetchReadClub == NULL ifTrue: [Heaper BLAST: #ReadClubRemoved].
	^myBeWork fetchReadClub!
*/
}
/**
 * Essential.  Irrevocably remove edit permission. Requires ownership authority.
 */
public void removeEditClub() {
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	myBeWork.setEditClub(null);
/*
udanax-top.st:22252:FeWork methodsFor: 'permissions'!
{void CLIENT} removeEditClub
	"Essential.  Irrevocably remove edit permission. Requires ownership authority."
	
	(CurrentKeyMaster fluidGet hasAuthority: self owner) 
		ifFalse: [Heaper BLAST: #MustBeOwner].
	myBeWork setEditClub: NULL!
*/
}
/**
 * Essential.  Irrevocably remove read permission (although you should note that editors are
 * still able to read, if there are any). Requires ownership authority.
 */
public void removeReadClub() {
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	myBeWork.setReadClub(null);
/*
udanax-top.st:22259:FeWork methodsFor: 'permissions'!
{void CLIENT} removeReadClub
	"Essential.  Irrevocably remove read permission (although you should note that editors are still able to read, if there are any). Requires ownership authority."
	
	(CurrentKeyMaster fluidGet hasAuthority: self owner) 
		ifFalse: [Heaper BLAST: #MustBeOwner].
	myBeWork setReadClub: NULL!
*/
}
/**
 * Essential.  Change who has edit permission. Requires ownership authority.
 * Aborts if the Work doesn't have an edit Club.
 */
public void setEditClub(ID club) {
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	if (myBeWork.fetchEditClub() == null) {
		throw new AboraRuntimeException(AboraRuntimeException.EDIT_CLUB_IRREVOCABLY_REMOVED);
	}
	myBeWork.setEditClub(club);
/*
udanax-top.st:22266:FeWork methodsFor: 'permissions'!
{void CLIENT} setEditClub: club {ID | NULL}
	"Essential.  Change who has edit permission. Requires ownership authority.
	 Aborts if the Work doesn't have an edit Club."
	
	(CurrentKeyMaster fluidGet hasAuthority: self owner) 
		ifFalse: [Heaper BLAST: #MustBeOwner].
	myBeWork fetchEditClub == NULL
		ifTrue: [Heaper BLAST: #EditClubIrrevocablyRemoved].
	myBeWork setEditClub: club!
*/
}
/**
 * Essential.  Change the initial read Club for frozen Works in the trail. Requires ownership
 * authority. Setting it to NULL turns off the recording of history.
 */
public void setHistoryClub(ID club) {
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	myBeWork.setHistoryClub(club);
/*
udanax-top.st:22276:FeWork methodsFor: 'permissions'!
{void CLIENT} setHistoryClub: club {ID | NULL}
	"Essential.  Change the initial read Club for frozen Works in the trail. Requires ownership authority. Setting it to NULL turns off the recording of history."
	
	(CurrentKeyMaster fluidGet hasAuthority: self owner) 
		ifFalse: [Heaper BLAST: #MustBeOwner].
	myBeWork setHistoryClub: club!
*/
}
/**
 * Essential.  Change who has read permission. Requires ownership authority.
 * Aborts if the works doesn't have a read Club.
 */
public void setReadClub(ID club) {
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	if (myBeWork.fetchReadClub() == null) {
		throw new AboraRuntimeException(AboraRuntimeException.READ_CLUB_IRREVOCABLY_REMOVED);
	}
	myBeWork.setReadClub(club);
/*
udanax-top.st:22283:FeWork methodsFor: 'permissions'!
{void CLIENT} setReadClub: club {ID | NULL}
	"Essential.  Change who has read permission. Requires ownership authority.
	 Aborts if the works doesn't have a read Club."
	
	(CurrentKeyMaster fluidGet hasAuthority: self owner) 
		ifFalse: [Heaper BLAST: #MustBeOwner].
	myBeWork fetchReadClub == NULL
		ifTrue: [Heaper BLAST: #ReadClubIrrevocablyRemoved].
	myBeWork setReadClub: club!
*/
}
/**
 * Essential.  Adds to the endorsements on this Work. The set of endorsements must be a
 * finite number of (club ID, token ID) pairs. This requires the signature authority of all
 * of the Clubs used to endorse; will blast and do nothing if any of the required authority
 * is lacking. The token IDs must not be named IDs.
 */
public void endorse(CrossRegion additionalEndorsements) {
	FeRangeElement.validateEndorsement(additionalEndorsements, ((FeKeyMaster) CurrentKeyMaster.fluidGet()));
	myBeWork.endorse(additionalEndorsements);
/*
udanax-top.st:22295:FeWork methodsFor: 'endorsing'!
{void CLIENT} endorse: additionalEndorsements {CrossRegion}
	"Essential.  Adds to the endorsements on this Work. The set of endorsements must be a finite number of (club ID, token ID) pairs. This requires the signature authority of all of the Clubs used to endorse; will blast and do nothing if any of the required authority is lacking. The token IDs must not be named IDs."
	
	FeRangeElement validateEndorsement: additionalEndorsements with: CurrentKeyMaster fluidGet.
	myBeWork endorse: additionalEndorsements!
*/
}
/**
 * Essential.  Return all of the endorsements which have been placed on this Work and are not
 * currently retracted.
 * (Endorsements are used to filter various operations which return sets of Works.  See
 * FeEdition::rangeTranscluders() for one way to find this work by filtering for its
 * endorsements.)
 */
public CrossRegion endorsements() {
	return myBeWork.endorsements();
/*
udanax-top.st:22301:FeWork methodsFor: 'endorsing'!
{CrossRegion CLIENT} endorsements
	"Essential.  Return all of the endorsements which have been placed on this Work and are not currently retracted.
	(Endorsements are used to filter various operations which return sets of Works.  See FeEdition::rangeTranscluders() for one way to find this work by filtering for its endorsements.)"
	
	^myBeWork endorsements!
*/
}
/**
 * Essential.  Removes endorsements from this Work. This requires the signature authority of
 * all of the Clubs whose endorsements are in the list; will blast and do nothing if any of
 * the required authority is lacking. Ignores all endorsements which you could have removed,
 * but which don't happen to be there right now.
 */
public void retract(CrossRegion removedEndorsements) {
	FeRangeElement.validateEndorsement(removedEndorsements, ((FeKeyMaster) CurrentKeyMaster.fluidGet()));
	myBeWork.retract(removedEndorsements);
/*
udanax-top.st:22307:FeWork methodsFor: 'endorsing'!
{void CLIENT} retract: removedEndorsements {CrossRegion}
	"Essential.  Removes endorsements from this Work. This requires the signature authority of all of the Clubs whose endorsements are in the list; will blast and do nothing if any of the required authority is lacking. Ignores all endorsements which you could have removed, but which don't happen to be there right now."
	
	FeRangeElement validateEndorsement: removedEndorsements with: CurrentKeyMaster fluidGet.
	myBeWork retract: removedEndorsements!
*/
}
/**
 * Essential.  Add to the list of sponsors of this Work. Requires signature authority of all
 * of the Clubs in the set.
 */
public void sponsor(IDRegion clubs) {
	FeRangeElement.validateSignature(clubs, ((FeKeyMaster) CurrentKeyMaster.fluidGet()));
	myBeWork.sponsor(clubs);
/*
udanax-top.st:22315:FeWork methodsFor: 'sponsoring'!
{void CLIENT} sponsor: clubs {IDRegion}
	"Essential.  Add to the list of sponsors of this Work. Requires signature authority of all of the Clubs in the set."
	
	FeRangeElement validateSignature: clubs with: CurrentKeyMaster fluidGet.
	myBeWork sponsor: clubs!
*/
}
/**
 * Essential.  All of the Clubs which are sponsoring this Work to keep it from being
 * discarded.
 * What sort of permissions does this require?
 */
public IDRegion sponsors() {
	return myBeWork.sponsors();
/*
udanax-top.st:22321:FeWork methodsFor: 'sponsoring'!
{IDRegion CLIENT} sponsors
	"Essential.  All of the Clubs which are sponsoring this Work to keep it from being discarded.
	What sort of permissions does this require?"
	
	^myBeWork sponsors!
*/
}
/**
 * Essential.  End sponsorship of this Work by all of the listed Clubs. Requires signature
 * authority of all of the Clubs in the set, even if they are not currently sponsors.
 * Should this use the CurrentKeyMaster? Or the internal KeyMaster if it is grabbed?
 */
public void unsponsor(IDRegion clubs) {
	FeRangeElement.validateSignature(clubs, ((FeKeyMaster) CurrentKeyMaster.fluidGet()));
	myBeWork.unsponsor(clubs);
/*
udanax-top.st:22327:FeWork methodsFor: 'sponsoring'!
{void CLIENT} unsponsor: clubs {IDRegion}
	"Essential.  End sponsorship of this Work by all of the listed Clubs. Requires signature authority of all of the Clubs in the set, even if they are not currently sponsors.
	Should this use the CurrentKeyMaster? Or the internal KeyMaster if it is grabbed?"
	
	FeRangeElement validateSignature: clubs with: CurrentKeyMaster fluidGet.
	myBeWork unsponsor: clubs!
*/
}
/**
 * The authority of my KeyMaster has changed and I need to update my status
 */
public void updateStatus() {
	/* If I was grabbing and lost permission to edit, or signature authority for the author,
		evict myself
	else if I was waiting for a grab and gained permission to do so
		and the Work is ungrabbed
			grab it */
	Ravi.knownBug();
	/* Add mechanism to notify when signature Club of Author is changed */
	if (canRevise()) {
		if ( ! ((myBeWork.canBeEditedBy(myKeyMaster)) && (myKeyMaster.hasSignatureAuthority(myAuthor)))) {
			release();
		}
	}
	else {
		if (amWaiting && (myKeyMaster != null && ((myBeWork.canBeEditedBy(myKeyMaster)) && (myKeyMaster.hasSignatureAuthority(myAuthor))))) {
			if (myBeWork.tryLock(this)) {
				amWaiting = false;
				if (myStatusDetectors != null) {
					Stepper stomper = myStatusDetectors.stepper();
					for (; stomper.hasValue(); stomper.step()) {
						FeStatusDetector stat = (FeStatusDetector) stomper.fetch();
						if (stat == null) {
							continue ;
						}
						Someone.thingToDo();
						/* reasons */
						stat.grabbed(this, myAuthor, 0);
					}
					stomper.destroy();
				}
			}
		}
	}
/*
udanax-top.st:22336:FeWork methodsFor: 'server grab status'!
{void} updateStatus
	"The authority of my KeyMaster has changed and I need to update my status"
	
	"If I was grabbing and lost permission to edit, or signature authority for the author,
		evict myself
	else if I was waiting for a grab and gained permission to do so
		and the Work is ungrabbed
			grab it"
	Ravi knownBug. "Add mechanism to notify when signature Club of Author is changed"
	self canRevise ifTrue:
		[((myBeWork canBeEditedBy: myKeyMaster)
				and: [myKeyMaster hasSignatureAuthority: myAuthor])
			ifFalse: [self release]]
	ifFalse: [(amWaiting
			and: [myKeyMaster ~~ NULL
			and: [(myBeWork canBeEditedBy: myKeyMaster)
			and: [myKeyMaster hasSignatureAuthority: myAuthor]]])
		ifTrue:
			[(myBeWork tryLock: self) ifTrue:
				[amWaiting := false.
				myStatusDetectors ~~ NULL ifTrue:
					[myStatusDetectors stepper forEach:
						[ :stat {FeStatusDetector} |
						self thingToDo. "reasons"
						stat grabbed: self with: myAuthor with: IntegerVarZero]]]]]!
*/
}
/**
 * Trigger all my immediate RevisionDetectors who can read the Work
 */
public void triggerRevisionDetectors(FeEdition contents, ID author, int time, int sequence) {
	Stepper stomper = myRevisionDetectors.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Pair pair = (Pair) stomper.fetch();
		if (pair == null) {
			continue ;
		}
		if (myBeWork.canBeReadBy(((FeKeyMaster) pair.left()))) {
			((FeRevisionDetector) pair.right()).revised(this, contents, author, time, sequence);
		}
	}
	stomper.destroy();
/*
udanax-top.st:22364:FeWork methodsFor: 'server contents'!
{void} triggerRevisionDetectors: contents {FeEdition}
	with: author {ID}
	with: time {IntegerVar}
	with: sequence {IntegerVar}
	"Trigger all my immediate RevisionDetectors who can read the Work"
	
	myRevisionDetectors stepper forEach:
		[ :pair {Pair of: FeKeyMaster and: FeRevisionDetector} |
		(myBeWork canBeReadBy: (pair left cast: FeKeyMaster)) ifTrue:
			[(pair right cast: FeRevisionDetector)
				revised: self with: contents with: author with: time with: sequence]]!
*/
}
public ID fetchAuthor() {
	return myAuthor;
/*
udanax-top.st:22378:FeWork methodsFor: 'server accessing'!
{ID | NULL} fetchAuthor
	^myAuthor!
*/
}
public BeRangeElement fetchBe() {
	return myBeWork;
/*
udanax-top.st:22382:FeWork methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	^myBeWork!
*/
}
public ID getAuthor() {
	if (myAuthor == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NO_AUTHOR);
	}
	return myAuthor;
/*
udanax-top.st:22386:FeWork methodsFor: 'server accessing'!
{ID} getAuthor
	myAuthor == NULL ifTrue:
		[Heaper BLAST: #NoAuthor].
	^myAuthor!
*/
}
public BeRangeElement getOrMakeBe() {
	return myBeWork;
/*
udanax-top.st:22392:FeWork methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	
	^myBeWork!
*/
}
public FeWork(BeWork be) {
	super();
	myBeWork = be;
	myKeyMaster = null;
	myAuthor = null;
	amWaiting = false;
	myStatusDetectors = null;
	myRevisionDetectors = null;
	myKeyMaster = null;
/*
udanax-top.st:22398:FeWork methodsFor: 'protected: create'!
create: be {BeWork}
	super create.
	myBeWork := be.
	myKeyMaster := NULL.
	myAuthor _ NULL.
	amWaiting := false.
	myStatusDetectors := NULL.
	myRevisionDetectors := NULL.
	myKeyMaster := NULL.!
*/
}
public void destruct() {
	myBeWork.removeFeRangeElement(this);
	myBeWork.tryUnlock(this);
	if (myKeyMaster != null) {
		myKeyMaster.unregisterWork(this);
	}
	super.destruct();
/*
udanax-top.st:22411:FeWork methodsFor: 'destruct'!
{void} destruct
	myBeWork removeFeRangeElement: self.
	myBeWork tryUnlock: self.
	myKeyMaster ~~ NULL ifTrue:
		[myKeyMaster unregisterWork: self].
	super destruct.!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("Work(");
	oo.print("ids: ");
	oo.print((FeServer.iDsOf(this)));
	if (canRead()) {
		oo.print(" contents: ");
		oo.print(edition());
	}
	if (canRevise()) {
		oo.print(" (grabbed)");
	}
	oo.print(")");
/*
udanax-top.st:22421:FeWork methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << 'Work('
		<< 'ids: ' << (FeServer iDsOf: self).
	self canRead ifTrue:
		[oo << ' contents: ' << self edition].
	self canRevise ifTrue:
		[oo << ' (grabbed)'].
	oo << ')'!
*/
}
public FeRangeElement again() {
	Someone.thingToDo();
	/* deal with work consolidation */
	return this;
/*
udanax-top.st:22433:FeWork methodsFor: 'accessing'!
{FeRangeElement} again
	
	self thingToDo. "deal with work consolidation"
	^self!
*/
}
public boolean canMakeIdentical(FeRangeElement newIdentity) {
	if ( ! (isIdentical(newIdentity))) {
		throw new UnimplementedException();
	}
	return true;
/*
udanax-top.st:22438:FeWork methodsFor: 'accessing'!
{BooleanVar} canMakeIdentical: newIdentity {FeRangeElement}
	(self isIdentical: newIdentity) ifFalse:
		[self unimplemented].
	^true!
*/
}
public void makeIdentical(FeRangeElement newIdentity) {
	throw new UnimplementedException();
/*
udanax-top.st:22444:FeWork methodsFor: 'accessing'!
{void} makeIdentical: newIdentity {FeRangeElement} 
	
	self unimplemented. "deal with work consolidation"!
*/
}
/**
 * @deprecated
 */
public void addSponsors(IDRegion clubs) {
	throw new PasseException();
/*
udanax-top.st:22450:FeWork methodsFor: 'smalltalk: passe'!
{void} addSponsors: clubs {IDRegion}
	self passe "sponsor"!
*/
}
/**
 * @deprecated
 */
public ID currentAuthor() {
	throw new PasseException();
/*
udanax-top.st:22454:FeWork methodsFor: 'smalltalk: passe'!
{ID} currentAuthor
	self passe "grabber"!
*/
}
/**
 * @deprecated
 */
public void lock() {
	throw new PasseException();
/*
udanax-top.st:22458:FeWork methodsFor: 'smalltalk: passe'!
{void} lock
	self passe!
*/
}
/**
 * @deprecated
 */
public ID lockingClub() {
	throw new PasseException();
/*
udanax-top.st:22462:FeWork methodsFor: 'smalltalk: passe'!
{ID} lockingClub
	
	self passe.!
*/
}
/**
 * @deprecated
 */
public void removeSponsors(IDRegion clubs) {
	throw new PasseException();
/*
udanax-top.st:22466:FeWork methodsFor: 'smalltalk: passe'!
{void} removeSponsors: clubs {IDRegion}
	self passe!
*/
}
/**
 * Essential.  Registers a request so that the next time this Work would have been unlocked
 * and the KeyMaster has edit permission, it will be locked by this client. If this client
 * already has it locked, then it has no effect. To find out when this happens, place Status
 * Detectors on the Work.
 * @deprecated
 */
public void requestLock() {
	throw new PasseException();
/*
udanax-top.st:22470:FeWork methodsFor: 'smalltalk: passe'!
{void} requestLock
	"Essential.  Registers a request so that the next time this Work would have been unlocked and the KeyMaster has edit permission, it will be locked by this client. If this client already has it locked, then it has no effect. To find out when this happens, place Status Detectors on the Work."
	
	self passe.
	amWaiting := true.
	self updateStatus.!
*/
}
/**
 * Essential.  Change the authority through which the Work is being read and revised. Blasts
 * if the Work is locked and the new authority is insufficient to maintain the lock.
 * @deprecated
 */
public void setKeyMaster(FeKeyMaster km) {
	throw new PasseException();
/*
udanax-top.st:22477:FeWork methodsFor: 'smalltalk: passe'!
{void} setKeyMaster: km {FeKeyMaster | NULL}
	"Essential.  Change the authority through which the Work is being read and revised. Blasts if the Work is locked and the new authority is insufficient to maintain the lock."
	
	self passe.  "Subsumed by grab"
	"Check that the new authority can maintain existing lock"
	(self canRevise and: [km == NULL or: [(myBeWork canBeEditedBy: km) not]])
		ifTrue: [Heaper BLAST: #MustHaveEditPermission].
	self knownBug.  "check the CurrentAuthor."
	"Change the km and check for change in read permission"
	myAuthor _ CurrentAuthor fluidGet.
	myKeyMaster ~~ NULL ifTrue: [myKeyMaster unregisterWork: self].
	myKeyMaster := km.
	myKeyMaster ~~ NULL ifTrue: [myKeyMaster registerWork: self].
	"Update Detectors and cached information"
	self updateStatus!
*/
}
/**
 * @deprecated
 */
public void unendorse(CrossRegion removedEndorsements) {
	throw new PasseException();
/*
udanax-top.st:22493:FeWork methodsFor: 'smalltalk: passe'!
{void} unendorse: removedEndorsements {CrossRegion}
	self passe "retract"!
*/
}
/**
 * Essential.  Release the lock on this Work. Does nothing if it is already unlocked.
 * @deprecated
 */
public void unlock() {
	throw new PasseException();
/*
udanax-top.st:22497:FeWork methodsFor: 'smalltalk: passe'!
{void} unlock
	"Essential.  Release the lock on this Work. Does nothing if it is already unlocked."
	
	self passe.
	(myBeWork tryUnlock: self) ifTrue:
		["Notify all the status detectors"
		myStatusDetectors ~~ NULL ifTrue:
			[myStatusDetectors stepper forEach: [ :stat {FeStatusDetector} |
				stat canRevise: self with: false]]]!
*/
}
/**
 * Essential. Trigger a Detector whenever there is a revision to the Work which the
 * CurrentKeyMaster can see. If this detector has already been added, then the old KeyMaster
 * associated with it is replaced with the CurrentKeyMaster.
 * See RevisionDetector::revised (Edition * contents,
 * ID * author,
 * IntegerVar sequence,
 * IntegerVar time).
 */
public void addRevisionDetector(FeRevisionDetector detector) {
	if (myRevisionDetectors == null) {
		myRevisionDetectors = PrimSet.weak(7, (RevisionDetectorExecutor.make(this)));
		myBeWork.addRevisionWatcher(this);
	}
	else {
		Stepper stomper = myRevisionDetectors.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			Pair pair = (Pair) stomper.fetch();
			if (pair == null) {
				continue ;
			}
			if (detector.isEqual(pair.right())) {
				myRevisionDetectors.remove(pair);
			}
		}
		stomper.destroy();
	}
	myRevisionDetectors.introduce((Pair.make(((FeKeyMaster) CurrentKeyMaster.fluidGet()), detector)));
/*
udanax-top.st:22509:FeWork methodsFor: 'history'!
{void} addRevisionDetector: detector {FeRevisionDetector}
	"Essential. Trigger a Detector whenever there is a revision to the Work which the CurrentKeyMaster can see. If this detector has already been added, then the old KeyMaster associated with it is replaced with the CurrentKeyMaster.
	See RevisionDetector::revised (Edition * contents,
		ID * author,
		IntegerVar sequence,
		IntegerVar time)."
	
	myRevisionDetectors == NULL ifTrue:
		[myRevisionDetectors := PrimSet weak: 7 with: (RevisionDetectorExecutor make: self).
		myBeWork addRevisionWatcher: self]
	ifFalse:
		[myRevisionDetectors stepper forEach: [ :pair {Pair} |
			(detector isEqual: pair right) ifTrue:
				[myRevisionDetectors remove: pair]]].
	myRevisionDetectors introduce: (Pair make: CurrentKeyMaster fluidGet with: detector)!
*/
}
/**
 * The ID of the author of the last revision of this Work to its current Edition, or its
 * creation if it hasn't been revised since. The Work must be grabbed, or the
 * CurrentKeyMaster must be able to exercise the authority of the Read, Edit, or History
 * Club.
 */
public ID lastRevisionAuthor() {
	if ( ! (canReadHistory())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_READ_PERMISSION);
	}
	return myBeWork.lastRevisionAuthor();
/*
udanax-top.st:22525:FeWork methodsFor: 'history'!
{ID CLIENT} lastRevisionAuthor
	"The ID of the author of the last revision of this Work to its current Edition, or its creation if it hasn't been revised since. The Work must be grabbed, or the CurrentKeyMaster must be able to exercise the authority of the Read, Edit, or History Club."
	
	self canReadHistory ifFalse:
		[Heaper BLAST: #MustHaveReadPermission].
	^myBeWork lastRevisionAuthor!
*/
}
/**
 * The sequence number of the last revision of this Work to its current Edition, or its
 * creation if it hasn't been revised since. The Work must be grabbed, or the
 * CurrentKeyMaster must be able to exercise the authority of the Read, Edit, or History
 * Club.
 */
public int lastRevisionNumber() {
	if ( ! (canReadHistory())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_READ_PERMISSION);
	}
	return myBeWork.lastRevisionNumber();
/*
udanax-top.st:22532:FeWork methodsFor: 'history'!
{IntegerVar CLIENT} lastRevisionNumber
	"The sequence number of the last revision of this Work to its current Edition, or its creation if it hasn't been revised since. The Work must be grabbed, or the CurrentKeyMaster must be able to exercise the authority of the Read, Edit, or History Club."
	
	self canReadHistory ifFalse:
		[Heaper BLAST: #MustHaveReadPermission].
	^myBeWork lastRevisionNumber!
*/
}
/**
 * The time of the last revision of this Work to its current Edition, or its creation if it
 * hasn't been revised since. The Work must be grabbed, or the CurrentKeyMaster must be able
 * to exercise the authority of the Read, Edit, or History Club.
 */
public int lastRevisionTime() {
	if ( ! (canReadHistory())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_READ_PERMISSION);
	}
	return myBeWork.lastRevisionTime();
/*
udanax-top.st:22539:FeWork methodsFor: 'history'!
{IntegerVar CLIENT} lastRevisionTime
	"The time of the last revision of this Work to its current Edition, or its creation if it hasn't been revised since. The Work must be grabbed, or the CurrentKeyMaster must be able to exercise the authority of the Read, Edit, or History Club."
	
	self canReadHistory ifFalse:
		[Heaper BLAST: #MustHaveReadPermission].
	^myBeWork lastRevisionTime!
*/
}
/**
 * Essential. Inform the work that its last revision detector has gone away.
 */
public void removeLastRevisionDetector() {
	myRevisionDetectors = null;
	myBeWork.removeRevisionWatcher(this);
/*
udanax-top.st:22546:FeWork methodsFor: 'history'!
{void} removeLastRevisionDetector
	"Essential. Inform the work that its last revision detector has gone away."
	
	myRevisionDetectors := NULL.
	myBeWork removeRevisionWatcher: self!
*/
}
/**
 * Essential. Return a detector tht will trigger whenever there is a revision to the Work
 * which the CurrentKeyMaster can see.
 * See RevisionDetector::revised (Edition * contents,
 * ID * author,
 * IntegerVar sequence,
 * IntegerVar time).
 */
public FeRevisionDetector revisionDetector() {
	Dean.shouldImplement();
	addRevisionDetector(null);
	return null;
/*
udanax-top.st:22552:FeWork methodsFor: 'history'!
{FeRevisionDetector CLIENT} revisionDetector
	"Essential. Return a detector tht will trigger whenever there is a revision to the Work which the CurrentKeyMaster can see.
	See RevisionDetector::revised (Edition * contents,
		ID * author,
		IntegerVar sequence,
		IntegerVar time)."
	
	Dean shouldImplement.
	self addRevisionDetector: NULL.
	^NULL "fodder"!
*/
}
/**
 * Return the revision trail of the receiver.  The trail will be empty if no revisions have
 * been recorded. The trail is updated immediately when the Work is revised.
 * In order to get the trail, either the Work must be grabbed, or you must be a member of the
 * Read, Edit, or History Clubs.
 */
public FeEdition revisions() {
	Someone.knownBug();
	/* This needs a label. */
	if ( ! (canReadHistory())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_READ_PERMISSION);
	}
	return FeEdition.on(myBeWork.revisions());
/*
udanax-top.st:22563:FeWork methodsFor: 'history'!
{FeEdition CLIENT} revisions
	"Return the revision trail of the receiver.  The trail will be empty if no revisions have been recorded. The trail is updated immediately when the Work is revised.
	In order to get the trail, either the Work must be grabbed, or you must be a member of the Read, Edit, or History Clubs."
	
	self knownBug.  "This needs a label."
	self canReadHistory ifFalse:
		[Heaper BLAST: #MustHaveReadPermission].
	^FeEdition on: myBeWork revisions!
*/
}
/**
 * self canRead or CurrentKeyMaster has authority of the historyClub
 */
public boolean canReadHistory() {
	FeKeyMaster ckm;
	ckm = ((FeKeyMaster) CurrentKeyMaster.fluidFetch());
	return canRead() || (ckm != null && (myBeWork.fetchHistoryClub() != null && (ckm.hasAuthority(myBeWork.fetchHistoryClub()))));
/*
udanax-top.st:22574:FeWork methodsFor: 'private:'!
{BooleanVar} canReadHistory
	"self canRead or CurrentKeyMaster has authority of the historyClub"
	| ckm {FeKeyMaster} |
	ckm := CurrentKeyMaster fluidFetch.
	^self canRead
		or: [ckm ~~ NULL
		and: [myBeWork fetchHistoryClub ~~ NULL
		and: [ckm hasAuthority: myBeWork fetchHistoryClub]]]!
*/
}
public static void bombReleaseWork(FeWork CHARGE) {
	(CHARGE).release();
/*
udanax-top.st:22592:FeWork class methodsFor: 'exceptions: exceptions'!
bomb.ReleaseWork: CHARGE {FeWork wimpy}
	^[(CHARGE quickCast: FeWork) release]!
*/
}
/**
 * Essential.  Create a new Work whose initial contents are the given Edition. The reader,
 * editor, owner, sponsor, and KeyMaster come from the fluid environment. If the KeyMaster
 * has edit permission, then the Work is initially grabbed by it.
 * Note: This does not assign it a global ID; that must be done separately (see
 * Server::assignID).
 */
public static FeWork make(FeEdition contents) {
	FeKeyMaster.assertSponsorship();
	FeKeyMaster.assertSignatureAuthority();
	return (((BeGrandMap) CurrentGrandMap.fluidGet()).newWork(contents)).makeLockedFeWork();
/*
udanax-top.st:22597:FeWork class methodsFor: 'creation'!
{FeWork CLIENT} make: contents {FeEdition}
	"Essential.  Create a new Work whose initial contents are the given Edition. The reader, editor, owner, sponsor, and KeyMaster come from the fluid environment. If the KeyMaster has edit permission, then the Work is initially grabbed by it.
	Note: This does not assign it a global ID; that must be done separately (see Server::assignID)."
	
	FeKeyMaster assertSponsorship.
	FeKeyMaster assertSignatureAuthority.
	^(CurrentGrandMap fluidGet newWork: contents) makeLockedFeWork!
*/
}
public static FeWork on(BeWork be) {
	FeWork result;
	result = new FeWork(be);
	be.addFeRangeElement(result);
	return result;
/*
udanax-top.st:22605:FeWork class methodsFor: 'creation'!
{FeWork} on: be {BeWork}
	| result {FeWork} |
	result := self create: be.
	be addFeRangeElement: result.
	^result!
*/
}
/**
 * {void CLIENT} addRevisionDetector: detector {PrRevisionDetector}
 * {void CLIENT} addStatusDetector: detector {PrStatusDetector}
 * {BooleanVar CLIENT} canRead
 * {BooleanVar CLIENT} canRevise
 * {ID CLIENT} editClub
 * {FeEdition CLIENT} edition
 * {void CLIENT} endorse: added {CrossRegion}
 * {CrossRegion CLIENT} endorsements
 * {void CLIENT} grab
 * {ID CLIENT} grabber
 * {ID CLIENT} historyClub
 * {ID CLIENT} lastRevisionAuthor
 * {IntegerVar CLIENT} lastRevisionNumber
 * {IntegerVar CLIENT} lastRevisionTime
 * {ID CLIENT} readClub
 * {void CLIENT} release
 * {void CLIENT} removeEditClub
 * {void CLIENT} removeReadClub
 * {void CLIENT} removeRevisionDetector: detector {PrRevisionDetector}
 * {void CLIENT} removeStatusDetector: detector {PrStatusDetector}
 * {void CLIENT} requestGrab
 * {void CLIENT} retract: removed {CrossRegion}
 * {void CLIENT} revise: newEdition {FeEdition}
 * {FeEdition CLIENT} revisions
 * {void CLIENT} setEditClub: club {ID}
 * {void CLIENT} setHistoryClub: club {ID | NULL}
 * {void CLIENT} setReadClub: club {ID}
 * {void CLIENT} sponsor: clubs {IDRegion}
 * {IDRegion CLIENT} sponsors
 * {void CLIENT} unsponsor: clubs {IDRegion}
 */
public static void infostProtocol() {
/*
udanax-top.st:22614:FeWork class methodsFor: 'smalltalk: system'!
info.stProtocol
"{void CLIENT} addRevisionDetector: detector {PrRevisionDetector}
{void CLIENT} addStatusDetector: detector {PrStatusDetector}
{BooleanVar CLIENT} canRead
{BooleanVar CLIENT} canRevise
{ID CLIENT} editClub
{FeEdition CLIENT} edition
{void CLIENT} endorse: added {CrossRegion}
{CrossRegion CLIENT} endorsements
{void CLIENT} grab
{ID CLIENT} grabber
{ID CLIENT} historyClub
{ID CLIENT} lastRevisionAuthor
{IntegerVar CLIENT} lastRevisionNumber
{IntegerVar CLIENT} lastRevisionTime
{ID CLIENT} readClub
{void CLIENT} release
{void CLIENT} removeEditClub
{void CLIENT} removeReadClub
{void CLIENT} removeRevisionDetector: detector {PrRevisionDetector}
{void CLIENT} removeStatusDetector: detector {PrStatusDetector}
{void CLIENT} requestGrab
{void CLIENT} retract: removed {CrossRegion}
{void CLIENT} revise: newEdition {FeEdition}
{FeEdition CLIENT} revisions
{void CLIENT} setEditClub: club {ID}
{void CLIENT} setHistoryClub: club {ID | NULL}
{void CLIENT} setReadClub: club {ID}
{void CLIENT} sponsor: clubs {IDRegion}
{IDRegion CLIENT} sponsors
{void CLIENT} unsponsor: clubs {IDRegion}
"!
*/
}
public FeWork() {
/*

Generated during transformation
*/
}
public FeWork(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
