/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.snarf.FlockLocation;
import info.dgjones.abora.gold.xcvr.Rcvr;
import java.io.PrintWriter;

/**
 * Contains all the information the packer needs to know about the flock on disk (except
 * forwarder stuff).  The packer knows about forwarders by having several FlockInfo objects
 * for the same flock.  We should consider having a separate class for forward information
 * that does not contain the flags and the oldSize.
 * myOldSize - this is the size of the flock on disk as of the last commit.  If this is zero,
 * it is uninitialized.  This is used to refitting without bringing in the snarf for this
 * flock.
 * myFlags - keeps track of whether the receive is a new flock (isn''t on disk yet), is
 * forgotten, is in the process is fchanging its forggten state (isChanging), and is Update
 * (contents have changed).
 */
public class FlockInfo extends FlockLocation {

	protected int myFlockHash;
	protected int myToken;
	protected int myFlags;
	protected int myOldSize;
	protected static PrimPtrTable TheFlockCategoryTable;
/*
udanax-top.st:26536:
FlockLocation subclass: #FlockInfo
	instanceVariableNames: '
		myFlockHash {UInt4}
		myToken {Int32}
		myFlags {UInt32}
		myOldSize {Int32}'
	classVariableNames: 'TheFlockCategoryTable {PrimPtrTable smalltalk} '
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:26544:
FlockInfo comment:
'Contains all the information the packer needs to know about the flock on disk (except forwarder stuff).  The packer knows about forwarders by having several FlockInfo objects for the same flock.  We should consider having a separate class for forward information that does not contain the flags and the oldSize.
myOldSize - this is the size of the flock on disk as of the last commit.  If this is zero, it is uninitialized.  This is used to refitting without bringing in the snarf for this flock.
myFlags - keeps track of whether the receive is a new flock (isn''t on disk yet), is forgotten, is in the process is fchanging its forggten state (isChanging), and is Update (contents have changed).'!
*/
/*
udanax-top.st:26550:
(FlockInfo getOrMakeCxxClassDescription)
	friends:
'	friend UInt4  contentsDirty ();
	friend UInt4  forgottenMask ();
	friend UInt4  forgottenStateDirty ();
	friend UInt4  isNewMask ();
';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:26742:
FlockInfo class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:26745:
(FlockInfo getOrMakeCxxClassDescription)
	friends:
'	friend UInt4  contentsDirty ();
	friend UInt4  forgottenMask ();
	friend UInt4  forgottenStateDirty ();
	friend UInt4  isNewMask ();
';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FlockInfo.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return true if my shepherd has changed and informed the SnarfPacker.
 */
public boolean isContentsDirty() {
	return (myFlags & FlockInfo.contentsDirty()) != 0;
/*
udanax-top.st:26561:FlockInfo methodsFor: 'testing'!
{BooleanVar} isContentsDirty
	"Return true if my shepherd has changed and informed the SnarfPacker."
	
	^(myFlags bitAnd: FlockInfo contentsDirty) ~~ UInt32Zero!
*/
}
/**
 * Return true if our shepherd has received destroy
 */
public boolean isDestroyed() {
	return (myFlags & FlockInfo.destroyed()) != 0;
/*
udanax-top.st:26566:FlockInfo methodsFor: 'testing'!
{BooleanVar} isDestroyed
	"Return true if our shepherd has received destroy"
	
	^(myFlags bitAnd: FlockInfo destroyed) ~~ UInt32Zero.!
*/
}
/**
 * Return true if anything about my flock is changing (including if the flock is new).
 */
public boolean isDirty() {
	return (myFlags & ((FlockInfo.isNewMask() | FlockInfo.contentsDirty()) | FlockInfo.forgottenStateDirty())) != 0;
/*
udanax-top.st:26571:FlockInfo methodsFor: 'testing'!
{BooleanVar} isDirty
	"Return true if anything about my flock is changing (including if the flock is new)."
	
	^(myFlags bitAnd: ((FlockInfo isNewMask bitOr: FlockInfo contentsDirty) bitOr: FlockInfo forgottenStateDirty)) ~~ UInt32Zero!
*/
}
/**
 * Return true if our shepherd has been dismantled
 */
public boolean isDismantled() {
	return (myFlags & FlockInfo.dismantled()) != 0;
/*
udanax-top.st:26576:FlockInfo methodsFor: 'testing'!
{BooleanVar} isDismantled
	"Return true if our shepherd has been dismantled"
	
	^(myFlags bitAnd: FlockInfo dismantled) ~~ UInt32Zero.!
*/
}
/**
 * Return true if my Shepherd's new state is it should be forgotten.
 */
public boolean isForgotten() {
	return wasForgotten() != isForgottenStateDirty();
/*
udanax-top.st:26581:FlockInfo methodsFor: 'testing'!
{BooleanVar} isForgotten
	"Return true if my Shepherd's new state is it should be forgotten."
	
	^self wasForgotten ~~ self isForgottenStateDirty!
*/
}
/**
 * Return true if the shepherd I describe is changing between being forgotten and being
 * remembered.
 */
public boolean isForgottenStateDirty() {
	return (myFlags & FlockInfo.forgottenStateDirty()) != 0;
/*
udanax-top.st:26586:FlockInfo methodsFor: 'testing'!
{BooleanVar} isForgottenStateDirty
	"Return true if the shepherd I describe is changing between being forgotten and being remembered."
	
	^(myFlags bitAnd: FlockInfo forgottenStateDirty) ~~ UInt32Zero!
*/
}
/**
 * Return true if my shepherd has been forwarded.
 */
public boolean isForwarded() {
	return (myFlags & FlockInfo.forwarded()) != 0;
/*
udanax-top.st:26591:FlockInfo methodsFor: 'testing'!
{BooleanVar} isForwarded
	"Return true if my shepherd has been forwarded."
	
	^(myFlags bitAnd: FlockInfo forwarded) ~~ UInt32Zero!
*/
}
/**
 * Return true if the associated flock is new.  If so, myIndex
 * is an offset into the new flocks table inside the SnarfPacker.
 */
public boolean isNew() {
	return (myFlags & FlockInfo.isNewMask()) != 0;
/*
udanax-top.st:26596:FlockInfo methodsFor: 'testing'!
{BooleanVar} isNew
	"Return true if the associated flock is new.  If so, myIndex
	 is an offset into the new flocks table inside the SnarfPacker."
	
	^(myFlags bitAnd: FlockInfo isNewMask) ~~ UInt32Zero!
*/
}
/**
 * Return true if my shepherd was forgotten after the last commit.
 */
public boolean wasForgotten() {
	return (myFlags & FlockInfo.forgottenMask()) != 0;
/*
udanax-top.st:26602:FlockInfo methodsFor: 'testing'!
{BooleanVar} wasForgotten
	"Return true if my shepherd was forgotten after the last commit."
	
	^(myFlags bitAnd: FlockInfo forgottenMask) ~~ UInt32Zero!
*/
}
/**
 * Return true if our shepherd pointer was NULL in makePersistent
 */
public boolean wasShepNullInPersistent() {
	return (myFlags & FlockInfo.shepNullInPersistent()) != 0;
/*
udanax-top.st:26607:FlockInfo methodsFor: 'testing'!
{BooleanVar} wasShepNullInPersistent
	"Return true if our shepherd pointer was NULL in makePersistent"
	
	^(myFlags bitAnd: FlockInfo shepNullInPersistent) ~~ UInt32Zero.!
*/
}
/**
 * Reset my contentsDirty flag.  This is primarily used to know when a flock has
 * changed again after some info has been computed from it.
 */
public void clearContentsDirty() {
	myFlags = myFlags & ~ FlockInfo.contentsDirty();
/*
udanax-top.st:26614:FlockInfo methodsFor: 'accessing'!
{void} clearContentsDirty
	"Reset my contentsDirty flag.  This is primarily used to know when a flock has
	 changed again after some info has been computed from it."
	
	myFlags _ myFlags bitAnd: FlockInfo contentsDirty bitInvert!
*/
}
/**
 * A write to the disk has happened.  Commit all the changes in the flags.
 */
public void commitFlags() {
	if (isForgottenStateDirty()) {
		myFlags = myFlags ^ FlockInfo.forgottenMask();
	}
	myFlags = myFlags & FlockInfo.forgottenMask();
/*
udanax-top.st:26620:FlockInfo methodsFor: 'accessing'!
{void} commitFlags
	"A write to the disk has happened.  Commit all the changes in the flags."
	
	self isForgottenStateDirty ifTrue: [myFlags _ myFlags bitXor: FlockInfo forgottenMask].
	myFlags _ myFlags bitAnd: FlockInfo forgottenMask!
*/
}
public int flags() {
	return myFlags;
/*
udanax-top.st:26626:FlockInfo methodsFor: 'accessing'!
{Int32} flags
	^myFlags!
*/
}
public int flockHash() {
	return myFlockHash;
/*
udanax-top.st:26629:FlockInfo methodsFor: 'accessing'!
{UInt4} flockHash
	^myFlockHash!
*/
}
/**
 * As a freshly forwarded flock, I'll be treated as new for a while.
 */
public void forward(int index) {
	myFlags = myFlags | FlockInfo.forwarded();
	index(index);
/*
udanax-top.st:26632:FlockInfo methodsFor: 'accessing'!
{void} forward: index {Int32}
	"As a freshly forwarded flock, I'll be treated as new for a while."
	
	myFlags _ myFlags bitOr: FlockInfo forwarded.
	self index: index.!
*/
}
/**
 * Set my contentsDirty flag.  Return false if I was already dirty (in either way).
 */
public boolean markContentsDirty() {
	boolean flag;
	flag = ! isDirty();
	myFlags = myFlags | FlockInfo.contentsDirty();
	return flag;
/*
udanax-top.st:26638:FlockInfo methodsFor: 'accessing'!
{BooleanVar} markContentsDirty
	"Set my contentsDirty flag.  Return false if I was already dirty (in either way)."
	
	| flag {BooleanVar} |
	flag _ self isDirty not.
	myFlags _ myFlags bitOr: FlockInfo contentsDirty.
	^flag!
*/
}
/**
 * Set my shepNull flag.
 */
public void markDestroyed() {
	myFlags = myFlags | FlockInfo.destroyed();
/*
udanax-top.st:26646:FlockInfo methodsFor: 'accessing'!
{void} markDestroyed
	"Set my shepNull flag."
	
	myFlags _ myFlags bitOr: FlockInfo destroyed.!
*/
}
/**
 * Set my Dismantled flag.  BLAST if already set.
 */
public void markDismantled() {
	if ( ! ( ! isDismantled())) {
		throw new AboraAssertionException("Already dismantled");
	}
	myFlags = myFlags | FlockInfo.dismantled();
/*
udanax-top.st:26651:FlockInfo methodsFor: 'accessing'!
{void} markDismantled
	"Set my Dismantled flag.  BLAST if already set."
	
	self isDismantled not assert: 'Already dismantled'.
	myFlags _ myFlags bitOr: FlockInfo dismantled.!
*/
}
/**
 * Set my Forgotten flag.  Return false if I was already dirty.
 */
public boolean markForgotten() {
	boolean flag;
	flag = ! isDirty();
	if ( ! isForgotten()) {
		myFlags = myFlags ^ FlockInfo.forgottenStateDirty();
	}
	return flag;
/*
udanax-top.st:26657:FlockInfo methodsFor: 'accessing'!
{BooleanVar} markForgotten
	"Set my Forgotten flag.  Return false if I was already dirty."
	
	| flag {BooleanVar} |
	flag _ self isDirty not.
	self isForgotten not ifTrue: [myFlags _ myFlags bitXor: FlockInfo forgottenStateDirty].
	^flag!
*/
}
/**
 * Clear my Forgotten flag.  Return false if I was already dirty.
 */
public boolean markRemembered() {
	boolean flag;
	flag = ! isDirty();
	if (isForgotten()) {
		myFlags = myFlags ^ FlockInfo.forgottenStateDirty();
	}
	return flag;
/*
udanax-top.st:26665:FlockInfo methodsFor: 'accessing'!
{BooleanVar} markRemembered
	"Clear my Forgotten flag.  Return false if I was already dirty."
	
	| flag {BooleanVar} |
	flag _ self isDirty not.
	self isForgotten ifTrue: [myFlags _ myFlags bitXor: FlockInfo forgottenStateDirty].
	^flag!
*/
}
/**
 * Set my shepNull flag.
 */
public void markShepNull() {
	myFlags = myFlags | FlockInfo.shepNullInPersistent();
/*
udanax-top.st:26673:FlockInfo methodsFor: 'accessing'!
{void} markShepNull
	"Set my shepNull flag."
	
	myFlags _ myFlags bitOr: FlockInfo shepNullInPersistent.!
*/
}
public int oldSize() {
	return myOldSize;
/*
udanax-top.st:26678:FlockInfo methodsFor: 'accessing'!
{Int32} oldSize
	^myOldSize!
*/
}
public void setSize(int size) {
	myOldSize = size;
/*
udanax-top.st:26681:FlockInfo methodsFor: 'accessing'!
{void} setSize: size {Int32}
	myOldSize _ size!
*/
}
public Abraham fetchShepherd() {
	if (myToken == 0) {
		return null;
	}
	if (myToken == -1) {
		/* Removed smalltalkOnly */
		return null;
	}
	else {
		return Abraham.fetchShepherd(myToken);
	}
/*
udanax-top.st:26686:FlockInfo methodsFor: 'tokens'!
{Abraham} fetchShepherd
  myToken == nil ifTrue: [^NULL].
	myToken == -1
		ifTrue: [
		[self halt]smalltalkOnly. ^ NULL ]
		ifFalse: [ ^Abraham fetchShepherd: myToken]!
*/
}
public Abraham getShepherd() {
	Abraham shep;
	shep = fetchShepherd();
	if (shep == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_SHEPHERD);
	}
	return shep;
/*
udanax-top.st:26694:FlockInfo methodsFor: 'tokens'!
{Abraham} getShepherd
	| shep {Abraham} |
	
	shep := self fetchShepherd.
	shep == NULL ifTrue: [ Heaper BLAST: #NullShepherd ].
	^ shep!
*/
}
/**
 * Register this info as the best known informatino about the flock.
 */
public void registerInfo() {
	((DiskManager) CurrentPacker.fluidGet()).flockInfoTable().store(myToken, this);
	/* Removed smalltalkOnly */
/*
udanax-top.st:26701:FlockInfo methodsFor: 'tokens'!
{void} registerInfo
	"Register this info as the best known informatino about the flock."
	
	CurrentPacker fluidGet flockInfoTable at: myToken store: self.
	[| cat shep |
	shep _ self getShepherd.
	[shep == nil ifTrue: [self halt]]smalltalkOnly.
	shep isStub ifTrue: [cat _ shep getCategoryFromStub] ifFalse: [cat _ shep getCategory].
	TheFlockCategoryTable at: myToken store: cat] smalltalkOnly!
*/
}
public int token() {
	/* Removed smalltalkOnly */
	return myToken;
/*
udanax-top.st:26711:FlockInfo methodsFor: 'tokens'!
{Int32} token
[myToken == nil ifTrue: [self halt]] smalltalkOnly.
	^myToken!
*/
}
public FlockInfo(Abraham shep, int snarfID, int index, int flags, int size) {
	super(snarfID, index);
	myFlockHash = shep.hashForEqual();
	myToken = shep.token();
	/* Removed smalltalkOnly */
	myFlags = flags;
	myOldSize = size;
	/* Removed smalltalkOnly */
/*
udanax-top.st:26717:FlockInfo methodsFor: 'create'!
create: shep {Abraham} with: snarfID {SnarfID} with: index {Int32} with: flags {Int32} with: size {Int32}
	super create: snarfID with: index.
	
	myFlockHash _ shep hashForEqual.
	
	myToken _ shep token.
	[myToken == nil ifTrue:[self halt]]smalltalkOnly.
	myFlags _ flags.
	myOldSize _ size.
	[shep == NULL ifTrue:[self halt] ]smalltalkOnly!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	if (isContentsDirty()) {
		oo.print("D");
	}
	if (isNew()) {
		oo.print("N");
	}
	if (isDestroyed()) {
		oo.print("X"
		/* X for Xtinct */
		);
	}
	if (isDismantled()) {
		oo.print("Z"
		/* Z for zapped */
		);
	}
	if (wasForgotten()) {
		oo.print("-");
	}
	else {
		oo.print("+");
	}
	if (isForgotten()) {
		oo.print("-");
	}
	else {
		oo.print("+");
	}
	oo.print(", ");
	oo.print(snarfID());
	oo.print(", ");
	oo.print(index());
	oo.print(", ");
	oo.print(myOldSize);
	oo.print(")");
/*
udanax-top.st:26730:FlockInfo methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '('.
	self isContentsDirty ifTrue: [oo << 'D'].
	self isNew ifTrue: [oo << 'N'].
	self isDestroyed ifTrue: [oo << 'X' "X for Xtinct"].
	self isDismantled ifTrue: [oo << 'Z' "Z for zapped"].
	self wasForgotten ifTrue: [oo << '-'] ifFalse: [oo << '+'].
	self isForgotten ifTrue: [oo << '-'] ifFalse: [oo << '+'].
	oo << ', ' << self snarfID << ', ' << self index << ', ' << myOldSize << ')'!
*/
}
public static FlockInfo forgotten(Abraham shep, int snarfID, int index) {
	return new FlockInfo(shep, snarfID, index, FlockInfo.forgottenMask(), 0);
/*
udanax-top.st:26756:FlockInfo class methodsFor: 'creation'!
{FlockInfo} forgotten: shep {Abraham} with: snarfID {SnarfID} with: index {Int32} 
	^ self create: shep
		with: snarfID
		with: index 
		with: FlockInfo forgottenMask
		with: Int32Zero.!
*/
}
/**
 * Make a ShepherdLocation for a new shepherd. Index is the index into
 * the new flocks table in the snarfPacker. The newmask indicates
 * that the index is into the newFlocks table rather than a snarf.
 */
public static FlockInfo make(Abraham shep, int index) {
	return new FlockInfo(shep, 0, index, (((FlockInfo.contentsDirty() | FlockInfo.forgottenStateDirty()) & ~ FlockInfo.forgottenMask()) | FlockInfo.isNewMask()), 0);
/*
udanax-top.st:26763:FlockInfo class methodsFor: 'creation'!
make: shep {Abraham} with: index {IntegerVar} 
	"Make a ShepherdLocation for a new shepherd. Index is the index into 
	the new flocks table in the snarfPacker. The newmask indicates 
	that the index is into the newFlocks table rather than a snarf."
	^ self create: shep 
		with: Int32Zero
		with: index DOTasLong 
		with: (((FlockInfo contentsDirty bitOr: FlockInfo forgottenStateDirty)
				bitAnd: FlockInfo forgottenMask bitInvert)
				bitOr: FlockInfo isNewMask)
		with: Int32Zero.!
*/
}
/**
 * Make a flockInfo to a new location for the same shepherd.  Clear the new flag, and keep
 * the rest the same.
 */
public static FlockInfo make(FlockInfo info, int snarfID, int index) {
	return new FlockInfo(info.getShepherd(), snarfID, index, (info.flags() & ~ FlockInfo.isNewMask()), info.oldSize());
/*
udanax-top.st:26776:FlockInfo class methodsFor: 'creation'!
make: info {FlockInfo} with: snarfID {SnarfID} with: index {Int32} 
	"Make a flockInfo to a new location for the same shepherd.  Clear the new flag, and keep the rest the same."
	^self create: info getShepherd
		with: snarfID
		with: index
		with: (info flags bitAnd: FlockInfo isNewMask bitInvert)
		with: info oldSize!
*/
}
public static FlockInfo remembered(Abraham shep, int snarfID, int index) {
	return new FlockInfo(shep, snarfID, index, 0, 0);
/*
udanax-top.st:26785:FlockInfo class methodsFor: 'creation'!
{FlockInfo} remembered: shep {Abraham} with: snarfID {SnarfID} with: index {Int32} 
	^ self create: shep
		with: snarfID 
		with: index 
		with: UInt32Zero
		with: Int32Zero.!
*/
}
public static boolean testContentsDirty(FlockInfo info) {
	return info.isContentsDirty();
/*
udanax-top.st:26794:FlockInfo class methodsFor: 'debugging tools'!
{BooleanVar} testContentsDirty: info {FlockInfo}
	^info isContentsDirty!
*/
}
public static boolean testForgotten(FlockInfo info) {
	return info.isForgotten();
/*
udanax-top.st:26797:FlockInfo class methodsFor: 'debugging tools'!
{BooleanVar} testForgotten: info {FlockInfo}
	^info isForgotten!
*/
}
public static int contentsDirty() {
	return 4;
/*
udanax-top.st:26802:FlockInfo class methodsFor: 'testing flags'!
{UInt32 INLINE} contentsDirty
	^ 4!
*/
}
public static int destroyed() {
	return 16;
/*
udanax-top.st:26805:FlockInfo class methodsFor: 'testing flags'!
{UInt32 INLINE} destroyed
	^ 16!
*/
}
public static int dismantled() {
	return 32;
/*
udanax-top.st:26808:FlockInfo class methodsFor: 'testing flags'!
{UInt32 INLINE} dismantled
	^ 32!
*/
}
public static int forgottenMask() {
	return 1;
/*
udanax-top.st:26811:FlockInfo class methodsFor: 'testing flags'!
{UInt32 INLINE} forgottenMask
	^ 1!
*/
}
public static int forgottenStateDirty() {
	return 2;
/*
udanax-top.st:26814:FlockInfo class methodsFor: 'testing flags'!
{UInt32 INLINE} forgottenStateDirty
	^ 2!
*/
}
public static int forwarded() {
	return 128;
/*
udanax-top.st:26817:FlockInfo class methodsFor: 'testing flags'!
{UInt32 INLINE} forwarded
	^ 128!
*/
}
public static int isNewMask() {
	return 8;
/*
udanax-top.st:26820:FlockInfo class methodsFor: 'testing flags'!
{UInt32 INLINE} isNewMask
	^ 8!
*/
}
public static int shepNullInPersistent() {
	return 64;
/*
udanax-top.st:26823:FlockInfo class methodsFor: 'testing flags'!
{UInt32 INLINE} shepNullInPersistent
	^ 64!
*/
}
public static void staticTimeNonInherited() {
	TheFlockCategoryTable = PrimPtrTable.make(2048);
/*
udanax-top.st:26828:FlockInfo class methodsFor: 'smalltalk: initialization'!
staticTimeNonInherited
	[TheFlockCategoryTable _ PrimPtrTable make: 2048] smalltalkOnly!
*/
}
public static FlockInfo getInfo(int index) {
	return (FlockInfo) (((DiskManager) CurrentPacker.fluidGet()).flockInfoTable().get(index));
/*
udanax-top.st:26833:FlockInfo class methodsFor: 'flock tables'!
{FlockInfo} getInfo: index {Int32}
	[DiskManager] USES.
	^ (CurrentPacker fluidGet flockInfoTable get: index) cast: FlockInfo!
*/
}
public static void removeInfo(int token) {
	((DiskManager) CurrentPacker.fluidGet()).flockInfoTable().remove(token);
	/* Abraham returnToken: token */
/*
udanax-top.st:26837:FlockInfo class methodsFor: 'flock tables'!
{void} removeInfo: token {Int32}
	CurrentPacker fluidGet flockInfoTable remove: token.
	"Abraham returnToken: token"!
*/
}
public FlockInfo() {
/*

Generated during transformation
*/
}
public FlockInfo(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
