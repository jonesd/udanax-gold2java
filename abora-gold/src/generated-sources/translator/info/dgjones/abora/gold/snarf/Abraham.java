/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.WeakPtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.ShepFlag;
import info.dgjones.abora.gold.java.missing.ShepherdStub;
import info.dgjones.abora.gold.java.missing.smalltalk.IdentityDictionary;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.tokens.TokenSource;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.DeletedHeaper;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class Abraham extends Heaper {

	protected int myHash;
	protected int myToken;
	protected FlockInfo myInfo;
	protected static IdentityDictionary DismantleStatistics;
	protected static TokenSource TheTokenSource;
/*
udanax-top.st:3:
Heaper subclass: #Abraham
	instanceVariableNames: '
		myHash {UInt32}
		myToken {Int32 NOCOPY}
		myInfo {FlockInfo NOCOPY}'
	classVariableNames: '
		DismantleStatistics {IdentityDictionary smalltalk of: Category and: IntegerVar} 
		TheTokenSource {TokenSource} '
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:14:
(Abraham getOrMakeCxxClassDescription)
	friends:
'friend class SnarfPacker;
friend class TestPacker;
friend class FakePacker;
friend class SnarfRecord;
friend class SnarfHandler;
friend void  unlockFunctionAvoidingDestroy (Abraham *);
friend class RecorderHoister;
';
	attributes: ((Set new) add: #DEFERRED.LOCKED; add: #DEFERRED; add: #COPY; yourself)!
*/
/*
udanax-top.st:269:
Abraham class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:272:
(Abraham getOrMakeCxxClassDescription)
	friends:
'friend class SnarfPacker;
friend class TestPacker;
friend class FakePacker;
friend class SnarfRecord;
friend class SnarfHandler;
friend void  unlockFunctionAvoidingDestroy (Abraham *);
friend class RecorderHoister;
';
	attributes: ((Set new) add: #DEFERRED.LOCKED; add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Abraham.class).setAttributes( new Set().add("DEFERREDLOCKED").add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Replace the shepherd in memory with a type compatible stub
 * instance that shares the same hash and flockInfo.
 */
public void becomeStub() {
	/* NOTE: Should this ensure that the flock is not dirty? */
	/* Each subclass of Abraham will have an implementation of the form: 
		new (this) MyStubClass()' or:
		'this->changeClassToThatOf(ProtoStubClass)' */
	int theHash;
	FlockInfo info;
	Category theCategory;
	theHash = myHash;
	info = myInfo;
	theCategory = getCategory();
	/* TODO newBecome */
	new ShepherdStub(theHash, info, theCategory);
	/* Removed translateOnly */
/*
udanax-top.st:28:Abraham methodsFor: 'protected: destruction'!
{void} becomeStub
	"Replace the shepherd in memory with a type compatible stub
	 instance that shares the same hash and flockInfo."
	 
	 "NOTE: Should this ensure that the flock is not dirty?"
	
	"Each subclass of Abraham will have an implementation of the form: 
		new (this) MyStubClass()' or:
		'this->changeClassToThatOf(ProtoStubClass)'"
	[| theHash {UInt32} info {FlockInfo} theCategory {Category} |
	theHash _ myHash.
	info _ myInfo.
	theCategory _ self getCategory.
	(ShepherdStub new.Become: self) create: theHash with: info with: theCategory] smalltalkOnly.
	[self unimplemented] translateOnly!
*/
}
/**
 * Called when an object is leaving RAM.  Additional behavior for subclasses of Abraham:
 * Tell the snarfPacker that I am leaving RAM and should be removed from its tables.
 */
public void destruct() {
	if (myInfo != null) {
		((DiskManager) CurrentPacker.fluidGet()).dropFlock(myToken);
	}
	super.destruct();
/*
udanax-top.st:45:Abraham methodsFor: 'protected: destruction'!
{void NOFAULT NOLOCK} destruct
	"Called when an object is leaving RAM.  Additional behavior for subclasses of Abraham:
	Tell the snarfPacker that I am leaving RAM and should be removed from its tables."
	
	myInfo ~~ NULL
		ifTrue: [CurrentPacker fluidGet dropFlock: myToken].
	super destruct!
*/
}
/**
 * Disconnect me from the universe and throw me off the disk.
 * For GC safety, we keep a strongptr to ourself -- is this still necessary?
 */
public void dismantle() {
	Abraham spt;
	DiskManager packer;
	spt = this;
	Category pos;
	pos = getCategory();
	DismantleStatistics.put(pos, (DismantleStatistics.ifAbsent(pos, 0)) + 1);
	/* Tell the disk the flock is dismantled. */
	packer = ((DiskManager) CurrentPacker.fluidGet());
	packer.dismantleFlock(myInfo);
	packer.flockTable().store(myToken, null);
	if (myInfo != null) {
		packer.dropFlock(myToken);
	}
/*
udanax-top.st:53:Abraham methodsFor: 'protected: destruction'!
{void} dismantle
	"Disconnect me from the universe and throw me off the disk. 
	
	For GC safety, we keep a strongptr to ourself -- is this still necessary?"
	| spt {Abraham} packer {DiskManager} |
	spt _ self.
	[| pos {Category} |
	pos _ self getCategory.
	DismantleStatistics at: pos put: (DismantleStatistics at: pos ifAbsent: [0]) + 1] smalltalkOnly.
	"Tell the disk the flock is dismantled."
	packer _ CurrentPacker fluidGet.
	packer dismantleFlock: myInfo.
	packer flockTable at: myToken store: NULL.
	myInfo ~~ NULL
		ifTrue: [packer dropFlock: myToken].!
*/
}
/**
 * The receiver has changed and so must eventually be rewritten to disk.
 */
public void diskUpdate() {
	if (myInfo == null) {
		/* Before a newShepherd. */
		((DiskManager) CurrentPacker.fluidGet()).storeAlmostNewShepherd(this);
	}
	else {
		((DiskManager) CurrentPacker.fluidGet()).diskUpdate(myInfo);
	}
/*
udanax-top.st:73:Abraham methodsFor: 'protected: disk'!
{void} diskUpdate
	"The receiver has changed and so must eventually be rewritten to disk."
	myInfo == NULL
		ifTrue: ["Before a newShepherd." 
				CurrentPacker fluidGet storeAlmostNewShepherd: self]
		ifFalse: [CurrentPacker fluidGet diskUpdate: myInfo]!
*/
}
/**
 * Record on disk that there are no more persistent pointers to the receiver.  When the in
 * core pointers go away, the receiver can be dismantled from disk.  That will happen
 * eventually.
 */
public void forget() {
	((DiskManager) CurrentPacker.fluidGet()).forgetFlock(myInfo);
/*
udanax-top.st:81:Abraham methodsFor: 'protected: disk'!
{void NOFAULT} forget
	"Record on disk that there are no more persistent pointers to the receiver.  When the in core pointers go away, the receiver can be dismantled from disk.  That will happen eventually."
	CurrentPacker fluidGet forgetFlock: myInfo!
*/
}
/**
 * The receiver has just been created. Put it on disk.
 */
public void newShepherd() {
	((DiskManager) CurrentPacker.fluidGet()).storeNewFlock(this);
/*
udanax-top.st:86:Abraham methodsFor: 'protected: disk'!
{void NOFAULT} newShepherd
	"The receiver has just been created. Put it on disk."
 
	CurrentPacker fluidGet storeNewFlock: self!
*/
}
/**
 * Record that there are now persistent pointers to the receiver.
 */
public void remember() {
	((DiskManager) CurrentPacker.fluidGet()).rememberFlock(myInfo);
/*
udanax-top.st:91:Abraham methodsFor: 'protected: disk'!
{void NOFAULT} remember
	"Record that there are now persistent pointers to the receiver."
	CurrentPacker fluidGet rememberFlock: myInfo!
*/
}
/**
 * Tell the packer I want to go away. It will mark me
 * as forgotten and actually dismantle me when it next
 * exits a consistent block. This avoids Jackpotting
 * when destroying a tree of objects.
 */
public void destroy() {
	/* [myToken < CurrentPacker fluidGet flockTable count 
		ifTrue: [CurrentPacker fluidGet flockTable at: myToken store: NULL]] smalltalkOnly. */
	((DiskManager) CurrentPacker.fluidGet()).destroyFlock(myInfo);
/*
udanax-top.st:98:Abraham methodsFor: 'destruction'!
{void} destroy
	"Tell the packer I want to go away. It will mark me 
	as forgotten and actually dismantle me when it next 
	exits a consistent block. This avoids Jackpotting 
	when destroying a tree of objects."
	"[myToken < CurrentPacker fluidGet flockTable count 
		ifTrue: [CurrentPacker fluidGet flockTable at: myToken store: NULL]] smalltalkOnly."
	CurrentPacker fluidGet destroyFlock: myInfo!
*/
}
public int actualHashForEqual() {
	return myHash;
/*
udanax-top.st:110:Abraham methodsFor: 'testing'!
{UInt32 NOFAULT} actualHashForEqual
	^myHash!
*/
}
/**
 * A hash of the contents of this flock
 */
public int contentsHash() {
	return getCategory().hashForEqual();
/*
udanax-top.st:113:Abraham methodsFor: 'testing'!
{UInt32} contentsHash
	"A hash of the contents of this flock"
	^self getCategory hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:117:Abraham methodsFor: 'testing'!
{BooleanVar NOFAULT} isEqual: other {Heaper}
	^self == other!
*/
}
/**
 * Return false only if the object cannot be flushed to disk. This will probably
 * only be false for Stamps and the like that contain session level pointers.
 */
public boolean isPurgeable() {
	return true;
/*
udanax-top.st:120:Abraham methodsFor: 'testing'!
{BooleanVar} isPurgeable
	"Return false only if the object cannot be flushed to disk. This will probably 
	only be false for Stamps and the like that contain session level pointers."
	^true!
*/
}
/**
 * This should be replaced with an isKindOf: that first checks to see
 * if you're asking about Abraham, and then otherwise possible faults.
 */
public boolean isShepherd() {
	Someone.hack();
	return true;
/*
udanax-top.st:126:Abraham methodsFor: 'testing'!
{BooleanVar NOFAULT} isShepherd
	"This should be replaced with an isKindOf: that first checks to see
	  if you're asking about Abraham, and then otherwise possible faults."
	
	self hack.
	^true!
*/
}
/**
 * Distinguish between stubs and shepherds.
 */
public boolean isStub() {
	return false;
/*
udanax-top.st:133:Abraham methodsFor: 'testing'!
{BooleanVar NOFAULT} isStub 
	"Distinguish between stubs and shepherds."
	
	^false!
*/
}
/**
 * All manually generated subclasses are locked.  Automatically
 * defined unlocked classes will reimplement this.
 */
public boolean isUnlocked() {
	return false;
/*
udanax-top.st:138:Abraham methodsFor: 'testing'!
{BooleanVar} isUnlocked
	"All manually generated subclasses are locked.  Automatically
	 defined unlocked classes will reimplement this."
	
	^false!
*/
}
/**
 * Return the object that describes the state of this flock wrt disk.
 */
public FlockInfo fetchInfo() {
	/* This should be made protected. */
	return myInfo;
/*
udanax-top.st:146:Abraham methodsFor: 'accessing'!
{FlockInfo NOFAULT} fetchInfo
	"Return the object that describes the state of this flock wrt disk."
	 
	 "This should be made protected."
	^myInfo!
*/
}
/**
 * Set the object that knows where this flock is on disk.  Change it when the object moves.
 */
public void flockInfo(FlockInfo info) {
	WeakPtrArray flocks;
	if (AboraSupport.findCategory(info.getClass()) == AboraSupport.findCategory(DeletedHeaper.class)) {
		halt();
	}
	myInfo = info;
	if (info.token() != myToken && (myToken != 0)) {
		Abraham.returnToken(myToken);
	}
	myToken = myInfo.token();
	/* Register when a flockInfo has been assigned. */
	flocks = ((DiskManager) CurrentPacker.fluidGet()).flockTable();
	if (myToken != 0) {
		if (myToken >= flocks.count()) {
			/* Grow if necessary. */
			((DiskManager) CurrentPacker.fluidGet()).flockTable(((WeakPtrArray) (flocks.copyGrow(myToken))));
			flocks.destroy();
			flocks = ((DiskManager) CurrentPacker.fluidGet()).flockTable();
		}
	}
	else {
		halt();
	}
	flocks.store(myToken, this);
	myInfo.registerInfo();
/*
udanax-top.st:152:Abraham methodsFor: 'accessing'!
{void NOFAULT} flockInfo: info {FlockInfo}
	"Set the object that knows where this flock is on disk.  Change it when the object moves."
 
	| flocks {WeakPtrArray} |
	
	[info class == DeletedHeaper ifTrue: [self halt]] smalltalkOnly.
	
	myInfo _ info.
	(info token ~~ myToken and: [myToken ~~ nil]) ifTrue: [Abraham returnToken: myToken].
	myToken _ myInfo token.
	
	"Register when a flockInfo has been assigned."
	flocks _ CurrentPacker fluidGet flockTable.
	myToken ~~ nil ifTrue: 
	   [myToken >= flocks count ifTrue:
		["Grow if necessary."
		CurrentPacker fluidGet flockTable: ((flocks copyGrow: myToken) cast: WeakPtrArray).
		flocks destroy.
		flocks _ CurrentPacker fluidGet flockTable]]
		ifFalse: [[self halt] smalltalkOnly].
	flocks at: myToken store: self.
	myInfo registerInfo!
*/
}
/**
 * Return the object that describes the state of this flock wrt disk.
 */
public FlockInfo getInfo() {
	if (myInfo == null) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_INITIALIZED);
	}
	if (AboraSupport.findCategory(myInfo.getClass()) == AboraSupport.findCategory(DeletedHeaper.class)) {
		throw new AboraRuntimeException("info was deleted");
	}
	return myInfo;
/*
udanax-top.st:175:Abraham methodsFor: 'accessing'!
{FlockInfo NOFAULT} getInfo
	"Return the object that describes the state of this flock wrt disk."
	 
	myInfo == NULL ifTrue: [Heaper BLAST: #MustBeInitialized].
	[(myInfo class == DeletedHeaper) ifTrue: [self error: 'info was deleted']] smalltalkOnly.
	^myInfo!
*/
}
/**
 * Return the category of stubs used for the receiver. Shepherd Patriarch classes reimplement
 * this to use more specific Stub types.
 */
public Category getShepherdStubCategory() {
	return AboraSupport.findCategory(ShepherdStub.class);
/*
udanax-top.st:182:Abraham methodsFor: 'accessing'!
{Category NOFAULT} getShepherdStubCategory
	"Return the category of stubs used for the receiver. Shepherd Patriarch classes reimplement this to use more specific Stub types."
	[^ShepherdStub] smalltalkOnly.
	'
	BLAST(SHEPHERD_HAS_NO_STUB_DEFINED);
	return NULL;' translateOnly!
*/
}
/**
 * Return the object that describes the state of this flock wrt disk.
 */
public int token() {
	if (myToken == 0) {
		halt();
		myToken = TheTokenSource.takeToken();
	}
	return myToken;
/*
udanax-top.st:190:Abraham methodsFor: 'accessing'!
{Int32 NOFAULT} token
	"Return the object that describes the state of this flock wrt disk."
	 myToken == nil ifTrue: [[self halt] smalltalkOnly.
	 myToken _ TheTokenSource takeToken
	 ].
	^myToken!
*/
}
/**
 * New Shepherds must be stored to disk.
 */
public Abraham() {
	super();
	myHash = ((DiskManager) CurrentPacker.fluidGet()).nextHashForEqual();
	/* Start out remembered, changing to forgotten.  They also start out as
	 if they were on disk (newShepherd must be called to make it so.  This
	 prevents intermediate diskUpdates from forcing a new object to disk 
	 before creation is finished. */
	restartAbraham();
/*
udanax-top.st:199:Abraham methodsFor: 'protected: create'!
create
	"New Shepherds must be stored to disk."
	
	super create.
	myHash _ CurrentPacker fluidGet nextHashForEqual.
	"Start out remembered, changing to forgotten.  They also start out as
	 if they were on disk (newShepherd must be called to make it so.  This
	 prevents intermediate diskUpdates from forcing a new object to disk 
	 before creation is finished."
	
	self restartAbraham!
*/
}
/**
 * This is the root of the automatically generated constructors for creating Stubs.
 */
public Abraham(ShepFlag ignored, int hash, FlockInfo info) {
	super();
	myHash = hash;
	if (AboraSupport.findCategory(info.getClass()) == AboraSupport.findCategory(DeletedHeaper.class)) {
		halt();
	}
	restartAbraham();
	if (info != null) {
		flockInfo(info);
	}
/*
udanax-top.st:211:Abraham methodsFor: 'protected: create'!
create.ShepFlag: ignored {ShepFlag var unused} with: hash {UInt32} with: info {FlockInfo}
	"This is the root of the automatically generated constructors for creating Stubs."
	
	super create.
	myHash _ hash.
	[info class == DeletedHeaper ifTrue: [self halt]] smalltalkOnly.
	self restartAbraham.
	info ~~ NULL ifTrue: [self flockInfo: info]!
*/
}
/**
 * This is for shepherds that are becoming from another shepherd.
 */
public Abraham(int hash) {
	super();
	Someone.thingToDo();
	/* Change my callers to use Abraham::Abraham(UInt32,APTR(FlockInfo)).  The flockInfo should be restored at the Abraham level instead of below.  This also more likely causes the type checker to catch inappropriate become-constructor use */
	myHash = hash;
	restartAbraham();
/*
udanax-top.st:220:Abraham methodsFor: 'protected: create'!
{INLINE} create: hash {UInt32}
	"This is for shepherds that are becoming from another shepherd."
	
	super create.
	self thingToDo. "Change my callers to use Abraham::Abraham(UInt32,APTR(FlockInfo)).  The flockInfo should be restored at the Abraham level instead of below.  This also more likely causes the type checker to catch inappropriate become-constructor use"
	
	myHash _ hash.
	
	self restartAbraham!
*/
}
public void restartAbraham(Rcvr trans) {
	/* Transform: Rewrote body */
	myToken = TheTokenSource.takeToken();
	myInfo = null;
/*
udanax-top.st:232:Abraham methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartAbraham: trans {Rcvr unused default: NULL}
	myToken _ TheTokenSource takeToken.
	myToken == nil ifTrue:  [self halt] smalltalkOnly.
	myInfo _ NULL.!
*/
}
/**
 * This is for ShepherdStubs that use the hash and forgetFlag from the object for which they
 * are stubbing.
 */
public Abraham(int hash, FlockInfo info) {
	super();
	myHash = hash;
	if (AboraSupport.findCategory(info.getClass()) == AboraSupport.findCategory(DeletedHeaper.class)) {
		halt();
	}
	flockInfo(info);
/*
udanax-top.st:240:Abraham methodsFor: 'smalltalk: only'!
create: hash {UInt32} with: info {FlockInfo}
	"This is for ShepherdStubs that use the hash and forgetFlag from the object for which they are stubbing."
	
	super create.
	myHash _ hash.
	[info class == DeletedHeaper ifTrue: [self halt]] smalltalkOnly.
	self flockInfo: info.!
*/
}
/*
udanax-top.st:248:Abraham methodsFor: 'smalltalk: only'!
{BooleanVar} isKindOf: cat {Category} 
	"Optimized for Abraham because xcvrs use it so much."
	^cat == Abraham or: [super isKindOf: cat]!
*/
public void restartAbraham() {
	restartAbraham(null);
/*
udanax-top.st:253:Abraham methodsFor: 'smalltalk: only'!
{void} restartAbraham
	self restartAbraham: NULL!
*/
}
public Abraham(Rcvr receiver) {
	super(receiver);
	myHash = receiver.receiveUInt32();
	restartAbraham(receiver);
/*
udanax-top.st:259:Abraham methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myHash _ receiver receiveUInt32.
	self restartAbraham: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendUInt32(myHash);
/*
udanax-top.st:264:Abraham methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendUInt32: myHash.!
*/
}
public static IdentityDictionary dismantleStatistics() {
	return DismantleStatistics;
/*
udanax-top.st:286:Abraham class methodsFor: 'smalltalk: utilities'!
dismantleStatistics
	^DismantleStatistics!
*/
}
public static void cleanupGarbage() {
	linkTimeNonInherited();
/*
udanax-top.st:291:Abraham class methodsFor: 'smalltalk: cleanup'!
cleanupGarbage
	self linkTimeNonInherited!
*/
}
public static void initTimeNonInherited() {
	DismantleStatistics = new IdentityDictionary();
	mayBecome(AboraSupport.findCategory(ShepherdStub.class));
	TheTokenSource = TokenSource.make();
/*
udanax-top.st:297:Abraham class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	[DismantleStatistics _ IdentityDictionary new] smalltalkOnly.
	[self mayBecome: ShepherdStub] smalltalkOnly.
	
	TheTokenSource _ TokenSource make.!
*/
}
public static void linkTimeNonInherited() {
	TheTokenSource = null;
/*
udanax-top.st:303:Abraham class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	TheTokenSource _ NULL!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(boolean.class, "InsideTransactionFlag", DiskManager.emulsion(), Boolean.FALSE);
/*
udanax-top.st:307:Abraham class methodsFor: 'smalltalk: initialization'!
staticTimeNonInherited
	BooleanVar defineFluid: #InsideTransactionFlag with: DiskManager emulsion with: [false].!
*/
}
public static boolean isConstructed(Heaper obj) {
	return obj != null && (obj.getCategory() != AboraSupport.findCategory(DeletedHeaper.class));
/*
udanax-top.st:312:Abraham class methodsFor: 'global: functions'!
{BooleanVar INLINE} isConstructed: obj {Heaper}
	^obj ~~ NULL and: [obj getCategory ~~ DeletedHeaper]!
*/
}
public static boolean isDestructed(Heaper obj) {
	return obj == null || (obj.getCategory() == AboraSupport.findCategory(DeletedHeaper.class));
/*
udanax-top.st:315:Abraham class methodsFor: 'global: functions'!
{BooleanVar INLINE} isDestructed: obj {Heaper}
	^obj == NULL or: [obj getCategory == DeletedHeaper]!
*/
}
public static Abraham fetchShepherd(int token) {
	PtrArray table;
	table = ((DiskManager) CurrentPacker.fluidGet()).flockTable();
	if (token < table.count()) {
		return (Abraham) (table.fetch(token));
	}
	else {
		return null;
	}
/*
udanax-top.st:320:Abraham class methodsFor: 'tokens'!
{Abraham} fetchShepherd: token {Int32}
	| table {PtrArray} |
	table := CurrentPacker fluidGet flockTable.
	token < table count
		ifTrue: [^(table fetch: token) cast: Abraham]
		ifFalse: [^NULL]!
*/
}
public static void returnToken(int token) {
	TheTokenSource.returnToken(token);
/*
udanax-top.st:327:Abraham class methodsFor: 'tokens'!
{void} returnToken: token {Int32}
	TheTokenSource returnToken: token!
*/
}
public Category getCategoryFromStub() {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
}
