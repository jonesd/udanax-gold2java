/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.backend.DiskManagerEmulsion;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.collection.basic.WeakPtrArray;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.diskman.Cattleman;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Array;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.java.missing.smalltalk.Smalltalk;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.snarf.SnarfPacker;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;

/**
 * This is the public interface for managing objects that should go to disk.
 * This is also the anchor for the so-called Backend emulsion, but I''ll call it
 * the DiskManager emulsion for simplicity.
 */
public class DiskManager extends Heaper {

	protected Array myFluidSpace;
	protected PrimPtrTable myFlockInfoTable;
	protected WeakPtrArray myFlockTable;
	protected static Emulsion SecretEmulsion;
/*
udanax-top.st:16213:
Heaper subclass: #DiskManager
	instanceVariableNames: '
		myFluidSpace {char star}
		myFlockInfoTable {PrimPtrTable}
		myFlockTable {WeakPtrArray}'
	classVariableNames: 'SecretEmulsion {Emulsion star} '
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:16220:
DiskManager comment:
'This is the public interface for managing objects that should go to disk.
This is also the anchor for the so-called Backend emulsion, but I''ll call it
the DiskManager emulsion for simplicity.'!
*/
/*
udanax-top.st:16224:
(DiskManager getOrMakeCxxClassDescription)
	friends:
'/- friends for class DiskManager -/
friend class Abraham;
';
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:16431:
DiskManager class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:16434:
(DiskManager getOrMakeCxxClassDescription)
	friends:
'/- friends for class DiskManager -/
friend class Abraham;
';
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DiskManager.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Queue destroy of the given flock.  The destroy will probably happen later.
 */
public void destroyFlock(FlockInfo info) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16233:DiskManager methodsFor: 'shepherds'!
{void} destroyFlock: info {FlockInfo} 
	"Queue destroy of the given flock.  The destroy will probably happen later."
	 
	self subclassResponsibility!
*/
}
/**
 * The flock described by info is Dirty!! On the next commit, rewrite it to the disk.
 */
public void diskUpdate(FlockInfo info) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16238:DiskManager methodsFor: 'shepherds'!
{void} diskUpdate: info {FlockInfo | NULL} 
	"The flock described by info is Dirty!! On the next commit, rewrite it to the disk."
	self subclassResponsibility!
*/
}
/**
 * The flock designated by info has completed all dismantling actions; throw it off the disk.
 */
public void dismantleFlock(FlockInfo info) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16243:DiskManager methodsFor: 'shepherds'!
{void} dismantleFlock: info {FlockInfo} 
	"The flock designated by info has completed all dismantling actions; throw it off the disk."
	 
	self subclassResponsibility!
*/
}
/**
 * The flock identified by token is being removed from memory. For now, this is an
 * error if the flock has been updated. If the flock has been forgotten, then it will
 * be dismantled when next it comes in from disk.
 */
public void dropFlock(int token) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16248:DiskManager methodsFor: 'shepherds'!
{void} dropFlock: token {Int32}
	"The flock identified by token is being removed from memory. For now, this is an 
	error if the flock has been updated. If the flock has been forgotten, then it will 
	be dismantled when next it comes in from disk."
	self subclassResponsibility!
*/
}
/**
 * Remember that there are no more persistent pointers to the shepherd
 * described by info. If it gets garbage collected, remember to dismantle it
 * when it comes back in from the disk.
 */
public void forgetFlock(FlockInfo info) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16255:DiskManager methodsFor: 'shepherds'!
{void} forgetFlock: info {FlockInfo} 
	"Remember that there are no more persistent pointers to the shepherd 
	described by info. If it gets garbage collected, remember to dismantle it 
	when it comes back in from the disk."
	self subclassResponsibility!
*/
}
/**
 * Return the starting object for the entire backend. This will be the 0th
 * flock in the first snarf following the snarfInfo tables. This will eventually
 * always be a shepherd that describes the protocol of the rest of the disk.
 */
public Turtle getInitialFlock() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16262:DiskManager methodsFor: 'shepherds'!
{Turtle} getInitialFlock
	"Return the starting object for the entire backend. This will be the 0th 
	flock in the first snarf following the snarfInfo tables. This will eventually 
	always be a shepherd that describes the protocol of the rest of the disk."
	self subclassResponsibility!
*/
}
/**
 * Shepherds use a sequence number for their hash. The most trivial (reasonable)
 * implementation just uses a BatchCounter. This will not be persistent till we get
 * Turtles.
 */
public int nextHashForEqual() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16269:DiskManager methodsFor: 'shepherds'!
{UInt32} nextHashForEqual
	"Shepherds use a sequence number for their hash. The most trivial (reasonable) 
	implementation just uses a BatchCounter. This will not be persistent till we get 
	Turtles."
	self subclassResponsibility!
*/
}
/**
 * There are now persistent pointers to the shepherd described by info.  See forgetFlock.
 */
public void rememberFlock(FlockInfo info) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16276:DiskManager methodsFor: 'shepherds'!
{void} rememberFlock: info {FlockInfo} 
	"There are now persistent pointers to the shepherd described by info.  See forgetFlock."
	 
	self subclassResponsibility!
*/
}
public void setHashCounter(Counter aCounter) {
/*
udanax-top.st:16281:DiskManager methodsFor: 'shepherds'!
{void} setHashCounter: aCounter {Counter unused}!
*/
}
/**
 * Shep has been created, but is not consistent yet. storeNewFlock must be called on it
 * before the next makeConsistent.
 */
public void storeAlmostNewShepherd(Abraham shep) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16283:DiskManager methodsFor: 'shepherds'!
{void} storeAlmostNewShepherd: shep {Abraham} 
	"Shep has been created, but is not consistent yet. storeNewFlock must be called on it before the next makeConsistent."
	 
	self subclassResponsibility!
*/
}
/**
 * A turtle just got created!! Remember it as the initial flock.
 */
public void storeInitialFlock(Abraham turtle, XcvrMaker protocol, Cookbook cookbook) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16288:DiskManager methodsFor: 'shepherds'!
{void} storeInitialFlock: turtle {Abraham} with: protocol {XcvrMaker} with: cookbook {Cookbook}
	"A turtle just got created!! Remember it as the initial flock."
	self subclassResponsibility!
*/
}
/**
 * Shep just got created!! On some later commit, assign it to a snarf
 * and write it to the disk.
 */
public void storeNewFlock(Abraham shep) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16292:DiskManager methodsFor: 'shepherds'!
{void} storeNewFlock: shep {Abraham} 
	"Shep just got created!! On some later commit, assign it to a snarf 
	and write it to the disk."
	 
	self subclassResponsibility!
*/
}
/**
 * If something is already imaged at that location, then return it. If there is already
 * an existing stub with the same hash at a different location, follow them both till we
 * know that they are actually different objects.
 */
public Abraham fetchCanonical(int hash, int snarfID, int index) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16300:DiskManager methodsFor: 'stubs'!
{Abraham} fetchCanonical: hash {UInt32} with: snarfID {SnarfID} with: index {Int32}
	"If something is already imaged at that location, then return it. If there is already
	 an existing stub with the same hash at a different location, follow them both till we 
	 know that they are actually different objects."
	self subclassResponsibility!
*/
}
/**
 * Retrieve from the disk the flock at index within the specified snarf.  Since
 * stubs are canonical, and this only gets called by stubs, the existing stub will
 * *become* the shepherd for the flock.
 */
public void makeReal(FlockInfo info) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16307:DiskManager methodsFor: 'stubs'!
{void} makeReal: info {FlockInfo}
	"Retrieve from the disk the flock at index within the specified snarf.  Since
	 stubs are canonical, and this only gets called by stubs, the existing stub will 
	 *become* the shepherd for the flock."
	self subclassResponsibility!
*/
}
/**
 * Called to register a newly created stub (by the diskSpecialist) in the internal
 * tables. The diskSpecialist in particular calls this when it couldn't find an
 * already existing stub (with fetchCacnonical) representing the flock at the
 * particular location.
 */
public void registerStub(Abraham shep, int snarfID, int index) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16314:DiskManager methodsFor: 'stubs'!
{void} registerStub: shep {Abraham} with: snarfID {SnarfID} with: index {Int32}
	"Called to register a newly created stub (by the diskSpecialist) in the internal 
	tables. The diskSpecialist in particular calls this when it couldn't find an 
	already existing stub (with fetchCacnonical) representing the flock at the 
	particular location."
	self subclassResponsibility!
*/
}
/**
 * This is called before entering consistent block.  'dirty' is the block's declaration of
 * the maximum number of shepherds which it can dirty.  If this is a top level consistent
 * block, the virtual image in memory is now in a consistent state. It may be written to the
 * disk if necessary.
 */
public void beginConsistent(int dirty) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16324:DiskManager methodsFor: 'transactions'!
{void} beginConsistent: dirty {IntegerVar}
	"This is called before entering consistent block.  'dirty' is the block's declaration of the maximum number of shepherds which it can dirty.  If this is a top level consistent block, the virtual image in memory is now in a consistent state. It may be written to the disk if necessary.  "
	
	self subclassResponsibility!
*/
}
/**
 * This is called after beginConsistent, but before entering a consistent block, for
 * debugging purposes.  Default is to do nothing
 */
public void consistentBlockAt(String fileName, int lineNo) {
/*
udanax-top.st:16329:DiskManager methodsFor: 'transactions'!
{void} consistentBlockAt: fileName {char star unused} with: lineNo {Int32 unused}
	"This is called after beginConsistent, but before entering a consistent block, for debugging purposes.  Default is to do nothing"!
*/
}
/**
 * This is called after exiting a consistent block.
 */
public void endConsistent(int dirty) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16332:DiskManager methodsFor: 'transactions'!
{void} endConsistent: dirty {IntegerVar}
	"This is called after exiting a consistent block."
	
	self subclassResponsibility!
*/
}
public boolean insideCommit() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16337:DiskManager methodsFor: 'transactions'!
{BooleanVar} insideCommit
	self subclassResponsibility!
*/
}
/**
 * Flush everything out to disk and remove all purgeable imaged objects from memory.
 */
public void purge() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16340:DiskManager methodsFor: 'transactions'!
{void} purge
	"Flush everything out to disk and remove all purgeable imaged objects from memory. "
	 
	self subclassResponsibility!
*/
}
/**
 * purge all shepherds that are currently clean, not locked, not dirty, and
 * purgeable. Purging just turns them into stubs, freeing the rest of their flocks.
 * Garbage collection can clean up the flocks and any stubs no longer pointed to
 * by something in memory.
 */
public void purgeClean(boolean noneLocked) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16345:DiskManager methodsFor: 'transactions'!
{void} purgeClean: noneLocked {BooleanVar default: false}
	"purge all shepherds that are currently clean, not locked, not dirty, and 
	purgeable. Purging just turns them into stubs, freeing the rest of their flocks. 
	Garbage collection can clean up the flocks and any stubs no longer pointed to 
	by something in memory."
	self subclassResponsibility!
*/
}
/*
udanax-top.st:16355:DiskManager methodsFor: 'smalltalk: passe'!
{void} consistent: aBlock {BlockClosure}
	"Execute the block inside a pseudo-transaction."
	self passe!
*/
/*
udanax-top.st:16360:DiskManager methodsFor: 'smalltalk: passe'!
{void} consistent: dirty {IntegerVar} with: aBlock {BlockClosure}
	"Execute the block inside a pseudo-transaction."
	self passe!
*/
/**
 * The virtual image in memory is now in a consistent state. It may be written to
 * the disk if necessary.
 * @deprecated
 */
public void makeConsistent() {
	throw new PasseException();
/*
udanax-top.st:16365:DiskManager methodsFor: 'smalltalk: passe'!
{void} makeConsistent
	"The virtual image in memory is now in a consistent state. It may be written to 
	the disk if necessary."
	
	self passe!
*/
}
/**
 * The virtual image in memory is now in a consistent state. It may be written to the disk if
 * necessary.  This is called before entering a top level consistent block.  'dirty' is the
 * block's declaration of the maximum number of shepherds which it can dirty.
 * @deprecated
 */
public void makeConsistentBegin(int dirty) {
	throw new PasseException();
/*
udanax-top.st:16371:DiskManager methodsFor: 'smalltalk: passe'!
{void} makeConsistentBegin: dirty {IntegerVar}
	"The virtual image in memory is now in a consistent state. It may be written to the disk if necessary.  This is called before entering a top level consistent block.  'dirty' is the block's declaration of the maximum number of shepherds which it can dirty."
	
	self passe!
*/
}
/**
 * This is called after exiting a top level consistent block.
 * @deprecated
 */
public void makeConsistentEnd() {
	throw new PasseException();
/*
udanax-top.st:16376:DiskManager methodsFor: 'smalltalk: passe'!
{void} makeConsistentEnd
	"This is called after exiting a top level consistent block."
	
	self passe!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:16383:DiskManager methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public boolean isFake() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:16386:DiskManager methodsFor: 'testing'!
{BooleanVar} isFake
	self subclassResponsibility!
*/
}
public void flockInfoTable(PrimPtrTable table) {
	myFlockInfoTable = table;
/*
udanax-top.st:16391:DiskManager methodsFor: 'protected: accessing'!
{void INLINE} flockInfoTable: table{PrimPtrTable}
	myFlockInfoTable := table!
*/
}
public void flockTable(WeakPtrArray table) {
	myFlockTable = table;
/*
udanax-top.st:16394:DiskManager methodsFor: 'protected: accessing'!
{void INLINE} flockTable: table {WeakPtrArray}
	myFlockTable := table.!
*/
}
public PrimPtrTable flockInfoTable() {
	return myFlockInfoTable;
/*
udanax-top.st:16399:DiskManager methodsFor: 'accessing'!
{PrimPtrTable INLINE} flockInfoTable
	^ myFlockInfoTable!
*/
}
public WeakPtrArray flockTable() {
	return myFlockTable;
/*
udanax-top.st:16402:DiskManager methodsFor: 'accessing'!
{WeakPtrArray INLINE} flockTable
	^ myFlockTable!
*/
}
public DiskManager() {
	super();
	myFluidSpace = null;
	myFlockInfoTable = PrimPtrTable.make(2048);
	myFlockTable = (WeakPtrArray) WeakPtrArray.make((Cattleman.make(this)), 2048);
/*
udanax-top.st:16407:DiskManager methodsFor: 'protected: creation'!
create
	super create.
	myFluidSpace _ NULL.
	myFlockInfoTable _ PrimPtrTable make: 2048.
	myFlockTable _ WeakPtrArray make: (Cattleman make: self) with: 2048.!
*/
}
public void destruct() {
	if (myFluidSpace != null) {
		Object currentPackerOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentPacker, this);
		try {
			DiskManager.emulsion().destructAll();
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentPacker, currentPackerOldValue);
		}
	}
	super.destruct();
/*
udanax-top.st:16413:DiskManager methodsFor: 'protected: creation'!
{void} destruct
	(myFluidSpace ~~ NULL) ifTrue: [
		CurrentPacker fluidBind: self during:
			[DiskManager emulsion destructAll]]. 
	super destruct.!
*/
}
public Array fluidSpace() {
	return myFluidSpace;
/*
udanax-top.st:16422:DiskManager methodsFor: 'emulsion accessing'!
{char star} fluidSpace
	^myFluidSpace.!
*/
}
public Array fluidSpace(Array aFluidSpace) {
	return myFluidSpace = aFluidSpace;
/*
udanax-top.st:16426:DiskManager methodsFor: 'emulsion accessing'!
{char star} fluidSpace: aFluidSpace {char star}
	^myFluidSpace _ aFluidSpace.!
*/
}
/**
 * This builds the disk managing structure.
 */
public static DiskManager initializeDisk(String fname) {
	CurrentPacker.fluidSet((SnarfPacker.initializeUrdiOnDisk(fname)));
	return ((DiskManager) CurrentPacker.fluidGet());
/*
udanax-top.st:16443:DiskManager class methodsFor: 'creation'!
{DiskManager} initializeDisk: fname {char star}
	"This builds the disk managing structure."
	CurrentPacker fluidSet: (SnarfPacker initializeUrdiOnDisk: fname).
	^CurrentPacker fluidGet!
*/
}
public static DiskManager make(String fname) {
	CurrentPacker.fluidSet((SnarfPacker.make(fname)));
	return ((DiskManager) CurrentPacker.fluidGet());
/*
udanax-top.st:16449:DiskManager class methodsFor: 'creation'!
make: fname {char star}
	CurrentPacker fluidSet: (SnarfPacker make: fname).
	^CurrentPacker fluidGet!
*/
}
public static Emulsion emulsion() {
	/* Removed smalltalkOnly */
	if (SecretEmulsion == null) {
		SecretEmulsion = DiskManagerEmulsion.make();
	}
	return SecretEmulsion;
/*
udanax-top.st:16456:DiskManager class methodsFor: 'emulsion accessing'!
{Emulsion} emulsion
	[SecretEmulsion == nil ifTrue: [SecretEmulsion _ NULL]] smalltalkOnly.
	(SecretEmulsion == NULL) ifTrue: [
		SecretEmulsion _ DiskManagerEmulsion make].
	^SecretEmulsion.!
*/
}
public static void cleanupGarbage() {
	Smalltalk.atPut(DISK_CUISINE, null);
	SecretEmulsion = null;
/*
udanax-top.st:16464:DiskManager class methodsFor: 'smalltalk: initialization'!
{void} cleanupGarbage
	DiskCuisine _ NULL.
	SecretEmulsion _ NULL.!
*/
}
public static void exitTimeNonInherited() {
	if (((DiskManager) CurrentPacker.fluidFetch()) != null) {
		((DiskManager) CurrentPacker.fluidGet()).destroy();
		CurrentPacker.fluidSet(null);
	}
/*
udanax-top.st:16468:DiskManager class methodsFor: 'smalltalk: initialization'!
{void} exitTimeNonInherited
	CurrentPacker fluidFetch ~~ NULL
		ifTrue:
			[CurrentPacker fluidGet destroy.
			CurrentPacker fluidSet: NULL]!
*/
}
public static void linkTimeNonInherited() {
	Recipe.defineGlobal(DISK_CUISINE, null);
	SecretEmulsion = null;
/*
udanax-top.st:16474:DiskManager class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	Recipe star defineGlobal: #DiskCuisine with: NULL.
	SecretEmulsion _ NULL.!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(DiskManager.class, "CurrentPacker", Emulsion.globalEmulsion(), null);
	AboraSupport.defineFluid(boolean.class, "InsideAgenda", DiskManager.emulsion(), Boolean.FALSE);
/*
udanax-top.st:16478:DiskManager class methodsFor: 'smalltalk: initialization'!
staticTimeNonInherited
	DiskManager defineFluid: #CurrentPacker with: Emulsion globalEmulsion with: [NULL].
	BooleanVar defineFluid: #InsideAgenda with: DiskManager emulsion with: [false].!
*/
}
public static void bombConsistentBlock(int CHARGE) {
	((DiskManager) CurrentPacker.fluidGet()).endConsistent(CHARGE);
/*
udanax-top.st:16484:DiskManager class methodsFor: 'exceptions: exceptions'!
bomb.ConsistentBlock: CHARGE {IntegerVar}
	^[CurrentPacker fluidGet endConsistent: CHARGE]!
*/
}
/*
udanax-top.st:16490:DiskManager class methodsFor: 'smalltalk: transactions'!
{void} consistent: aBlock {BlockClosure}
	"Execute the block inside a pseudo-transaction."
	DiskManager consistent: -1 with: aBlock with: thisContext sender!
*/
/*
udanax-top.st:16495:DiskManager class methodsFor: 'smalltalk: transactions'!
{void} consistent: dirty {IntegerVar} with: aBlock {BlockClosure} 
	"Execute the block inside a pseudo-transaction."
	
	self knownBug. "there are still unbounded consistent bugs which need to be broken up"
	self consistent: dirty with: aBlock with: thisContext sender!
*/
/*
udanax-top.st:16501:DiskManager class methodsFor: 'smalltalk: transactions'!
{void} consistent: dirty {IntegerVar default: -1} with: aBlock {BlockClosure} with: context {Context}
	
	| fileName {String} |
	CurrentPacker fluidGet beginConsistent: dirty.
	"(context isKindOf: MethodContext)
		ifTrue: [fileName _ context printString]
		ifFalse: [fileName _ '[] in ', context mclass name, '>>', context selector].
	CurrentPacker fluidGet consistentBlockAt: fileName with: context pc."
	[InsideTransactionFlag fluidBind: true 
			during: aBlock] 
		valueNowOrOnUnwindDo: (DiskManager bomb.ConsistentBlock: dirty)!
*/
/*
udanax-top.st:16513:DiskManager class methodsFor: 'smalltalk: transactions'!
{void} insistent: aBlock {BlockClosure}
	"Execute the block inside a pseudo-transaction."
	DiskManager insistent: -1 with: aBlock with: thisContext sender!
*/
/*
udanax-top.st:16518:DiskManager class methodsFor: 'smalltalk: transactions'!
{void} insistent: dirty {IntegerVar} with: aBlock {BlockClosure} 
	"Execute the block inside a pseudo-transaction."
	
	self insistent: dirty with: aBlock with: thisContext sender!
*/
/*
udanax-top.st:16523:DiskManager class methodsFor: 'smalltalk: transactions'!
{void} insistent: dirty {IntegerVar default: -1} with: aBlock {BlockClosure} with: context {Context}
	
	InsideTransactionFlag fluidFetch assert: 'Must be inside a transaction'.
	DiskManager consistent: dirty with: aBlock with: context!
*/
public DiskManager(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public void destroyAbandoned() {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
public void purgeClean() {
	purgeClean( false );
/*

Generated during transformation: AddDefaultParameter
*/
}
}
