/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.FakePacker;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XcvrMaker;

/**
 * Most of the disk operations are just no-ops.
 */
public class FakePacker extends DiskManager {

	protected Turtle myTurtle;
	protected int myCount;
/*
udanax-top.st:16693:
DiskManager subclass: #FakePacker
	instanceVariableNames: '
		myTurtle {Turtle | NULL}
		myCount {UInt4}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:16699:
FakePacker comment:
'Most of the disk operations are just no-ops.'!
*/
/*
udanax-top.st:16701:
(FakePacker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:16828:
FakePacker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:16831:
(FakePacker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FakePacker.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void beginConsistent(int dirty) {
/*
udanax-top.st:16706:FakePacker methodsFor: 'transactions'!
{void} beginConsistent: dirty {IntegerVar unused}!
*/
}
public void endConsistent(int dirty) {
	Agenda agenda;
	if ( ! (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue())) {
		agenda = myTurtle.fetchAgenda();
		if (agenda != null && ( ! ((Boolean) InsideAgenda.fluidFetch()).booleanValue())) {
			Object insideAgendaOldValue = AboraBlockSupport.enterFluidBindDuring(InsideAgenda, true);
			try {
				while (agenda.step());
			}
			finally {
				AboraBlockSupport.exitFluidBindDuring(InsideAgenda, insideAgendaOldValue);
			}
		}
	}
/*
udanax-top.st:16708:FakePacker methodsFor: 'transactions'!
{void} endConsistent: dirty {IntegerVar unused}
	
	| agenda {Agenda | NULL} |
	InsideTransactionFlag fluidFetch
		ifFalse: 
			[agenda _ myTurtle fetchAgenda.
			(agenda ~~ NULL and: [InsideAgenda fluidFetch not])
				ifTrue: 
					[InsideAgenda fluidBind: true during:
						[[agenda step] whileTrue]]]!
*/
}
public boolean insideCommit() {
	return false;
/*
udanax-top.st:16719:FakePacker methodsFor: 'transactions'!
{BooleanVar} insideCommit
	^ false!
*/
}
/**
 * Flush everything out to disk and remove all purgeable imaged
 * objects from memory.  This doesn't clear the ShepherdMap table.
 * This will have to be a weak table, and then the destruction of a
 * shepherd or shepherdStub should remove it from myShepherdMap.
 */
public void purge() {
/*
udanax-top.st:16722:FakePacker methodsFor: 'transactions'!
{void} purge
	"Flush everything out to disk and remove all purgeable imaged
	 objects from memory.  This doesn't clear the ShepherdMap table.  
	 This will have to be a weak table, and then the destruction of a 
	 shepherd or shepherdStub should remove it from myShepherdMap."!
*/
}
/**
 * No shepherds are clean, so no-op.
 */
public void purgeClean(boolean noneLocked) {
/*
udanax-top.st:16728:FakePacker methodsFor: 'transactions'!
{void} purgeClean: noneLocked {BooleanVar unused default: false}
	"No shepherds are clean, so no-op."!
*/
}
/**
 * Queue destroy of the given flock.  dismantle it immediately in the FakePacker.
 */
public void destroyFlock(FlockInfo info) {
	Someone.knownBug();
	/* This needs to stack shepherds for deletion after all agenda items. */
	info.markDestroyed();
	info.getShepherd().dismantle();
/*
udanax-top.st:16733:FakePacker methodsFor: 'shepherds'!
{void} destroyFlock: info {FlockInfo} 
	"Queue destroy of the given flock.  dismantle it immediately in the FakePacker."
	
	self knownBug.  "This needs to stack shepherds for deletion after all agenda items."
	info markDestroyed.
	info getShepherd dismantle!
*/
}
/**
 * The flock identified by token is Dirty!! On some later commit, write it to the disk.
 */
public void diskUpdate(FlockInfo info) {
/*
udanax-top.st:16740:FakePacker methodsFor: 'shepherds'!
{void} diskUpdate: info {FlockInfo | NULL} 
	"The flock identified by token is Dirty!! On some later commit, write it to the disk."!
*/
}
/**
 * Tehre are no local data-structures.
 */
public void dismantleFlock(FlockInfo info) {
	/* info markDismantled. */
/*
udanax-top.st:16743:FakePacker methodsFor: 'shepherds'!
{void} dismantleFlock: info {FlockInfo} 
	"Tehre are no local data-structures."
	"info markDismantled."!
*/
}
/**
 * No prob.
 */
public void dropFlock(int token) {
/*
udanax-top.st:16747:FakePacker methodsFor: 'shepherds'!
{void} dropFlock: token {Int32}
	"No prob."!
*/
}
/**
 * Yeah. Right.
 */
public void forgetFlock(FlockInfo info) {
/*
udanax-top.st:16750:FakePacker methodsFor: 'shepherds'!
{void} forgetFlock: info {FlockInfo} 
	"Yeah. Right."!
*/
}
public Turtle getInitialFlock() {
	return myTurtle;
/*
udanax-top.st:16753:FakePacker methodsFor: 'shepherds'!
{Turtle} getInitialFlock
	^ myTurtle!
*/
}
/**
 * Shepherds use a sequence number for their hash.  Return the next one
 * and increment.  This should actually spread the hashes.
 */
public int nextHashForEqual() {
	/* This actually needs to roll over the UInt32 limit. */
	myCount = myCount + 1;
	return myCount;
/*
udanax-top.st:16757:FakePacker methodsFor: 'shepherds'!
{UInt32} nextHashForEqual
	"Shepherds use a sequence number for their hash.  Return the next one
	 and increment.  This should actually spread the hashes."
	"This actually needs to roll over the UInt32 limit."
	myCount _ myCount + 1.
	^ myCount!
*/
}
/**
 * There are now persistent pointers to the shepherd represented by token.
 */
public void rememberFlock(FlockInfo info) {
/*
udanax-top.st:16764:FakePacker methodsFor: 'shepherds'!
{void} rememberFlock: info {FlockInfo} 
	"There are now persistent pointers to the shepherd represented by token."!
*/
}
/**
 * Do nothing
 */
public void storeAlmostNewShepherd(Abraham shep) {
/*
udanax-top.st:16767:FakePacker methodsFor: 'shepherds'!
{void} storeAlmostNewShepherd: shep {Abraham unused} 
	"Do nothing"!
*/
}
public void storeInitialFlock(Abraham turtle, XcvrMaker protocol, Cookbook cookbook) {
	throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_REAL_DISK_MANAGER);
/*
udanax-top.st:16771:FakePacker methodsFor: 'shepherds'!
{void} storeInitialFlock: turtle {Abraham unused}
	with: protocol {XcvrMaker unused}
	with: cookbook {Cookbook unused}
	Heaper BLAST: #MustBeRealDiskManager!
*/
}
/**
 * Shep just got created!! On some later commit, assign it to a snarf
 * and write it to the disk.
 */
public void storeNewFlock(Abraham shep) {
	FlockInfo info;
	if ( ! (shep.fetchInfo() == null)) {
		throw new AboraAssertionException("Must not have an info yet");
	}
	/* Create a FlockInfo to make the FlockTable registration happy. */
	info = FlockInfo.make(shep, - myCount);
	shep.flockInfo(info);
/*
udanax-top.st:16777:FakePacker methodsFor: 'shepherds'!
{void} storeNewFlock: shep {Abraham} 
	"Shep just got created!! On some later commit, assign it to a snarf 
	and write it to the disk."
	| info {FlockInfo} |
	shep fetchInfo == NULL assert: 'Must not have an info yet'.
	"Create a FlockInfo to make the FlockTable registration happy."
	info _ FlockInfo make: shep with: myCount negated.
	shep flockInfo: info.!
*/
}
public void storeTurtle(Turtle turtle) {
	myTurtle = turtle;
/*
udanax-top.st:16787:FakePacker methodsFor: 'shepherds'!
{void} storeTurtle: turtle {Turtle}
	myTurtle _ turtle!
*/
}
/**
 * If something is already imaged at that location, then return it. If there is already
 * an existing stub with the same hash at a different location, follow them till we
 * know that they are actually different objects.
 */
public Abraham fetchCanonical(int hash, int snarfID, int index) {
	throw new UnimplementedException();
/*
udanax-top.st:16792:FakePacker methodsFor: 'stubs'!
{Abraham} fetchCanonical: hash {UInt32 unused} with: snarfID {SnarfID unused} with: index {Int32 unused}
	"If something is already imaged at that location, then return it. If there is already
	 an existing stub with the same hash at a different location, follow them till we 
	 know that they are actually different objects."
	self unimplemented.
	^NULL!
*/
}
/**
 * Retrieve from the disk the flock at index within the specified snarf.  Since
 * stubs are canonical, and this only gets called by stubs, the existing stub will
 * *become* the shepherd for the flock.
 */
public void makeReal(FlockInfo info) {
	throw new UnimplementedException();
/*
udanax-top.st:16800:FakePacker methodsFor: 'stubs'!
{void} makeReal: info {FlockInfo unused}
	"Retrieve from the disk the flock at index within the specified snarf.  Since
	 stubs are canonical, and this only gets called by stubs, the existing stub will 
	 *become* the shepherd for the flock."
	self unimplemented!
*/
}
public void registerStub(Abraham shep, int snarfID, int index) {
	throw new UnimplementedException();
/*
udanax-top.st:16807:FakePacker methodsFor: 'stubs'!
{void} registerStub: shep {Abraham unused} with: snarfID {SnarfID unused} with: index {Int32 unused}
	self unimplemented!
*/
}
public FakePacker() {
	super();
	myTurtle = null;
	myCount = 0;
/*
udanax-top.st:16813:FakePacker methodsFor: 'protected: create'!
create
	super create.
	myTurtle _ NULL.
	myCount _ UInt32Zero.!
*/
}
public boolean isFake() {
	return true;
/*
udanax-top.st:16820:FakePacker methodsFor: 'testing'!
{BooleanVar} isFake
	^ true!
*/
}
public void destroyAbandoned() {
/*
udanax-top.st:16825:FakePacker methodsFor: 'internals'!
{void} destroyAbandoned!
*/
}
public static DiskManager make() {
	DiskManager packer;
	packer = new FakePacker();
	CurrentPacker.fluidSet(packer);
	return packer;
/*
udanax-top.st:16836:FakePacker class methodsFor: 'creation'!
{DiskManager} make
	
	| packer {DiskManager} |
	packer _ FakePacker create.
	CurrentPacker fluidSet: packer.
	^packer!
*/
}
public FakePacker(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
