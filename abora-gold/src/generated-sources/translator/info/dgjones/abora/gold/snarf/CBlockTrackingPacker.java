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
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.CBlockTracker;
import info.dgjones.abora.gold.snarf.CBlockTrackingPacker;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.snarf.SnarfPacker;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XcvrMaker;

public class CBlockTrackingPacker extends DiskManager {

	protected DiskManager myPacker;
	protected CBlockTracker myTracker;
/*
udanax-top.st:16528:
DiskManager subclass: #CBlockTrackingPacker
	instanceVariableNames: '
		myPacker {DiskManager}
		myTracker {CBlockTracker | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:16534:
(CBlockTrackingPacker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:16681:
CBlockTrackingPacker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:16684:
(CBlockTrackingPacker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CBlockTrackingPacker.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void beginConsistent(int dirty) {
	myTracker = CBlockTracker.make(dirty, myTracker);
	myPacker.beginConsistent(dirty);
/*
udanax-top.st:16539:CBlockTrackingPacker methodsFor: 'transactions'!
{void} beginConsistent: dirty {IntegerVar}
	myTracker _ CBlockTracker make: dirty with: myTracker.
	myPacker beginConsistent: dirty!
*/
}
public void consistentBlockAt(String fileName, int lineNo) {
	if (checkTracker()) {
		myTracker.track(fileName, lineNo);
		myPacker.consistentBlockAt(fileName, lineNo);
	}
/*
udanax-top.st:16544:CBlockTrackingPacker methodsFor: 'transactions'!
{void} consistentBlockAt: fileName {char star} with: lineNo {Int32}
	
	self checkTracker ifTrue:
		[myTracker track: fileName with: lineNo.
		myPacker consistentBlockAt: fileName with: lineNo]!
*/
}
public void endConsistent(int dirty) {
	if (checkTracker()) {
		myTracker = myTracker.fetchUnwrapped();
		myPacker.endConsistent(dirty);
	}
/*
udanax-top.st:16550:CBlockTrackingPacker methodsFor: 'transactions'!
{void} endConsistent: dirty {IntegerVar} 
	
	self checkTracker ifTrue:
		[myTracker _ myTracker fetchUnwrapped.
		myPacker endConsistent: dirty]!
*/
}
public boolean insideCommit() {
	return myPacker.insideCommit();
/*
udanax-top.st:16556:CBlockTrackingPacker methodsFor: 'transactions'!
{BooleanVar} insideCommit
	^ myPacker insideCommit!
*/
}
public void purge() {
	myPacker.purge();
/*
udanax-top.st:16559:CBlockTrackingPacker methodsFor: 'transactions'!
{void} purge
	myPacker purge!
*/
}
public void purgeClean(boolean noneLocked) {
	myPacker.purgeClean(noneLocked);
/*
udanax-top.st:16563:CBlockTrackingPacker methodsFor: 'transactions'!
{void} purgeClean: noneLocked {BooleanVar default: false}
	myPacker purgeClean: noneLocked!
*/
}
/**
 * Queue destroy of the given flock.  The destroy will probably happen later.
 */
public void destroyFlock(FlockInfo info) {
	myPacker.destroyFlock(info);
/*
udanax-top.st:16570:CBlockTrackingPacker methodsFor: 'shepherds'!
{void} destroyFlock: info {FlockInfo} 
	"Queue destroy of the given flock.  The destroy will probably happen later."
	
	myPacker destroyFlock: info!
*/
}
public void diskUpdate(FlockInfo info) {
	if (checkTracker()) {
		myTracker.dirty(info);
		myPacker.diskUpdate(info);
	}
/*
udanax-top.st:16575:CBlockTrackingPacker methodsFor: 'shepherds'!
{void} diskUpdate: info {FlockInfo | NULL} 
	
	self checkTracker ifTrue:
		[myTracker dirty: info.
		myPacker diskUpdate: info]!
*/
}
/**
 * The flock designated by info has completed all dismantling actions; throw it off the disk.
 */
public void dismantleFlock(FlockInfo info) {
	myPacker.dismantleFlock(info);
/*
udanax-top.st:16581:CBlockTrackingPacker methodsFor: 'shepherds'!
{void} dismantleFlock: info {FlockInfo} 
	"The flock designated by info has completed all dismantling actions; throw it off the disk."
	
	myPacker dismantleFlock: info!
*/
}
public void dropFlock(int token) {
	myPacker.dropFlock(token);
/*
udanax-top.st:16586:CBlockTrackingPacker methodsFor: 'shepherds'!
{void} dropFlock: token {Int32} 
	myPacker dropFlock: token!
*/
}
public void forgetFlock(FlockInfo info) {
	if (checkTracker()) {
		myTracker.dirty(info);
		myPacker.forgetFlock(info);
	}
/*
udanax-top.st:16590:CBlockTrackingPacker methodsFor: 'shepherds'!
{void} forgetFlock: info {FlockInfo} 
	self checkTracker ifTrue:
		[myTracker dirty: info.
		myPacker forgetFlock: info]!
*/
}
public Turtle getInitialFlock() {
	return myPacker.getInitialFlock();
/*
udanax-top.st:16596:CBlockTrackingPacker methodsFor: 'shepherds'!
{Turtle} getInitialFlock
	 
	^myPacker getInitialFlock!
*/
}
public int nextHashForEqual() {
	return myPacker.nextHashForEqual();
/*
udanax-top.st:16600:CBlockTrackingPacker methodsFor: 'shepherds'!
{UInt32} nextHashForEqual
	
	^myPacker nextHashForEqual!
*/
}
public void rememberFlock(FlockInfo info) {
	if (checkTracker()) {
		myTracker.dirty(info);
		myPacker.rememberFlock(info);
	}
/*
udanax-top.st:16604:CBlockTrackingPacker methodsFor: 'shepherds'!
{void} rememberFlock: info {FlockInfo} 
	
	self checkTracker ifTrue:
		[myTracker dirty: info.
		myPacker rememberFlock: info]!
*/
}
public void storeAlmostNewShepherd(Abraham shep) {
	myPacker.storeAlmostNewShepherd(shep);
/*
udanax-top.st:16610:CBlockTrackingPacker methodsFor: 'shepherds'!
{void} storeAlmostNewShepherd: shep {Abraham} 
	myPacker storeAlmostNewShepherd: shep!
*/
}
public void storeInitialFlock(Abraham turtle, XcvrMaker protocol, Cookbook cookbook) {
	myPacker.storeInitialFlock(turtle, protocol, cookbook);
/*
udanax-top.st:16614:CBlockTrackingPacker methodsFor: 'shepherds'!
{void} storeInitialFlock: turtle {Abraham} with: protocol {XcvrMaker} with: cookbook {Cookbook}
	myPacker storeInitialFlock: turtle with: protocol with: cookbook!
*/
}
public void storeNewFlock(Abraham shep) {
	if (checkTracker()) {
		myPacker.storeNewFlock(shep);
		myTracker.dirty(shep.getInfo());
	}
/*
udanax-top.st:16617:CBlockTrackingPacker methodsFor: 'shepherds'!
{void} storeNewFlock: shep {Abraham} 
	self checkTracker ifTrue:
		[myPacker storeNewFlock: shep.
		myTracker dirty: shep getInfo]!
*/
}
public Abraham fetchCanonical(int hash, int snarfID, int index) {
	return myPacker.fetchCanonical(hash, snarfID, index);
/*
udanax-top.st:16625:CBlockTrackingPacker methodsFor: 'stubs'!
{Abraham} fetchCanonical: hash {UInt32} with: snarfID {SnarfID} with: index {Int32}
	
	^myPacker fetchCanonical: hash with: snarfID with: index!
*/
}
public void makeReal(FlockInfo info) {
	myPacker.makeReal(info);
/*
udanax-top.st:16629:CBlockTrackingPacker methodsFor: 'stubs'!
{void} makeReal: info {FlockInfo}
	
	myPacker makeReal: info!
*/
}
public void registerStub(Abraham shep, int snarfID, int index) {
	myPacker.registerStub(shep, snarfID, index);
/*
udanax-top.st:16633:CBlockTrackingPacker methodsFor: 'stubs'!
{void} registerStub: shep {Abraham} with: snarfID {SnarfID} with: index {Int32}
	myPacker registerStub: shep with: snarfID with: index!
*/
}
public int consistentCount() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:16639:CBlockTrackingPacker methodsFor: 'smalltalk: testing'!
consistentCount
	^myPacker consistentCount!
*/
}
public CBlockTrackingPacker(DiskManager subPacker) {
	super();
	myPacker = subPacker;
	myTracker = null;
	flockTable(myPacker.flockTable());
	flockInfoTable(myPacker.flockInfoTable());
/*
udanax-top.st:16644:CBlockTrackingPacker methodsFor: 'create'!
create: subPacker {DiskManager} 
	super create.
	myPacker _ subPacker.
	myTracker _ NULL.
	self flockTable: myPacker flockTable.
	self flockInfoTable: myPacker flockInfoTable.!
*/
}
public void destruct() {
	if ( ! (myTracker == null)) {
		throw new AboraAssertionException();
	}
	myPacker.destroy();
	super.destruct();
/*
udanax-top.st:16654:CBlockTrackingPacker methodsFor: 'protected: destruction'!
{void} destruct
	
	(myTracker == NULL) assert.
	myPacker destroy.
	super destruct!
*/
}
public boolean isFake() {
	return myPacker.isFake();
/*
udanax-top.st:16662:CBlockTrackingPacker methodsFor: 'testing'!
{BooleanVar} isFake
	^ myPacker isFake!
*/
}
public boolean checkTracker() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:16667:CBlockTrackingPacker methodsFor: 'private:'!
{BooleanVar} checkTracker 
	
	myTracker ~~ NULL ifTrue:
		[^true].
	[Logger] USES.
	ErrorLog << 'Must be inside consistent block
'!
*/
}
/**
 * Used by ResetCommit bomb
 */
public void commitState(boolean flag) {
	((SnarfPacker) myPacker).commitState(flag);
/*
udanax-top.st:16675:CBlockTrackingPacker methodsFor: 'private:'!
{void} commitState: flag {BooleanVar}
	"Used by ResetCommit bomb"
	
	(myPacker cast: SnarfPacker) commitState: flag!
*/
}
public static DiskManager make(DiskManager subPacker) {
	return new CBlockTrackingPacker(subPacker);
/*
udanax-top.st:16689:CBlockTrackingPacker class methodsFor: 'creation'!
{DiskManager} make: subPacker {DiskManager} 
	
	^CBlockTrackingPacker create: subPacker!
*/
}
public CBlockTrackingPacker() {
/*

Generated during transformation
*/
}
public CBlockTrackingPacker(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
