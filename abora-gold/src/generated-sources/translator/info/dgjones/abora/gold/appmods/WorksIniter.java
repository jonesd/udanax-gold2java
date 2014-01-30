/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.appmods;

import info.dgjones.abora.gold.appmods.WorksIniter;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.locks.BooLock;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.detect.FeWaitDetector;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeBooLockSmith;
import info.dgjones.abora.gold.nadmin.FeClubDescription;
import info.dgjones.abora.gold.nkernel.FeClub;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.nkernel.WorksWaitDetector;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.wrapper.FeSet;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * The purpose of WorksIniter is to do the one-time initialization of clubs and homedocs to
 * prepare a backend for ordinary client use. It is pretty sparse right now, but will
 * eventually have much more stuff
 */
public class WorksIniter extends Thunk {

/*
udanax-top.st:62746:
Thunk subclass: #WorksIniter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-appmods'!
*/
/*
udanax-top.st:62750:
WorksIniter comment:
'The purpose of WorksIniter is to do the one-time initialization of clubs and homedocs to prepare a backend for ordinary client use. It is pretty sparse right now, but will eventually have much more stuff'!
*/
/*
udanax-top.st:62752:
(WorksIniter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WorksIniter.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void initializeClubs() {
	FeClub testClub;
	ID testID;
	/* Make an autonomous Test club */
	testClub = (FeClub) FeClub.make((FeClubDescription.make(FeSet.make(), FeBooLockSmith.make())).edition());
	testID = FeServer.iDOf(testClub);
	testClub.setReadClub(testID);
	testClub.setEditClub(testID);
	testClub.setSignatureClub(testID);
	testClub.setOwner(testID);
	FeServer.nameClub((Sequence.string("Test")), testID);
	FeServer.enableAccess(testID);
/*
udanax-top.st:62757:WorksIniter methodsFor: 'initialization'!
{void} initializeClubs
	
	| testClub {FeClub} testID {ID} |
	"Make an autonomous Test club"
	testClub := FeClub make: (FeClubDescription make: FeSet make with: FeBooLockSmith make) edition.
	testID := FeServer iDOf: testClub.
	testClub setReadClub: testID.
	testClub setEditClub: testID.
	testClub setSignatureClub: testID.
	testClub setOwner: testID.
	FeServer nameClub: (Sequence string: 'Test') with: testID.
	FeServer enableAccess: testID.!
*/
}
public void initializeSystem() {
	Connection aConnection;
	ID adminID;
	FeWaitDetector wwd;
	/* Removed smalltalkOnly */
	aConnection = Connection.make(AboraSupport.findCategory(FeServer.class));
	CurrentKeyMaster.fluidSet(((BooLock) (FeServer.loginByName((Sequence.string("System Admin"))))).boo());
	((FeKeyMaster) CurrentKeyMaster.fluidGet()).incorporate(FeKeyMaster.makePublic());
	adminID = FeServer.clubID((Sequence.string("System Admin")));
	InitialOwner.fluidSet(adminID);
	InitialReadClub.fluidSet(adminID);
	InitialEditClub.fluidSet(adminID);
	InitialSponsor.fluidSet(adminID);
	CurrentAuthor.fluidSet(adminID);
	initializeClubs();
	wwd = new WorksWaitDetector(AboraSupport.logger, "WorksInit done!");
	FeServer.waitForWrite(wwd);
	((DiskManager) CurrentPacker.fluidGet()).purge();
	/* Removed smalltalkOnly */
	aConnection.destroy();
/*
udanax-top.st:62770:WorksIniter methodsFor: 'initialization'!
{void} initializeSystem
	|  aConnection {Connection}  adminID {ID} wwd {FeWaitDetector} |
	[Transcript nextPutAll: 'creating connection'; cr; endEntry] smalltalkOnly.
	aConnection := Connection make: FeServer.
	CurrentKeyMaster fluidSet: ((FeServer
		loginByName: (Sequence string: 'System Admin')) cast: BooLock) boo.
	CurrentKeyMaster fluidGet incorporate: FeKeyMaster makePublic.
	adminID := FeServer clubID: (Sequence string: 'System Admin').
	InitialOwner fluidSet: adminID.
	InitialReadClub fluidSet: adminID.
	InitialEditClub fluidSet: adminID.
	InitialSponsor fluidSet: adminID.
	CurrentAuthor fluidSet: adminID.
	self initializeClubs.
	wwd := WorksWaitDetector create: cerr with: 'WorksInit done!!'.
	FeServer waitForWrite: wwd.
	[DiskManager] USES.
	CurrentPacker fluidGet purge.
	[Transcript nextPutAll: 'exiting'; cr; endEntry] smalltalkOnly.
	aConnection destroy!
*/
}
public void execute() {
	initializeSystem();
/*
udanax-top.st:62795:WorksIniter methodsFor: 'execute'!
{void} execute
	self initializeSystem!
*/
}
public WorksIniter(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:62801:WorksIniter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:62804:WorksIniter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public WorksIniter() {
/*

Generated during transformation
*/
}
}
