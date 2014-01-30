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
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.PackOBits;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeSession;
import info.dgjones.abora.gold.nkernel.FeAdminer;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.rcmain.ServerLoop;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.TextyXcvrMaker;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A client interface for system administration operations. This object can only be obtained
 * using a KeyMaster that has System Admin authority.
 */
public class FeAdminer extends Heaper {

	protected FeKeyMaster myAdminKM;
/*
udanax-top.st:19045:
Heaper subclass: #FeAdminer
	instanceVariableNames: 'myAdminKM {FeKeyMaster}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:19049:
FeAdminer comment:
'A client interface for system administration operations. This object can only be obtained using a KeyMaster that has System Admin authority. '!
*/
/*
udanax-top.st:19051:
(FeAdminer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #EQ; yourself)!
*/
/*
udanax-top.st:19173:
FeAdminer class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19176:
(FeAdminer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeAdminer.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential. Enable or disable the ability of the Server to accept communications
 * connections from client machines. Anyone who has received a GateKeeper or Server object
 * will continue to stay connected, but no new such objects will be handed out
 */
public void acceptConnections(boolean open) {
	((BeGrandMap) CurrentGrandMap.fluidGet()).acceptConnections(open);
/*
udanax-top.st:19056:FeAdminer methodsFor: 'administrivia'!
{void CLIENT} acceptConnections: open {BooleanVar}
	"Essential. Enable or disable the ability of the Server to accept communications connections from client machines. Anyone who has received a GateKeeper or Server object will continue to stay connected, but no new such objects will be handed out"
	
	CurrentGrandMap fluidGet acceptConnections: open!
*/
}
/**
 * Essential. Return a list of all active sessions.
 */
public Stepper activeSessions() {
	return FeSession.allActive();
/*
udanax-top.st:19061:FeAdminer methodsFor: 'administrivia'!
{Stepper CLIENT of: FeSession} activeSessions
	"Essential. Return a list of all active sessions."
	
	^FeSession allActive!
*/
}
/**
 * Essential. Execute a sequence of server configuration commands.
 */
public void execute(PrimIntArray commands) {
	Rcvr rc;
	Heaper next;
	Someone.knownBug();
	/* only accepts UInt8Arrays */
	rc = TextyXcvrMaker.make().makeRcvr((TransferSpecialist.make((Cookbook.makeString("boot")))), (XnReadStream.make(((UInt8Array) commands))));
	next = rc.receiveHeaper();
	while (next != null) {
		if (next instanceof Thunk) {
			Thunk thunk = (Thunk) next;
			thunk.execute();
		}
		next = rc.receiveHeaper();
	}
	rc.destroy();
/*
udanax-top.st:19066:FeAdminer methodsFor: 'administrivia'!
{void CLIENT} execute: commands {PrimIntArray}
	"Essential. Execute a sequence of server configuration commands."
	
	| rc {Rcvr} next {Heaper | NULL} |
	self knownBug. "only accepts UInt8Arrays"
	rc := TextyXcvrMaker make 
				makeRcvr: (TransferSpecialist make: (Cookbook make.String: 'boot'))
				with: (XnReadStream make: (commands cast: UInt8Array)).
	next := rc receiveHeaper. 
	[next ~~ NULL] whileTrue: 
		[next cast: Thunk into: [:thunk | thunk execute] others: [].
		next := rc receiveHeaper].
	rc destroy!
*/
}
/**
 * Essential. Grant a Club the authority to assign global IDs on this Server.
 */
public void grant(ID clubID, IDRegion globalIDs) {
	((BeGrandMap) CurrentGrandMap.fluidGet()).grant(clubID, globalIDs);
/*
udanax-top.st:19080:FeAdminer methodsFor: 'administrivia'!
{void CLIENT} grant: clubID {ID} with: globalIDs {IDRegion}
	"Essential. Grant a Club the authority to assign global IDs on this Server."
	
	CurrentGrandMap fluidGet grant: clubID with: globalIDs!
*/
}
/**
 * Essential. List who has been granted authority to various regions of the global IDSpace on
 * this Server.
 */
public TableStepper grants(IDRegion clubIDs, IDRegion globalIDs) {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).grants(clubIDs, globalIDs);
/*
udanax-top.st:19085:FeAdminer methodsFor: 'administrivia'!
{TableStepper CLIENT of: ID and: IDRegion} grants: clubIDs {IDRegion default: NULL}
	with: globalIDs {IDRegion default: NULL}
	"Essential. List who has been granted authority to various regions of the global IDSpace on this Server."
	
	^CurrentGrandMap fluidGet grants: clubIDs with: globalIDs!
*/
}
/**
 * Essential. Whether the Server is accepting communications connections from client
 * machines.
 */
public boolean isAcceptingConnections() {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).isAcceptingConnections();
/*
udanax-top.st:19091:FeAdminer methodsFor: 'administrivia'!
{BooleanVar CLIENT} isAcceptingConnections
	"Essential. Whether the Server is accepting communications connections from client machines. "
	
	^CurrentGrandMap fluidGet isAcceptingConnections!
*/
}
/**
 * Essential. Shutdown the Server immediately, taking down all the connections and writing
 * all current changes to disk.
 */
public void shutdown() {
	((DiskManager) CurrentPacker.fluidFetch()).purge();
	ServerLoop.scheduleTermination();
/*
udanax-top.st:19096:FeAdminer methodsFor: 'administrivia'!
{void CLIENT} shutdown
	"Essential. Shutdown the Server immediately, taking down all the connections and writing all current changes to disk."
	
	[DiskManager] USES.
	CurrentPacker fluidFetch purge.
	ServerLoop scheduleTermination.!
*/
}
/**
 * @deprecated
 */
public void clearProfile() {
	throw new PasseException();
/*
udanax-top.st:19105:FeAdminer methodsFor: 'smalltalk: passe'!
{void} clearProfile
	self passe "rc file"!
*/
}
/**
 * @deprecated
 */
public void consistencyCheck() {
	throw new PasseException();
/*
udanax-top.st:19109:FeAdminer methodsFor: 'smalltalk: passe'!
{void} consistencyCheck
	self passe "rc file"!
*/
}
/**
 * @deprecated
 */
public FeLockSmith defaultLockSmith() {
	throw new PasseException();
/*
udanax-top.st:19113:FeAdminer methodsFor: 'smalltalk: passe'!
{FeLockSmith} defaultLockSmith
	self passe!
*/
}
/**
 * Disable login access to a Club, by revoking its direct membership of the System Access
 * Club
 * @deprecated
 */
public void disableAccess(ID clubID) {
	throw new PasseException();
/*
udanax-top.st:19117:FeAdminer methodsFor: 'smalltalk: passe'!
{void} disableAccess: clubID {ID}
	"Disable login access to a Club, by revoking its direct membership of the System Access Club"
	
	self passe. "see FeServer"!
*/
}
/**
 * @deprecated
 */
public void enableAccess(ID clubID) {
	throw new PasseException();
/*
udanax-top.st:19122:FeAdminer methodsFor: 'smalltalk: passe'!
{void} enableAccess: clubID {ID}
	self passe. "see FeServer"!
*/
}
/**
 * @deprecated
 */
public void nameClub(Sequence name, ID clubID) {
	throw new PasseException();
/*
udanax-top.st:19125:FeAdminer methodsFor: 'smalltalk: passe'!
{void} nameClub: name {Sequence} with: clubID {ID}
	self passe. "see FeServer"!
*/
}
/**
 * @deprecated
 */
public void renameClub(PackOBits oldName, PackOBits newName) {
	throw new PasseException();
/*
udanax-top.st:19128:FeAdminer methodsFor: 'smalltalk: passe'!
{void} renameClub: oldName {PackOBits} with: newName {PackOBits}
	self passe. "see FeServer"!
*/
}
/**
 * @deprecated
 */
public void setDefaultLockSmith(FeLockSmith lockSmith) {
	throw new PasseException();
/*
udanax-top.st:19131:FeAdminer methodsFor: 'smalltalk: passe'!
{void} setDefaultLockSmith: lockSmith {FeLockSmith}
	self passe!
*/
}
/**
 * @deprecated
 */
public void shutDown() {
	throw new PasseException();
/*
udanax-top.st:19135:FeAdminer methodsFor: 'smalltalk: passe'!
{void} shutDown
	self passe "shutdown"!
*/
}
/**
 * @deprecated
 */
public void unnameClub(PackOBits name) {
	throw new PasseException();
/*
udanax-top.st:19139:FeAdminer methodsFor: 'smalltalk: passe'!
{void} unnameClub: name {PackOBits}
	self passe. "see FeServer"!
*/
}
/**
 * @deprecated
 */
public void writeProfile() {
	throw new PasseException();
/*
udanax-top.st:19142:FeAdminer methodsFor: 'smalltalk: passe'!
{void} writeProfile
	self passe "rc file"!
*/
}
/**
 * Essential. The LockSmith which hands out locks when a client tries to login through the
 * GateKeeper with an invalid Club ID or name.
 */
public FeLockSmith gateLockSmith() {
	return (FeLockSmith) (FeLockSmith.spec().wrap(((BeGrandMap) CurrentGrandMap.fluidGet()).gateLockSmithEdition()));
/*
udanax-top.st:19148:FeAdminer methodsFor: 'security'!
{FeLockSmith CLIENT} gateLockSmith
	"Essential. The LockSmith which hands out locks when a client tries to login through the GateKeeper with an invalid Club ID or name."
	[BeGrandMap] USES.
	^(FeLockSmith spec wrap: CurrentGrandMap fluidGet gateLockSmithEdition) cast: FeLockSmith!
*/
}
/**
 * Essential. Set the LockSmith which creates locks to hand out when a client tries to login
 * with an invalid Club ID or name through the GateKeeper.
 */
public void setGateLockSmith(FeLockSmith lockSmith) {
	((BeGrandMap) CurrentGrandMap.fluidFetch()).setGateLockSmithEdition(lockSmith.edition());
/*
udanax-top.st:19153:FeAdminer methodsFor: 'security'!
{void CLIENT} setGateLockSmith: lockSmith {FeLockSmith}
	"Essential. Set the LockSmith which creates locks to hand out when a client tries to login with an invalid Club ID or name through the GateKeeper."
	[BeGrandMap] USES.
	CurrentGrandMap fluidFetch setGateLockSmithEdition: lockSmith edition!
*/
}
public TableStepper grants() {
	return grants(null, null);
/*
udanax-top.st:19160:FeAdminer methodsFor: 'smalltalk: defaults'!
{TableStepper CLIENT of: ID and: IDRegion} grants
	^self grants: NULL with: NULL!
*/
}
public TableStepper grants(IDRegion clubIDs) {
	return grants(clubIDs, null);
/*
udanax-top.st:19163:FeAdminer methodsFor: 'smalltalk: defaults'!
{TableStepper CLIENT of: ID and: IDRegion} grants: clubIDs {IDRegion default: NULL}
	^self grants: clubIDs with: NULL!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:19168:FeAdminer methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:19170:FeAdminer methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static FeAdminer make() {
	FeKeyMaster.assertAdminAuthority();
	return new FeAdminer();
/*
udanax-top.st:19181:FeAdminer class methodsFor: 'create'!
{FeAdminer CLIENT} make
	FeKeyMaster assertAdminAuthority.
	^self create!
*/
}
/**
 * {void CLIENT} acceptConnections: open {BooleanVar}
 * {Stepper CLIENT of: FeSession} activeSessions
 * {void CLIENT} execute: commands {PrimIntegerArray}
 * {FeLockSmith CLIENT} gateLockSmith
 * {void CLIENT} grant: clubID {ID} with: globalIDs {IDRegion}
 * {TableStepper CLIENT of: ID and: IDRegion} grants
 * {TableStepper CLIENT of: ID and: IDRegion} grants: clubIDs {IDRegion default: NULL}
 * {TableStepper CLIENT of: ID and: IDRegion} grants: clubIDs {IDRegion default: NULL} with:
 * globalIDs {IDRegion default: NULL}
 * {BooleanVar CLIENT} isAcceptingConnections
 * {void CLIENT} setGateLockSmith: lockSmith {FeLockSmith}
 * {void CLIENT} shutDown
 */
public static void infostProtocol() {
/*
udanax-top.st:19188:FeAdminer class methodsFor: 'smalltalk: system'!
info.stProtocol
"{void CLIENT} acceptConnections: open {BooleanVar}
{Stepper CLIENT of: FeSession} activeSessions
{void CLIENT} execute: commands {PrimIntegerArray}
{FeLockSmith CLIENT} gateLockSmith
{void CLIENT} grant: clubID {ID} with: globalIDs {IDRegion}
{TableStepper CLIENT of: ID and: IDRegion} grants
{TableStepper CLIENT of: ID and: IDRegion} grants: clubIDs {IDRegion default: NULL}
{TableStepper CLIENT of: ID and: IDRegion} grants: clubIDs {IDRegion default: NULL} with: globalIDs {IDRegion default: NULL}
{BooleanVar CLIENT} isAcceptingConnections
{void CLIENT} setGateLockSmith: lockSmith {FeLockSmith}
{void CLIENT} shutDown
"!
*/
}
public FeAdminer() {
/*

Generated during transformation
*/
}
public FeAdminer(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
