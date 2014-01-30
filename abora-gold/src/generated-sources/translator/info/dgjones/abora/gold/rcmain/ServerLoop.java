/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.rcmain;

import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.gchooks.RepairEngineer;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.Heaplet;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.rcmain.ServerChunk;
import info.dgjones.abora.gold.rcmain.ServerLoop;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;

/**
 * This is the superclass of all server loops.  There is only one instance of this class in
 * any backend.  Its execute method is the central backend processing loop.  When an instance
 * is created, its creation method must register it with this class.  At that time, all
 * listeners that have been created up to this point will be passed to the new server loop
 * instance.  After this time, new listeners will be passed to the serverloop instance as
 * they register themselves with this class.
 */
public class ServerLoop extends Thunk {

	protected Category myCategory;
	protected MuSet myActiveChunks;
	protected boolean myTerminateFlag;
	protected Stepper myStepper;
	protected static ServerLoop MyServerLoopInstance;
	protected static MuSet TheChunks;
/*
udanax-top.st:57438:
Thunk subclass: #ServerLoop
	instanceVariableNames: '
		myCategory {Category}
		myActiveChunks {MuSet NOCOPY of: ServerChunk}
		myTerminateFlag {BooleanVar NOCOPY}
		myStepper {Stepper NOCOPY}'
	classVariableNames: '
		MyServerLoopInstance {ServerLoop} 
		TheChunks {MuSet of: ServerChunk} '
	poolDictionaries: ''
	category: 'Xanadu-rcmain'!
*/
/*
udanax-top.st:57448:
ServerLoop comment:
'  This is the superclass of all server loops.  There is only one instance of this class in any backend.  Its execute method is the central backend processing loop.  When an instance is created, its creation method must register it with this class.  At that time, all listeners that have been created up to this point will be passed to the new server loop instance.  After this time, new listeners will be passed to the serverloop instance as they register themselves with this class.
'!
*/
/*
udanax-top.st:57451:
(ServerLoop getOrMakeCxxClassDescription)
	friends:
'/- friends for class ServerLoop -/
friend class TerminateTest;
friend void  registerChunk (APTR(ServerChunk) aChunk);
friend void  registerServerLoop (APTR(ServerLoop) aLoop);
friend void  removeChunk (APTR(ServerChunk) aChunk);
friend void  scheduleTermination ();
';
	attributes: ((Set new) add: #(COPY boot ); add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:57551:
ServerLoop class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:57554:
(ServerLoop getOrMakeCxxClassDescription)
	friends:
'/- friends for class ServerLoop -/
friend class TerminateTest;
friend void  registerChunk (APTR(ServerChunk) aChunk);
friend void  registerServerLoop (APTR(ServerLoop) aLoop);
friend void  removeChunk (APTR(ServerChunk) aChunk);
friend void  scheduleTermination ();
';
	attributes: ((Set new) add: #(COPY boot ); add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ServerLoop.class).setAttributes( new Set().add( new String[]
	{"COPY", "boot"}).add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public MuSet activeChunks() {
	return myActiveChunks;
/*
udanax-top.st:57464:ServerLoop methodsFor: 'protected: accessing'!
{MuSet of: ServerChunk} activeChunks
	^myActiveChunks!
*/
}
/**
 * subclasses should call me too
 */
public void deregisterChunk(ServerChunk aChunk) {
	myActiveChunks.wipe(aChunk);
/*
udanax-top.st:57468:ServerLoop methodsFor: 'protected: accessing'!
{void} deregisterChunk: aChunk {ServerChunk}
	"subclasses should call me too"
	
	myActiveChunks wipe: aChunk!
*/
}
/**
 * subclass must call this one first
 */
public void registerChunk(ServerChunk aChunk) {
	if (aChunk.execute()) {
		myActiveChunks.store(aChunk);
	}
/*
udanax-top.st:57473:ServerLoop methodsFor: 'protected: accessing'!
{void} registerChunk: aChunk {ServerChunk}
	"subclass must call this one first"
	aChunk execute ifTrue:
		[myActiveChunks store: aChunk]!
*/
}
public void setTerminate(boolean toBoolean) {
	myTerminateFlag = toBoolean;
/*
udanax-top.st:57479:ServerLoop methodsFor: 'protected: accessing'!
{void} setTerminate: toBoolean {BooleanVar}
	myTerminateFlag _ toBoolean!
*/
}
public Stepper stepper() {
	return myStepper;
/*
udanax-top.st:57483:ServerLoop methodsFor: 'protected: accessing'!
{Stepper} stepper
	^ myStepper!
*/
}
public void stepper(Stepper stepper) {
	myStepper = stepper;
/*
udanax-top.st:57486:ServerLoop methodsFor: 'protected: accessing'!
{void} stepper: stepper {Stepper}
	myStepper := stepper!
*/
}
public boolean terminate() {
	return myTerminateFlag;
/*
udanax-top.st:57489:ServerLoop methodsFor: 'protected: accessing'!
{BooleanVar} terminate
	^ myTerminateFlag!
*/
}
public void execute() {
	ServerLoop deadLoop;
	Object currentServerLoopOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentServerLoop, this);
	try {
		Object currentServerConnectionOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentServerConnection, (Connection.make(myCategory)));
		try {
			((Connection) CurrentServerConnection.fluidGet()).bootHeaper();
			ServerLoop.registerServerLoop(this);
			while ( ! (((ServerLoop) CurrentServerLoop.fluidGet()).terminate())) {
				((ServerLoop) CurrentServerLoop.fluidGet()).stepper(((ServerLoop) CurrentServerLoop.fluidGet()).activeChunks().stepper());
				while (((ServerLoop) CurrentServerLoop.fluidGet()).stepper().hasValue()) {
					ServerChunk chunk;
					chunk = (ServerChunk) ((ServerLoop) CurrentServerLoop.fluidGet()).stepper().fetch();
					if ( ! (chunk.execute())) {
						((ServerLoop) CurrentServerLoop.fluidGet()).activeChunks().remove(chunk);
						if (chunk.shouldDestroy()) {
							chunk.destroy();
						}
					}
					Heaplet.garbageCollect();
					RepairEngineer.repairThings();
					((ServerLoop) CurrentServerLoop.fluidGet()).stepper().step();
				}
				((ServerLoop) CurrentServerLoop.fluidGet()).stepper(null);
				((ServerLoop) CurrentServerLoop.fluidGet()).scheduleChunks();
			}
			Stepper stomper = TheChunks.stepper();
			for (; stomper.hasValue(); stomper.step()) {
				ServerChunk c = (ServerChunk) stomper.fetch();
				if (c == null) {
					continue ;
				}
				c.destroy();
			}
			stomper.destroy();
			deadLoop = ServerLoop.removeServerLoop();
			if ((deadLoop) != ((ServerLoop) CurrentServerLoop.fluidGet())) {
				throw new AboraRuntimeException(AboraRuntimeException.WRONGUSERVERLOOP);
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentServerConnection, currentServerConnectionOldValue);
		}
		((Connection) CurrentServerConnection.fluidGet()).destroy();
		((ServerLoop) CurrentServerLoop.fluidGet()).destroy();
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentServerLoop, currentServerLoopOldValue);
	}
/*
udanax-top.st:57495:ServerLoop methodsFor: 'execution'!
{void} execute
	| deadLoop {ServerLoop} |
	CurrentServerLoop fluidBind: self during: 
		[CurrentServerConnection fluidBind: (Connection make: myCategory) during:
			[CurrentServerConnection fluidGet bootHeaper.
			ServerLoop registerServerLoop: self.
			[CurrentServerLoop fluidGet terminate] whileFalse:
				[CurrentServerLoop fluidGet stepper: CurrentServerLoop fluidGet activeChunks stepper.
				[CurrentServerLoop fluidGet stepper hasValue] whileTrue:
					[ | chunk {ServerChunk} |
					chunk := CurrentServerLoop fluidGet stepper fetch cast: ServerChunk.
					chunk execute ifFalse: [
						CurrentServerLoop fluidGet activeChunks remove: chunk.
						chunk shouldDestroy ifTrue: [chunk destroy]].
					Heaplet garbageCollect.
					RepairEngineer repairThings.
					CurrentServerLoop fluidGet stepper step].
				CurrentServerLoop fluidGet stepper: NULL.
				CurrentServerLoop fluidGet scheduleChunks].
			TheChunks stepper forEach: [:c {ServerChunk} | c destroy].
			deadLoop _ ServerLoop removeServerLoop.
			((deadLoop basicCast: Heaper star) ~= CurrentServerLoop fluidGet) ifTrue: [Heaper BLAST: #WRONG.U.SERVERLOOP]].
		CurrentServerConnection fluidGet destroy.
		CurrentServerLoop fluidGet destroy].!
*/
}
/**
 * Schedule any chunks that have bnecome active.
 */
public void scheduleChunks() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:57520:ServerLoop methodsFor: 'execution'!
{void} scheduleChunks
	"Schedule any chunks that have bnecome active."
	
	self subclassResponsibility!
*/
}
public ServerLoop() {
	super();
	restartServerLoop(null);
/*
udanax-top.st:57527:ServerLoop methodsFor: 'creation'!
create
	super create.
	self restartServerLoop: NULL.!
*/
}
public void restartServerLoop(Rcvr rcvr) {
	myTerminateFlag = false;
	myActiveChunks = MuSet.make();
	myStepper = null;
/*
udanax-top.st:57533:ServerLoop methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartServerLoop: rcvr {Rcvr unused}
	myTerminateFlag _ false.
	myActiveChunks := MuSet make.
	myStepper _ NULL.!
*/
}
public ServerLoop(Rcvr receiver) {
	super(receiver);
	myCategory = (Category) receiver.receiveHeaper();
	restartServerLoop(receiver);
/*
udanax-top.st:57541:ServerLoop methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCategory _ receiver receiveHeaper.
	self restartServerLoop: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCategory);
/*
udanax-top.st:57546:ServerLoop methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCategory.!
*/
}
public static void cleanupGarbage() {
	if (TheChunks == null) {
		return ;
	}
	Stepper stomper = TheChunks.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ServerChunk chunk = (ServerChunk) stomper.fetch();
		if (chunk == null) {
			continue ;
		}
		chunk.destroy();
	}
	stomper.destroy();
	TheChunks.destroy();
	TheChunks = null;
	MyServerLoopInstance = null;
/*
udanax-top.st:57567:ServerLoop class methodsFor: 'smalltalk: init'!
cleanupGarbage
	TheChunks == NULL ifTrue: [^ VOID].
	TheChunks stepper forEach: [:chunk {ServerChunk} | chunk destroy].
	TheChunks destroy.
	TheChunks _ NULL.
	MyServerLoopInstance _ NULL!
*/
}
public static void initTimeNonInherited() {
	TheChunks = MuSet.make();
/*
udanax-top.st:57574:ServerLoop class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	self REQUIRES: MuSet.
	TheChunks _ MuSet make.!
*/
}
public static void linkTimeNonInherited() {
	MyServerLoopInstance = null;
	TheChunks = null;
/*
udanax-top.st:57579:ServerLoop class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	MyServerLoopInstance _ NULL.
	TheChunks _ NULL!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(Connection.class, "CurrentServerConnection", Emulsion.globalEmulsion(), null);
	AboraSupport.defineFluid(ServerLoop.class, "CurrentServerLoop", Emulsion.globalEmulsion(), null);
/*
udanax-top.st:57584:ServerLoop class methodsFor: 'smalltalk: init'!
staticTimeNonInherited
	Connection defineFluid: #CurrentServerConnection with: Emulsion globalEmulsion with: [NULL].
	ServerLoop defineFluid: #CurrentServerLoop with: Emulsion globalEmulsion with: [NULL].!
*/
}
public static MuSet chunks() {
	return TheChunks;
/*
udanax-top.st:57590:ServerLoop class methodsFor: 'protected: accessing'!
{MuSet of: ServerChunk} chunks
	^TheChunks!
*/
}
public static ServerLoop currentServerLoop() {
	return MyServerLoopInstance;
/*
udanax-top.st:57596:ServerLoop class methodsFor: 'accessing'!
{ServerLoop} currentServerLoop
	^ MyServerLoopInstance!
*/
}
public static void introduceChunk(ServerChunk aChunk) {
	TheChunks.introduce(aChunk);
	if (MyServerLoopInstance != null) {
		MyServerLoopInstance.registerChunk(aChunk);
	}
/*
udanax-top.st:57600:ServerLoop class methodsFor: 'accessing'!
{void} introduceChunk: aChunk {ServerChunk} 
	TheChunks introduce: aChunk.
	MyServerLoopInstance ~~ NULL 
		ifTrue: [MyServerLoopInstance registerChunk: aChunk]!
*/
}
public static void registerServerLoop(ServerLoop aLoop) {
	if (MyServerLoopInstance != null) {
		throw new AboraRuntimeException(AboraRuntimeException.CANNOTUHAVEUMULTIPLEUSERVERULOOPS);
	}
	MyServerLoopInstance = aLoop;
	Stepper stomper = TheChunks.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ServerChunk chunk = (ServerChunk) stomper.fetch();
		if (chunk == null) {
			continue ;
		}
		MyServerLoopInstance.registerChunk(chunk);
	}
	stomper.destroy();
/*
udanax-top.st:57605:ServerLoop class methodsFor: 'accessing'!
{void} registerServerLoop: aLoop {ServerLoop}
	MyServerLoopInstance ~~ NULL
		ifTrue: [Heaper BLAST: #CANNOT.U.HAVE.U.MULTIPLE.U.SERVER.U.LOOPS].
	MyServerLoopInstance _ aLoop.
	TheChunks stepper forEach: [:chunk {ServerChunk} |
		MyServerLoopInstance registerChunk: chunk]!
*/
}
public static void removeChunk(ServerChunk aChunk) {
	TheChunks.wipe(aChunk);
	if (MyServerLoopInstance != null) {
		MyServerLoopInstance.deregisterChunk(aChunk);
	}
/*
udanax-top.st:57612:ServerLoop class methodsFor: 'accessing'!
{void} removeChunk: aChunk {ServerChunk} 
 
   	TheChunks wipe: aChunk.
   	MyServerLoopInstance ~~ NULL 
		ifTrue: [MyServerLoopInstance deregisterChunk: aChunk]!
*/
}
public static ServerLoop removeServerLoop() {
	ServerLoop oldLoop;
	if (MyServerLoopInstance == null) {
		throw new AboraRuntimeException(AboraRuntimeException.SERVERLOOPUISUNULL);
	}
	oldLoop = MyServerLoopInstance;
	MyServerLoopInstance = null;
	return oldLoop;
/*
udanax-top.st:57618:ServerLoop class methodsFor: 'accessing'!
{ServerLoop} removeServerLoop
	| oldLoop {ServerLoop} |
	
	(MyServerLoopInstance == NULL)
		ifTrue: [Heaper BLAST: #SERVERLOOP.U.IS.U.NULL.
			^ NULL].
	oldLoop _ MyServerLoopInstance.
	MyServerLoopInstance _ NULL.
	^ oldLoop!
*/
}
public static void scheduleTermination() {
	currentServerLoop().setTerminate(true);
/*
udanax-top.st:57628:ServerLoop class methodsFor: 'accessing'!
{void} scheduleTermination
	self currentServerLoop setTerminate: true!
*/
}
}
