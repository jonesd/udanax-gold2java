/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nadmin;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.DefaultSession;
import info.dgjones.abora.gold.nadmin.FeSession;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.rcmain.ServerChunk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Represent a single unique connection to the server over some underlying bytestream
 * channel.
 */
public class FeSession extends Heaper {

	protected ID myInitialLogin;
	protected int myConnectTime;
/*
udanax-top.st:23306:
Heaper subclass: #FeSession
	instanceVariableNames: '
		myInitialLogin {ID | NULL}
		myConnectTime {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nadmin'!
*/
/*
udanax-top.st:23312:
FeSession comment:
'Represent a single unique connection to the server over some underlying bytestream channel.'!
*/
/*
udanax-top.st:23314:
(FeSession getOrMakeCxxClassDescription)
	friends:
'friend class Lock;
';
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:23377:
FeSession class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:23380:
(FeSession getOrMakeCxxClassDescription)
	friends:
'friend class Lock;
';
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeSession.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential. The clock time at which the connection was initiated.
 */
public int connectTime() {
	return myConnectTime;
/*
udanax-top.st:23322:FeSession methodsFor: 'accessing'!
{IntegerVar CLIENT} connectTime
	"Essential. The clock time at which the connection was initiated."
	
	^myConnectTime!
*/
}
/**
 * Essential. Terminate this connection.  If withPrejudice is false, it completes the current
 * request and flushes all output before disconnecting.
 */
public void endSession(boolean withPrejudice) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:23327:FeSession methodsFor: 'accessing'!
{void CLIENT} endSession: withPrejudice {BooleanVar default: false}
	"Essential. Terminate this connection.  If withPrejudice is false, it completes the current request and flushes all output before disconnecting."
	
	self subclassResponsibility!
*/
}
/**
 * Essential. The ID of the club that the session logged into to get past the perimeter.
 * Blast of the session is not yet admitted.
 */
public ID initialLogin() {
	if (myInitialLogin == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_LOGGED_IN);
	}
	return myInitialLogin;
/*
udanax-top.st:23332:FeSession methodsFor: 'accessing'!
{ID CLIENT} initialLogin
	"Essential. The ID of the club that the session logged into to get past the perimeter.  Blast of the session is not yet admitted."
	
	myInitialLogin == NULL ifTrue: [Heaper BLAST: #NotLoggedIn].
	^myInitialLogin!
*/
}
/**
 * Return whether the session has sucessfully logged in, and is still logged in.
 */
public boolean isConnected() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:23338:FeSession methodsFor: 'accessing'!
{BooleanVar CLIENT} isConnected
	"Return whether the session has sucessfully logged in, and is still logged in."
	
	self subclassResponsibility!
*/
}
/**
 * Return whether the session has sucessfully logged in.
 */
public boolean isLoggedIn() {
	return myInitialLogin != null;
/*
udanax-top.st:23343:FeSession methodsFor: 'accessing'!
{BooleanVar} isLoggedIn
	"Return whether the session has sucessfully logged in."
	
	^myInitialLogin ~~ NULL!
*/
}
/**
 * Essential. A system-specific description of the actual transport medium over which the
 * connection is being maintained.
 */
public UInt8Array port() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:23348:FeSession methodsFor: 'accessing'!
{UInt8Array CLIENT} port
	"Essential. A system-specific description of the actual transport medium over which the connection is being maintained."
	
	self subclassResponsibility!
*/
}
/**
 * Essential. Gracefully terminate this connection
 */
public void endSession() {
	endSession(false);
/*
udanax-top.st:23355:FeSession methodsFor: 'smalltalk: defaults'!
{void CLIENT} endSession
	"Essential. Gracefully terminate this connection"
	
	self endSession: false!
*/
}
public FeSession() {
	super();
	myInitialLogin = null;
	myConnectTime = FeServer.currentTime();
	CurrentSession.fluidSet(this);
/*
udanax-top.st:23362:FeSession methodsFor: 'creation'!
create
	super create.
	myInitialLogin _ NULL.
	myConnectTime _ FeServer currentTime.
	CurrentSession fluidSet: self!
*/
}
/**
 * Set the ID of the Club which initially logged in during this session
 */
public void setInitialLogin(ID iD) {
	if ( ! (myInitialLogin == null)) {
		throw new AboraAssertionException();
	}
	myInitialLogin = iD;
/*
udanax-top.st:23370:FeSession methodsFor: 'private: accessing'!
{void} setInitialLogin: iD {ID}
	"Set the ID of the Club which initially logged in during this session"
	
	(myInitialLogin == NULL) assert.
	myInitialLogin := iD.!
*/
}
/**
 * {IntegerVar CLIENT} connectTime
 * {void CLIENT} disconnect
 * {IDRegion CLIENT} initialLogins
 * {PrimIntegerArray CLIENT} port
 */
public static void infostProtocol() {
/*
udanax-top.st:23388:FeSession class methodsFor: 'smalltalk: system'!
info.stProtocol
"{IntegerVar CLIENT} connectTime
{void CLIENT} disconnect
{IDRegion CLIENT} initialLogins
{PrimIntegerArray CLIENT} port
"!
*/
}
/**
 * CurrentSessions fluidFetch == NULL
 * ifTrue: [^Stepper itemStepper: CurrentSession fluidGet]
 * ifFalse:
 * [| acc {SetAccumulator} cur {FePromiseSession} |
 * acc _ SetAccumulator make.
 * cur _ CurrentSessions fluidGet.
 * [cur ~~ NULL] whileTrue:
 * [acc step: cur.
 * cur _ cur next].
 * ^(acc value cast: ScruSet) stepper]
 */
public static Stepper allActive() {
	return ImmuSet.make().stepper();
/*
udanax-top.st:23397:FeSession class methodsFor: 'accessing'!
{Stepper of: FeSession} allActive
	"CurrentSessions fluidFetch == NULL
		ifTrue: [^Stepper itemStepper: CurrentSession fluidGet]
		ifFalse:
			[| acc {SetAccumulator} cur {FePromiseSession} |
			acc _ SetAccumulator make.
			cur _ CurrentSessions fluidGet.
			[cur ~~ NULL] whileTrue:
				[acc step: cur.
				cur _ cur next].
			^(acc value cast: ScruSet) stepper]"
	^ ImmuSet make stepper!
*/
}
public static FeSession current() {
	return ((FeSession) CurrentSession.fluidGet());
/*
udanax-top.st:23410:FeSession class methodsFor: 'accessing'!
{FeSession CLIENT} current
	^CurrentSession fluidGet!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(FeSession.class, "CurrentSession", ServerChunk.emulsion(), DefaultSession.make());
/*
udanax-top.st:23415:FeSession class methodsFor: 'smalltalk: init'!
staticTimeNonInherited
	FeSession defineFluid: #CurrentSession with: ServerChunk emulsion with: [DefaultSession make].!
*/
}
public FeSession(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
