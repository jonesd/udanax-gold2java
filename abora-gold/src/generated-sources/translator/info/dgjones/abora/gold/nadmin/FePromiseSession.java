/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nadmin;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FePromiseSession;
import info.dgjones.abora.gold.nadmin.FeSession;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Represent a single unique connection to the server over some underlying bytestream
 * channel.
 */
public class FePromiseSession extends FeSession {

	protected UInt8Array myPort;
	protected PromiseManager myManager;
	protected Heaper myListener;
	protected FePromiseSession myNext;
/*
udanax-top.st:23454:
FeSession subclass: #FePromiseSession
	instanceVariableNames: '
		myPort {UInt8Array}
		myManager {PromiseManager}
		myListener {Heaper}
		myNext {FePromiseSession | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nadmin'!
*/
/*
udanax-top.st:23462:
FePromiseSession comment:
'Represent a single unique connection to the server over some underlying bytestream channel.'!
*/
/*
udanax-top.st:23464:
(FePromiseSession getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:23510:
FePromiseSession class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:23513:
(FePromiseSession getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FePromiseSession.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential. Terminate this connection.  If withPrejudice is false, it completes the current
 * request and flushes all output before disconnecting.
 */
public void endSession(boolean withPrejudice) {
	if ( ! (withPrejudice)) {
		myManager.force();
	}
	myManager = null;
	myListener.destroy();
	myListener = null;
	if (((Heaper) ((FePromiseSession) CurrentSessions.fluidGet())) == this) {
		CurrentSessions.fluidSet(next());
	}
	else {
		((FePromiseSession) CurrentSessions.fluidGet()).remove(this);
	}
/*
udanax-top.st:23469:FePromiseSession methodsFor: 'accessing'!
{void CLIENT} endSession: withPrejudice {BooleanVar default: false}
	"Essential. Terminate this connection.  If withPrejudice is false, it completes the current request and flushes all output before disconnecting."
	
	withPrejudice ifFalse: [myManager force].
	myManager _ NULL.
	myListener destroy.
	myListener _ NULL.
	(CurrentSessions fluidGet basicCast: Heaper star) == self 
		ifTrue: [CurrentSessions fluidSet: self next]
		ifFalse: [CurrentSessions fluidGet remove: self]!
*/
}
/**
 * Return whether the session has sucessfully logged in.
 */
public boolean isConnected() {
	return myManager != null;
/*
udanax-top.st:23480:FePromiseSession methodsFor: 'accessing'!
{BooleanVar} isConnected
	"Return whether the session has sucessfully logged in."
	
	^myManager ~~ NULL!
*/
}
public FePromiseSession next() {
	return myNext;
/*
udanax-top.st:23485:FePromiseSession methodsFor: 'accessing'!
{FePromiseSession | NULL} next
	^myNext!
*/
}
/**
 * Essential. A system-specific description of the actual transport medium over which the
 * connection is being maintained.
 */
public UInt8Array port() {
	return myPort;
/*
udanax-top.st:23488:FePromiseSession methodsFor: 'accessing'!
{UInt8Array CLIENT} port
	"Essential. A system-specific description of the actual transport medium over which the connection is being maintained."
	
	^myPort!
*/
}
public void remove(FePromiseSession session) {
	if (myNext != null) {
		if (myNext.isEqual(session)) {
			myNext = session.next();
		}
		else {
			myNext.remove(session);
		}
	}
/*
udanax-top.st:23493:FePromiseSession methodsFor: 'accessing'!
{void} remove: session {FePromiseSession}
	myNext ~~ NULL ifTrue:
		[(myNext isEqual: session) 
			ifTrue: [myNext _ session next]
			ifFalse: [myNext remove: session]]!
*/
}
public FePromiseSession(UInt8Array port, Heaper listener, PromiseManager manager) {
	super();
	myPort = port;
	myManager = manager;
	myListener = listener;
	myNext = ((FePromiseSession) CurrentSessions.fluidFetch());
	CurrentSessions.fluidSet(this);
/*
udanax-top.st:23501:FePromiseSession methodsFor: 'creation'!
create: port {UInt8Array} with: listener {Heaper} with: manager {PromiseManager}
	super create.
	myPort _ port.
	myManager _ manager.
	myListener _ listener.
	myNext _ CurrentSessions fluidFetch.
	CurrentSessions fluidSet: self.!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(FePromiseSession.class, "CurrentSessions", DiskManager.emulsion(), null);
/*
udanax-top.st:23518:FePromiseSession class methodsFor: 'smalltalk: init'!
staticTimeNonInherited
	FePromiseSession defineFluid: #CurrentSessions with: DiskManager emulsion with: [NULL].!
*/
}
public static FePromiseSession make(UInt8Array port, Heaper listener, PromiseManager manager) {
	return new FePromiseSession(port, listener, manager);
/*
udanax-top.st:23523:FePromiseSession class methodsFor: 'ceration'!
make: port {UInt8Array} with: listener {Heaper} with: manager {PromiseManager}
	^FePromiseSession create: port with: listener with: manager!
*/
}
public FePromiseSession() {
/*

Generated during transformation
*/
}
public FePromiseSession(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
