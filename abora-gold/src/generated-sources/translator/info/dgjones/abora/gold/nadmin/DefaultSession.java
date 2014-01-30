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
import info.dgjones.abora.gold.nadmin.DefaultSession;
import info.dgjones.abora.gold.nadmin.FeSession;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * The default session.
 */
public class DefaultSession extends FeSession {

/*
udanax-top.st:23418:
FeSession subclass: #DefaultSession
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nadmin'!
*/
/*
udanax-top.st:23422:
DefaultSession comment:
'The default session.'!
*/
/*
udanax-top.st:23424:
(DefaultSession getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:23443:
DefaultSession class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:23446:
(DefaultSession getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DefaultSession.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Do nothing
 */
public void endSession(boolean withPrejudice) {
/*
udanax-top.st:23429:DefaultSession methodsFor: 'accessing'!
{void CLIENT} endSession: withPrejudice {BooleanVar default: false}
	"Do nothing"!
*/
}
/**
 * Return whether the session has sucessfully logged in.
 */
public boolean isConnected() {
	return true;
/*
udanax-top.st:23432:DefaultSession methodsFor: 'accessing'!
{BooleanVar} isConnected
	"Return whether the session has sucessfully logged in."
	
	^true!
*/
}
/**
 * Essential. A system-specific description of the actual transport medium over which the
 * connection is being maintained.
 */
public UInt8Array port() {
	return UInt8Array.string("default");
/*
udanax-top.st:23437:DefaultSession methodsFor: 'accessing'!
{UInt8Array CLIENT} port
	"Essential. A system-specific description of the actual transport medium over which the connection is being maintained."
	
	^UInt8Array string: 'default'!
*/
}
public static FeSession make() {
	return new DefaultSession();
/*
udanax-top.st:23451:DefaultSession class methodsFor: 'creation'!
{FeSession} make
	^self create!
*/
}
public DefaultSession() {
/*

Generated during transformation
*/
}
public DefaultSession(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
