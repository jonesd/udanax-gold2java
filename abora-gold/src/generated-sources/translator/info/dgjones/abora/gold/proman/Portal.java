/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.SocketPortal;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.Portal;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class Portal extends Heaper {

/*
udanax-top.st:31333:
Heaper subclass: #Portal
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:31337:
(Portal getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:31357:
Portal class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:31360:
(Portal getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Portal.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public XnReadStream readStream() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:31342:Portal methodsFor: 'accessing'!
{XnReadStream} readStream
	
	self subclassResponsibility!
*/
}
public XnWriteStream writeStream() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:31346:Portal methodsFor: 'accessing'!
{XnWriteStream} writeStream
	
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:31352:Portal methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:31354:Portal methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static Portal make(String host, int port) {
	return SocketPortal.make(host, port);
/*
udanax-top.st:31365:Portal class methodsFor: 'pseudo constructors'!
make: host {char star} with: port {UInt32}
	^SocketPortal make: host with: port!
*/
}
public Portal() {
/*

Generated during transformation
*/
}
public Portal(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
