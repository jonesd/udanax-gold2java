/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.detect;

import info.dgjones.abora.gold.detect.FeDetector;
import info.dgjones.abora.gold.detect.FeWaitDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Will get sent a single message, once, with no parameters, when something happens. It can
 * be passed in to Server::waitForConsequences and Server::waitForWrite.BY.PROXY
 */
public class FeWaitDetector extends FeDetector {

/*
udanax-top.st:19919:
FeDetector subclass: #FeWaitDetector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-detect'!
*/
/*
udanax-top.st:19923:
FeWaitDetector comment:
'Will get sent a single message, once, with no parameters, when something happens. It can be passed in to Server::waitForConsequences and Server::waitForWrite.BY.PROXY '!
*/
/*
udanax-top.st:19925:
(FeWaitDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:19936:
FeWaitDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19939:
(FeWaitDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeWaitDetector.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential.  Whatever I was waiting for has happened
 */
public void done() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:19930:FeWaitDetector methodsFor: 'triggering'!
{void CLIENT} done
	"Essential.  Whatever I was waiting for has happened"
	
	self subclassResponsibility!
*/
}
/**
 * {NOWAIT CLIENT} done
 */
public static void infostProtocol() {
/*
udanax-top.st:19944:FeWaitDetector class methodsFor: 'smalltalk: system'!
info.stProtocol
"{NOWAIT CLIENT} done
"!
*/
}
public FeWaitDetector() {
/*

Generated during transformation
*/
}
public FeWaitDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
