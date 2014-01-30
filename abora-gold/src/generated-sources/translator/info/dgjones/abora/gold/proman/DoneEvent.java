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
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.DetectorEvent;
import info.dgjones.abora.gold.proman.DoneEvent;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class DoneEvent extends DetectorEvent {

/*
udanax-top.st:15966:
DetectorEvent subclass: #DoneEvent
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:15970:
(DoneEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:15987:
DoneEvent class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:15990:
(DoneEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DoneEvent.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Send the message across the wire.
 */
public void trigger(PromiseManager pm) {
	pm.sendResponse(PromiseManager.doneResponse());
	pm.sendIntegerVar(detector());
/*
udanax-top.st:15975:DoneEvent methodsFor: 'triggering'!
{void} trigger: pm {PromiseManager}
	"Send the message across the wire."
	
	pm sendResponse: PromiseManager doneResponse.
	pm sendIntegerVar: self detector.!
*/
}
public DoneEvent(int detector) {
	super(detector);
/*
udanax-top.st:15983:DoneEvent methodsFor: 'creation'!
create: detector {IntegerVar}
	super create: detector!
*/
}
public static DetectorEvent make(int detector) {
	return new DoneEvent(detector);
/*
udanax-top.st:15995:DoneEvent class methodsFor: 'creation'!
{DetectorEvent} make: detector {IntegerVar}
	^ self create: detector!
*/
}
public DoneEvent() {
/*

Generated during transformation
*/
}
public DoneEvent(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
