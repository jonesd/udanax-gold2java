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
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.DetectorEvent;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * The detectors for comm create these and queue them up because they can only go out between
 * requests.
 */
public class DetectorEvent extends Heaper {

	protected DetectorEvent myNext;
	protected int myDetector;
/*
udanax-top.st:15925:
Heaper subclass: #DetectorEvent
	instanceVariableNames: '
		myNext {DetectorEvent}
		myDetector {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:15931:
DetectorEvent comment:
'The detectors for comm create these and queue them up because they can only go out between requests.'!
*/
/*
udanax-top.st:15933:
(DetectorEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DetectorEvent.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public int detector() {
	return myDetector;
/*
udanax-top.st:15938:DetectorEvent methodsFor: 'accessing'!
{IntegerVar} detector
	^myDetector!
*/
}
public DetectorEvent next() {
	return myNext;
/*
udanax-top.st:15941:DetectorEvent methodsFor: 'accessing'!
{DetectorEvent} next
	^myNext!
*/
}
public void setNext(DetectorEvent event) {
	myNext = event;
/*
udanax-top.st:15944:DetectorEvent methodsFor: 'accessing'!
{void} setNext: event {DetectorEvent}
	myNext _ event!
*/
}
/**
 * Send the message across the wire.
 */
public void trigger(PromiseManager pm) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:15949:DetectorEvent methodsFor: 'triggering'!
{void} trigger: pm {PromiseManager}
	"Send the message across the wire."
	
	self subclassResponsibility!
*/
}
public DetectorEvent(int detector) {
	super();
	myDetector = detector;
	myNext = null;
/*
udanax-top.st:15956:DetectorEvent methodsFor: 'creation'!
create: detector {IntegerVar}
	super create.
	myDetector _ detector.
	myNext _ NULL!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:15963:DetectorEvent methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public DetectorEvent() {
/*

Generated during transformation
*/
}
public DetectorEvent(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
