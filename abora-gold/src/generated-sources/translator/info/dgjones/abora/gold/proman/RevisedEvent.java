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
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RevisedEvent;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class RevisedEvent extends DetectorEvent {

	protected Heaper myWork;
	protected Heaper myContents;
	protected Heaper myAuthor;
	protected int myTime;
	protected int mySequence;
/*
udanax-top.st:16147:
DetectorEvent subclass: #RevisedEvent
	instanceVariableNames: '
		myWork {Heaper}
		myContents {Heaper}
		myAuthor {Heaper}
		myTime {IntegerVar}
		mySequence {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:16156:
(RevisedEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:16191:
RevisedEvent class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:16194:
(RevisedEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RevisedEvent.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public RevisedEvent(int detector, Heaper work, Heaper contents, Heaper author, int time, int sequence) {
	super(detector);
	myWork = work;
	myContents = contents;
	myAuthor = author;
	myTime = time;
	mySequence = sequence;
/*
udanax-top.st:16161:RevisedEvent methodsFor: 'creation'!
create: detector {IntegerVar} 
	with: work {Heaper}
	with: contents {Heaper}
	with: author {Heaper}
	with: time {IntegerVar}
	with: sequence {IntegerVar}
	
	super create: detector.
	myWork _ work.
	myContents _ contents.
	myAuthor _ author.
	myTime _ time.
	mySequence _ sequence!
*/
}
/**
 * Send the message across the wire.
 */
public void trigger(PromiseManager pm) {
	pm.sendResponse(PromiseManager.revisedResponse());
	pm.sendIntegerVar(detector());
	pm.sendPromise(myWork);
	pm.sendPromise(myContents);
	pm.sendPromise(myAuthor);
	pm.sendIntegerVar(myTime);
	pm.sendPromise((PrimIntValue.make(myTime)));
	pm.sendIntegerVar(mySequence);
	pm.sendPromise((PrimIntValue.make(mySequence)));
/*
udanax-top.st:16177:RevisedEvent methodsFor: 'triggering'!
{void} trigger: pm {PromiseManager}
	"Send the message across the wire."
	
	pm sendResponse: PromiseManager revisedResponse.
	pm sendIntegerVar: self detector.
	pm sendPromise: myWork.
	pm sendPromise: myContents.
	pm sendPromise: myAuthor.
	pm sendIntegerVar: myTime.
	pm sendPromise: (PrimIntValue make: myTime).
	pm sendIntegerVar: mySequence.
	pm sendPromise: (PrimIntValue make: mySequence)!
*/
}
public static DetectorEvent make(int detector, Heaper work, Heaper contents, Heaper author, int time, int sequence) {
	return new RevisedEvent(detector, work, contents, author, time, sequence);
/*
udanax-top.st:16199:RevisedEvent class methodsFor: 'creation'!
{DetectorEvent} make: detector {IntegerVar} 
	with: work {Heaper}
	with: contents {Heaper}
	with: author {Heaper}
	with: time {IntegerVar}
	with: sequence {IntegerVar}
	
	^ self create: detector 
		with: work
		with: contents
		with: author
		with: time
		with: sequence!
*/
}
public RevisedEvent() {
/*

Generated during transformation
*/
}
public RevisedEvent(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
