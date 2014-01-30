/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.canopy.AbstractRecorderFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SimpleRecorderFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.RecorderTrigger;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * A finder which holds onto a RangeElement and looks for ResultRecorders which might want to
 * record it NOT.A.TYPE
 */
public class SimpleRecorderFinder extends AbstractRecorderFinder {

	protected BeRangeElement myRangeElement;
/*
udanax-top.st:40377:
AbstractRecorderFinder subclass: #SimpleRecorderFinder
	instanceVariableNames: 'myRangeElement {BeRangeElement}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:40381:
SimpleRecorderFinder comment:
'A finder which holds onto a RangeElement and looks for ResultRecorders which might want to record it NOT.A.TYPE '!
*/
/*
udanax-top.st:40383:
(SimpleRecorderFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SimpleRecorderFinder.class).setAttributes( new Set().add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public PropFinder findPast(BeEdition edition) {
	return this;
/*
udanax-top.st:40388:SimpleRecorderFinder methodsFor: 'accessing'!
{PropFinder} findPast: edition {BeEdition unused}
	
	^self!
*/
}
public boolean match(Prop prop) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:40392:SimpleRecorderFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	self subclassResponsibility!
*/
}
public void checkRecorder(ResultRecorder recorder, RecorderFossil fossil) {
	if ((recorder.accepts(rangeElement())) && (shouldTrigger(recorder, fossil))) {
		(RecorderTrigger.make(fossil, myRangeElement)).schedule();
	}
/*
udanax-top.st:40398:SimpleRecorderFinder methodsFor: 'recording'!
{void} checkRecorder: recorder {ResultRecorder}
	with: fossil {RecorderFossil}
	((recorder accepts: self rangeElement) and: [self shouldTrigger: recorder with: fossil]) ifTrue:
		[(RecorderTrigger make: fossil with: myRangeElement) schedule]!
*/
}
/**
 * Whether the recorder should be triggered with my RangeElement
 */
public boolean shouldTrigger(ResultRecorder recorder, RecorderFossil fossil) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:40404:SimpleRecorderFinder methodsFor: 'recording'!
{BooleanVar} shouldTrigger: recorder {ResultRecorder}
	with: fossil {RecorderFossil}
	"Whether the recorder should be triggered with my RangeElement"
	
	self subclassResponsibility!
*/
}
public SimpleRecorderFinder() {
	super();
	/* for generated code */
/*
udanax-top.st:40412:SimpleRecorderFinder methodsFor: 'create'!
create
	super create "for generated code"!
*/
}
public SimpleRecorderFinder(int flags, BeRangeElement element) {
	super(flags);
	myRangeElement = element;
/*
udanax-top.st:40415:SimpleRecorderFinder methodsFor: 'create'!
create: flags {UInt32}
	with: element {BeRangeElement}
	super create: flags.
	myRangeElement := element.!
*/
}
public BeEdition edition() {
	return (BeEdition) myRangeElement;
/*
udanax-top.st:40423:SimpleRecorderFinder methodsFor: 'protected:'!
{BeEdition} edition
	^myRangeElement cast: BeEdition!
*/
}
public BeRangeElement rangeElement() {
	return myRangeElement;
/*
udanax-top.st:40427:SimpleRecorderFinder methodsFor: 'protected:'!
{BeRangeElement} rangeElement
	^myRangeElement!
*/
}
public BeWork work() {
	return (BeWork) myRangeElement;
/*
udanax-top.st:40431:SimpleRecorderFinder methodsFor: 'protected:'!
{BeWork} work
	^myRangeElement cast: BeWork!
*/
}
public PropFinder oldPass(PropJoint parent) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:40437:SimpleRecorderFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	self subclassResponsibility!
*/
}
public SimpleRecorderFinder(Rcvr receiver) {
	super(receiver);
	myRangeElement = (BeRangeElement) receiver.receiveHeaper();
/*
udanax-top.st:40443:SimpleRecorderFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myRangeElement _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myRangeElement);
/*
udanax-top.st:40447:SimpleRecorderFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myRangeElement.!
*/
}
}
