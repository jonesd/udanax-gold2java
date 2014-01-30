/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.be.basic.BeIDHolder;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeIDHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class BeIDHolder extends BeRangeElement {

	protected ID myID;
/*
udanax-top.st:3340:
BeRangeElement subclass: #BeIDHolder
	instanceVariableNames: 'myID {ID}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:3344:
(BeIDHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:3380:
BeIDHolder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:3383:
(BeIDHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeIDHolder.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public ID iD() {
	return myID;
/*
udanax-top.st:3349:BeIDHolder methodsFor: 'accessing'!
{ID} iD
	^myID!
*/
}
public FeRangeElement makeFe(BeLabel label) {
	return FeIDHolder.on(this);
/*
udanax-top.st:3352:BeIDHolder methodsFor: 'accessing'!
{FeRangeElement} makeFe: label {BeLabel | NULL}
	^FeIDHolder on: self!
*/
}
/**
 * Does this need to clear the GrandMap table?
 */
public void dismantle() {
	throw new UnimplementedException();
/*
udanax-top.st:3357:BeIDHolder methodsFor: 'protected: dismantle'!
{void} dismantle
	"Does this need to clear the GrandMap table?"
	
	self unimplemented!
*/
}
public BeIDHolder(ID iD) {
	super();
	myID = iD;
	newShepherd();
/*
udanax-top.st:3364:BeIDHolder methodsFor: 'protected: creation'!
create: iD {ID}
	super create.
	myID _ iD.
	self newShepherd!
*/
}
public BeIDHolder(Rcvr receiver) {
	super(receiver);
	myID = (ID) receiver.receiveHeaper();
/*
udanax-top.st:3371:BeIDHolder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myID _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myID);
/*
udanax-top.st:3375:BeIDHolder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myID.!
*/
}
public static BeIDHolder make(ID iD) {
	return new BeIDHolder(iD);
/*
udanax-top.st:3388:BeIDHolder class methodsFor: 'creation'!
make: iD {ID}
	^ self create: iD!
*/
}
public BeIDHolder() {
/*

Generated during transformation
*/
}
}
