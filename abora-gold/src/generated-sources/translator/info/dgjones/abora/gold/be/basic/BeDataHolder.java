/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.be.basic.BeDataHolder;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeDataHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class BeDataHolder extends BeRangeElement {

	protected PrimValue myValue;
/*
udanax-top.st:2469:
BeRangeElement subclass: #BeDataHolder
	instanceVariableNames: 'myValue {PrimValue}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:2473:
(BeDataHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeDataHolder.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return me wrapped with a session level DataHolder.
 */
public FeRangeElement makeFe(BeLabel label) {
	return FeDataHolder.on(this);
/*
udanax-top.st:2478:BeDataHolder methodsFor: 'accessing'!
{FeRangeElement} makeFe: label {BeLabel | NULL}
	"Return me wrapped with a session level DataHolder."
	
	^FeDataHolder on: self!
*/
}
public PrimValue value() {
	return myValue;
/*
udanax-top.st:2483:BeDataHolder methodsFor: 'accessing'!
{PrimValue} value
	^myValue!
*/
}
public BeDataHolder(PrimValue value) {
	super();
	myValue = value;
	newShepherd();
/*
udanax-top.st:2488:BeDataHolder methodsFor: 'create'!
create: value {PrimValue}
	super create.
	myValue := value.
	self newShepherd!
*/
}
public BeDataHolder(Rcvr receiver) {
	super(receiver);
	myValue = (PrimValue) receiver.receiveHeaper();
/*
udanax-top.st:2496:BeDataHolder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myValue _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myValue);
/*
udanax-top.st:2500:BeDataHolder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myValue.!
*/
}
public BeDataHolder() {
/*

Generated during transformation
*/
}
}
