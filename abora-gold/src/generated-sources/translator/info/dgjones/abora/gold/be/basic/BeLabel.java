/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeLabel;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class BeLabel extends BeRangeElement {

/*
udanax-top.st:3391:
BeRangeElement subclass: #BeLabel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:3395:
(BeLabel getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeLabel.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public FeRangeElement makeFe(BeLabel label) {
	return FeLabel.on(this);
/*
udanax-top.st:3400:BeLabel methodsFor: 'accessing'!
{FeRangeElement} makeFe: label {BeLabel | NULL}
	^FeLabel on: self!
*/
}
public BeLabel() {
	super();
	newShepherd();
	Someone.hack();
	/* Labels don't know when they're pointed to as labels instead of range elements, so just remember them. */
	remember();
/*
udanax-top.st:3405:BeLabel methodsFor: 'creation'!
create
	super create.
	self newShepherd.
	self hack.  "Labels don't know when they're pointed to as labels instead of range elements, so just remember them."
	self remember!
*/
}
public BeLabel(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:3413:BeLabel methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:3416:BeLabel methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
}
