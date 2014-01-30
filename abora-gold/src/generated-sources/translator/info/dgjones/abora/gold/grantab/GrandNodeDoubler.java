/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.grantab;

import info.dgjones.abora.gold.collection.grand.GrandNode;
import info.dgjones.abora.gold.grantab.GrandNodeDoubler;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * GrandNodeDoubler performs the page splitting required for the extensible
 * GrandHash<collection>s in a deferred fashion.
 */
public class GrandNodeDoubler extends AgendaItem {

	protected GrandNode myNode;
/*
udanax-top.st:504:
AgendaItem subclass: #GrandNodeDoubler
	instanceVariableNames: 'myNode {GrandNode | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-grantab'!
*/
/*
udanax-top.st:508:
GrandNodeDoubler comment:
'GrandNodeDoubler performs the page splitting required for the extensible GrandHash<collection>s in a deferred fashion.'!
*/
/*
udanax-top.st:510:
(GrandNodeDoubler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:541:
GrandNodeDoubler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:544:
(GrandNodeDoubler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandNodeDoubler.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public GrandNodeDoubler(GrandNode gNode) {
	super();
	myNode = gNode;
	newShepherd();
/*
udanax-top.st:515:GrandNodeDoubler methodsFor: 'protected: creation'!
create: gNode {GrandNode}
	super create.
	myNode _ gNode.
	self newShepherd.!
*/
}
public boolean step() {
	if (myNode != null) {
		AboraBlockSupport.enterConsistent(myNode.doubleNodeConsistency() + 2);
		try {
			myNode.doubleNode();
			myNode = null;
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return false;
/*
udanax-top.st:522:GrandNodeDoubler methodsFor: 'accessing'!
{BooleanVar} step
	myNode ~~ NULL ifTrue:
		[DiskManager consistent: myNode doubleNodeConsistency + 2 with:
			[myNode doubleNode.
			myNode _ NULL.
			self diskUpdate]].
	^ false!
*/
}
public GrandNodeDoubler(Rcvr receiver) {
	super(receiver);
	myNode = (GrandNode) receiver.receiveHeaper();
/*
udanax-top.st:532:GrandNodeDoubler methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myNode _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myNode);
/*
udanax-top.st:536:GrandNodeDoubler methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myNode.!
*/
}
public static GrandNodeDoubler make(GrandNode gNode) {
	AboraBlockSupport.enterConsistent(1);
	try {
		return new GrandNodeDoubler(gNode);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:549:GrandNodeDoubler class methodsFor: 'creation'!
make: gNode {GrandNode}
	DiskManager consistent: 1 with: [
		^ GrandNodeDoubler create: gNode]!
*/
}
public GrandNodeDoubler() {
/*

Generated during transformation
*/
}
}
