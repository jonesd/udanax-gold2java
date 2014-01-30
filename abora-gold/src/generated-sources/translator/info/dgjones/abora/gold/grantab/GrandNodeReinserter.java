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
import info.dgjones.abora.gold.collection.grand.GrandOverflow;
import info.dgjones.abora.gold.grantab.GrandNodeReinserter;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * GrandNodeReinserter moves the contents of the GrandOverflow structure into the newly
 * doubled GrandNode.
 */
public class GrandNodeReinserter extends AgendaItem {

	protected GrandNode myNode;
	protected GrandOverflow myOverflow;
/*
udanax-top.st:553:
AgendaItem subclass: #GrandNodeReinserter
	instanceVariableNames: '
		myNode {GrandNode | NULL}
		myOverflow {GrandOverflow}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-grantab'!
*/
/*
udanax-top.st:559:
GrandNodeReinserter comment:
'GrandNodeReinserter moves the contents of the GrandOverflow structure into the newly doubled GrandNode.'!
*/
/*
udanax-top.st:561:
(GrandNodeReinserter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:597:
GrandNodeReinserter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:600:
(GrandNodeReinserter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandNodeReinserter.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public GrandNodeReinserter(GrandNode gNode, GrandOverflow gOverflow) {
	super();
	myNode = gNode;
	myOverflow = gOverflow;
	myNode.addReinserter();
	newShepherd();
/*
udanax-top.st:566:GrandNodeReinserter methodsFor: 'protected: creation'!
create: gNode {GrandNode} with: gOverflow {GrandOverflow}
	super create.
	myNode _ gNode.
	myOverflow _ gOverflow.
	myNode addReinserter.
	self newShepherd.!
*/
}
public boolean step() {
	if (myNode != null) {
		AboraBlockSupport.enterConsistent(myOverflow.reinsertEntriesConsistency() + 2);
		try {
			myOverflow.reinsertEntries(myNode);
			myNode.removeReinserter();
			myNode = null;
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return false;
/*
udanax-top.st:575:GrandNodeReinserter methodsFor: 'accessing'!
{BooleanVar} step
	myNode ~~ NULL ifTrue:
		[DiskManager consistent: myOverflow reinsertEntriesConsistency + 2 with:
			[myOverflow reinsertEntries: myNode.
			myNode removeReinserter.
			myNode _ NULL.
			self diskUpdate]].
	^ false!
*/
}
public GrandNodeReinserter(Rcvr receiver) {
	super(receiver);
	myNode = (GrandNode) receiver.receiveHeaper();
	myOverflow = (GrandOverflow) receiver.receiveHeaper();
/*
udanax-top.st:586:GrandNodeReinserter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myNode _ receiver receiveHeaper.
	myOverflow _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myNode);
	xmtr.sendHeaper(myOverflow);
/*
udanax-top.st:591:GrandNodeReinserter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myNode.
	xmtr sendHeaper: myOverflow.!
*/
}
public static GrandNodeReinserter make(GrandNode gNode, GrandOverflow gOverflow) {
	AboraBlockSupport.enterConsistent(2);
	try {
		return new GrandNodeReinserter(gNode, gOverflow);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:605:GrandNodeReinserter class methodsFor: 'creation'!
make: gNode {GrandNode} with: gOverflow {GrandOverflow}
	DiskManager consistent: 2 with: [
		^ GrandNodeReinserter create: gNode with: gOverflow]!
*/
}
public GrandNodeReinserter() {
/*

Generated during transformation
*/
}
}
