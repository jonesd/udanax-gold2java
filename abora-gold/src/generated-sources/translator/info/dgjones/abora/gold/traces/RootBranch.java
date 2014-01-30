/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.traces;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimIndexTable;
import info.dgjones.abora.gold.traces.BranchDescription;
import info.dgjones.abora.gold.traces.DagWood;
import info.dgjones.abora.gold.traces.RootBranch;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class RootBranch extends BranchDescription {

/*
udanax-top.st:4439:
BranchDescription subclass: #RootBranch
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Traces'!
*/
/*
udanax-top.st:4443:
(RootBranch getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RootBranch.class).setAttributes( new Set().add("COPY").add("SHEPHERDANCESTOR").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The recursion ends here.
 */
public void cacheRecur(PrimIndexTable navCache) {
/*
udanax-top.st:4448:RootBranch methodsFor: 'caching'!
{void} cacheRecur: navCache {PrimIndexTable}
	"The recursion ends here."!
*/
}
public RootBranch(DagWood ft) {
	super(ft);
	newShepherd();
	remember();
/*
udanax-top.st:4453:RootBranch methodsFor: 'create'!
create: ft {DagWood}
	super create: ft.
	self newShepherd.
	self remember!
*/
}
public RootBranch(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:4460:RootBranch methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:4463:RootBranch methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public RootBranch() {
/*

Generated during transformation
*/
}
}
