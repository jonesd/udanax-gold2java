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
import info.dgjones.abora.gold.traces.DagBranch;
import info.dgjones.abora.gold.traces.DagWood;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class DagBranch extends BranchDescription {

	protected TracePosition parent1;
	protected TracePosition parent2;
/*
udanax-top.st:4395:
BranchDescription subclass: #DagBranch
	instanceVariableNames: '
		parent1 {TracePosition}
		parent2 {TracePosition}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Traces'!
*/
/*
udanax-top.st:4401:
(DagBranch getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DagBranch.class).setAttributes( new Set().add("COPY").add("SHEPHERDANCESTOR").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void cacheRecur(PrimIndexTable navCache) {
	parent1.cacheIn(navCache);
	parent2.cacheIn(navCache);
/*
udanax-top.st:4406:DagBranch methodsFor: 'caching'!
{void} cacheRecur: navCache {PrimIndexTable}
	parent1 cacheIn: navCache.
	parent2 cacheIn: navCache!
*/
}
public DagBranch(DagWood ft, TracePosition p1, TracePosition p2) {
	super(ft);
	parent1 = p1;
	parent2 = p2;
	newShepherd();
	remember();
/*
udanax-top.st:4412:DagBranch methodsFor: 'create'!
create: ft {DagWood}with: p1 {TracePosition} with: p2 {TracePosition}
	super create: ft.
	parent1 _ p1.
	parent2 _ p2.
	self newShepherd.
	self remember!
*/
}
public int contentsHash() {
	return (super.contentsHash() ^ parent1.hashForEqual()) ^ parent2.hashForEqual();
/*
udanax-top.st:4421:DagBranch methodsFor: 'testing'!
{UInt32} contentsHash
	^(super contentsHash
		bitXor: parent1 hashForEqual)
		bitXor: parent2 hashForEqual!
*/
}
public DagBranch(Rcvr receiver) {
	super(receiver);
	parent1 = (TracePosition) receiver.receiveHeaper();
	parent2 = (TracePosition) receiver.receiveHeaper();
/*
udanax-top.st:4429:DagBranch methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	parent1 _ receiver receiveHeaper.
	parent2 _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(parent1);
	xmtr.sendHeaper(parent2);
/*
udanax-top.st:4434:DagBranch methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: parent1.
	xmtr sendHeaper: parent2.!
*/
}
public DagBranch() {
/*

Generated during transformation
*/
}
}
