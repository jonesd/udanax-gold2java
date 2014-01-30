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
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.traces.TreeBranch;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class TreeBranch extends BranchDescription {

	protected TracePosition parent;
/*
udanax-top.st:4466:
BranchDescription subclass: #TreeBranch
	instanceVariableNames: 'parent {TracePosition}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Traces'!
*/
/*
udanax-top.st:4470:
(TreeBranch getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TreeBranch.class).setAttributes( new Set().add("COPY").add("SHEPHERDANCESTOR").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void cacheRecur(PrimIndexTable navCache) {
	parent.cacheIn(navCache);
/*
udanax-top.st:4475:TreeBranch methodsFor: 'caching'!
{void} cacheRecur: navCache {PrimIndexTable}
	parent cacheIn: navCache!
*/
}
public TreeBranch(DagWood ft, TracePosition p) {
	super(ft);
	parent = p;
	newShepherd();
	remember();
/*
udanax-top.st:4480:TreeBranch methodsFor: 'create'!
create: ft {DagWood} with: p {TracePosition}
	super create: ft.
	parent _ p.
	self newShepherd.
	self remember!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ parent.hashForEqual();
/*
udanax-top.st:4488:TreeBranch methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: parent hashForEqual!
*/
}
public TreeBranch(Rcvr receiver) {
	super(receiver);
	parent = (TracePosition) receiver.receiveHeaper();
/*
udanax-top.st:4495:TreeBranch methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	parent _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(parent);
/*
udanax-top.st:4499:TreeBranch methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: parent.!
*/
}
public TreeBranch() {
/*

Generated during transformation
*/
}
}
