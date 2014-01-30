/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.traces;

import info.dgjones.abora.gold.collection.grand.GrandHashTable;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimIndexTable;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.unordered.HeaperAsPosition;
import info.dgjones.abora.gold.spaces.unordered.HeaperSpace;
import info.dgjones.abora.gold.traces.BranchDescription;
import info.dgjones.abora.gold.traces.DagWood;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Each dagwood defines a partial ordering of TracePositions.  Several implementation
 * variables use longs because they represent the size of an in-core array (which can''t get
 * that large).  The variable ''myRoot'' is just for debugging for the moment.
 */
public class DagWood extends Abraham {

	protected TracePosition myRoot;
	protected MuTable myTrunk;
	protected TracePosition myCachedPosition;
	protected PrimIndexTable myNavCache;
/*
udanax-top.st:5897:
Abraham subclass: #DagWood
	instanceVariableNames: '
		myRoot {TracePosition}
		myTrunk {MuTable of: TracePosition and: BranchDescription}
		myCachedPosition {TracePosition NOCOPY}
		myNavCache {PrimIndexTable NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Traces'!
*/
/*
udanax-top.st:5905:
DagWood comment:
'Each dagwood defines a partial ordering of TracePositions.  Several implementation variables use longs because they represent the size of an in-core array (which can''t get that large).  The variable ''myRoot'' is just for debugging for the moment.'!
*/
/*
udanax-top.st:5907:
(DagWood getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DagWood.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public TracePosition root() {
	return myRoot;
/*
udanax-top.st:5912:DagWood methodsFor: 'accessing'!
{TracePosition} root
	^myRoot!
*/
}
/**
 * Return all the successors of the receiver in the trace tree.
 */
public BranchDescription successorBranchOfPosition(BranchDescription branch, int position) {
	throw new UnimplementedException();
/*
udanax-top.st:5915:DagWood methodsFor: 'accessing'!
{BranchDescription} successorBranchOf: branch {BranchDescription unused} position: position {UInt32 unused}
	"Return all the successors of the receiver in the trace tree."
	self unimplemented.
	^NULL!
*/
}
/**
 * Return the first used positions on all the successors of trace.
 */
public MuSet successorsOf(TracePosition trace) {
	BranchDescription prevBranch;
	MuSet set;
	set = MuSet.make();
	prevBranch = (BranchDescription) (myTrunk.fetch((HeaperAsPosition.make(trace))));
	if (prevBranch != null) {
		prevBranch.addSuccessorsTo(set);
	}
	return set;
/*
udanax-top.st:5920:DagWood methodsFor: 'accessing'!
{MuSet} successorsOf: trace {TracePosition} 
	"Return the first used positions on all the successors of trace."
	| prevBranch {BranchDescription} set {MuSet} |
	set _ MuSet make.
	prevBranch _ (myTrunk fetch: (HeaperAsPosition make: trace)) cast: BranchDescription.
	prevBranch ~~ NULL
		ifTrue: [prevBranch addSuccessorsTo: set].
	^set!
*/
}
/**
 * Lookup the anchorTrace to find the branch hanging off it. If there isn't one,
 * then install branch as that branch. Otherwise walk a balanced walk down the
 * binary tree of branches to find a place to hang the new branch.
 */
public void installBranchAfter(BranchDescription branch, TracePosition anchorTrace) {
	BranchDescription prevBranch;
	Position pos;
	prevBranch = (BranchDescription) (myTrunk.fetch((pos = HeaperAsPosition.make(anchorTrace))));
	if (prevBranch == null) {
		myTrunk.introduce(pos, branch);
	}
	else {
		prevBranch.installBranch(branch);
	}
/*
udanax-top.st:5932:DagWood methodsFor: 'branches'!
{void} installBranch: branch {BranchDescription} after: anchorTrace {TracePosition} 
	"Lookup the anchorTrace to find the branch hanging off it. If there isn't one, 
	then install branch as that branch. Otherwise walk a balanced walk down the 
	binary tree of branches to find a place to hang the new branch."
	| prevBranch {BranchDescription} pos {Position} |
	prevBranch _ (myTrunk fetch: (pos _ HeaperAsPosition make: anchorTrace)) cast: BranchDescription.
	prevBranch == NULL
		ifTrue: [myTrunk at: pos introduce: branch]
		ifFalse: [prevBranch installBranch: branch]!
*/
}
/**
 * This should really create a new root, but that's harder to draw!!.
 */
public TracePosition newPosition() {
	return myRoot.newSuccessor();
/*
udanax-top.st:5943:DagWood methodsFor: 'branches'!
{TracePosition} newPosition
	"This should really create a new root, but that's harder to draw!!."
	^myRoot newSuccessor!
*/
}
/**
 * Install the supplied branch and position as the navCache and return it.
 */
public PrimIndexTable cacheTracePos(TracePosition tracePos) {
	if (myCachedPosition != null && (tracePos.isEqual(myCachedPosition))) {
		return myNavCache;
	}
	myCachedPosition = tracePos;
	myNavCache.clearAll();
	tracePos.cacheIn(myNavCache);
	return myNavCache;
/*
udanax-top.st:5950:DagWood methodsFor: 'caching'!
{PrimIndexTable} cacheTracePos: tracePos {TracePosition}
	"Install the supplied branch and position as the navCache and return it. "
	(myCachedPosition ~~ NULL 
		and: [tracePos isEqual: myCachedPosition]) 
		ifTrue: [^myNavCache].
	myCachedPosition _ tracePos.
	myNavCache clearAll.
	tracePos cacheIn: myNavCache.
	^myNavCache!
*/
}
/*
udanax-top.st:5963:DagWood methodsFor: 'smalltalk: inspect'!
{void} inspect
	Sensor leftShiftDown
		ifTrue: [self basicInspect]
		ifFalse: 
			[myRoot inspect]!
*/
public DagWood() {
	super();
	myCachedPosition = null;
	myNavCache = PrimIndexTable.make(128);
	myTrunk = GrandHashTable.make(HeaperSpace.make());
	myRoot = TracePosition.make((BranchDescription.make(this)), 1);
	/* Ensure that no elements get allocated on the root branch. */
	myRoot.newSuccessor();
	newShepherd();
	remember();
/*
udanax-top.st:5971:DagWood methodsFor: 'create'!
create
	super create.
	myCachedPosition _ NULL.
	myNavCache _ PrimIndexTable make: 128.
	myTrunk _ GrandHashTable make: HeaperSpace make.
	myRoot _ TracePosition make: (BranchDescription make: self) with: 1.
	"Ensure that no elements get allocated on the root branch."
	myRoot newSuccessor.
	self newShepherd.
	self remember!
*/
}
/**
 * re-initialize the non-persistent part
 */
public void restartDagWood(Rcvr trans) {
	myCachedPosition = null;
	myNavCache = PrimIndexTable.make(128);
/*
udanax-top.st:5984:DagWood methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartDagWood: trans {Rcvr unused default: NULL}
	"re-initialize the non-persistent part"
	myCachedPosition _ NULL.
	myNavCache _ PrimIndexTable make: 128.!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ myRoot.hashForEqual();
/*
udanax-top.st:5991:DagWood methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: myRoot hashForEqual!
*/
}
public DagWood(Rcvr receiver) {
	super(receiver);
	myRoot = (TracePosition) receiver.receiveHeaper();
	myTrunk = (MuTable) receiver.receiveHeaper();
	restartDagWood(receiver);
/*
udanax-top.st:5998:DagWood methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myRoot _ receiver receiveHeaper.
	myTrunk _ receiver receiveHeaper.
	self restartDagWood: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myRoot);
	xmtr.sendHeaper(myTrunk);
/*
udanax-top.st:6004:DagWood methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myRoot.
	xmtr sendHeaper: myTrunk.!
*/
}
}
