/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.traces;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimIndexTable;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.traces.BoundedTrace;
import info.dgjones.abora.gold.traces.BranchDescription;
import info.dgjones.abora.gold.traces.DagBranch;
import info.dgjones.abora.gold.traces.DagWood;
import info.dgjones.abora.gold.traces.RootBranch;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.traces.TreeBranch;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

/**
 * Instances of subclasses describe the different kinds of paths in a traceDag.  The
 * three kinds are root (no parent), tree (one parent) and dag (two parent) branches.
 * The dag caching routine chases up the dag finding the max of all paths.  The special
 * case of chasing up the hierarchy is probably not worth the code.
 * At the moment, these never go away!!!!!!
 */
public class BranchDescription extends Abraham {

	protected int lastPosition;
	protected BranchDescription myLeft;
	protected BranchDescription myRight;
	protected DagWood fulltrace;
/*
udanax-top.st:4234:
Abraham subclass: #BranchDescription
	instanceVariableNames: '
		lastPosition {UInt32}
		myLeft {BranchDescription}
		myRight {BranchDescription}
		fulltrace {DagWood}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Traces'!
*/
/*
udanax-top.st:4242:
BranchDescription comment:
'Instances of subclasses describe the different kinds of paths in a traceDag.  The 
three kinds are root (no parent), tree (one parent) and dag (two parent) branches.  
The dag caching routine chases up the dag finding the max of all paths.  The special 
case of chasing up the hierarchy is probably not worth the code.
At the moment, these never go away!!!!!!'!
*/
/*
udanax-top.st:4249:
(BranchDescription getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
/*
udanax-top.st:4378:
BranchDescription class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:4381:
(BranchDescription getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BranchDescription.class).setAttributes( new Set().add("DEFERRED").add("COPY").add("SHEPHERDPATRIARCH").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
public int contentsHash() {
	return ((super.contentsHash() ^ myLeft.hashForEqual()) ^ myRight.hashForEqual()) ^ fulltrace.hashForEqual();
/*
udanax-top.st:4254:BranchDescription methodsFor: 'testing'!
{UInt32} contentsHash
	^((super contentsHash
		bitXor: myLeft hashForEqual)
		bitXor: myRight hashForEqual)
		bitXor: fulltrace hashForEqual!
*/
}
public boolean doesInclude(int position, TracePosition tracePos) {
	int mark;
	mark = (fulltrace.cacheTracePos(tracePos)).fetch(this);
	return mark != 0 && ((position) <= mark);
/*
udanax-top.st:4261:BranchDescription methodsFor: 'testing'!
{BooleanVar} does: position {UInt32} include: tracePos {TracePosition} 
	| mark {IntegerVar} |
	[PrimIndexTable] USES.
	mark _ (fulltrace cacheTracePos: tracePos) fetch: self.
	^mark ~~ NULL and: [(Integer IntegerVar: position) <= mark]!
*/
}
/**
 * recur toward the root filling in the cache.
 */
public void cacheRecur(PrimIndexTable navCache) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:4269:BranchDescription methodsFor: 'deferred accessing'!
{void} cacheRecur: navCache {PrimIndexTable}
	"recur toward the root filling in the cache."
	self subclassResponsibility!
*/
}
/**
 * Add the first useable positions for all successor branches to the set.
 */
public void addSuccessorsTo(MuSet set) {
	set.store((TracePosition.make(this, 3)));
	if (myLeft != null) {
		myLeft.addSuccessorsTo(set);
	}
	if (myRight != null) {
		myRight.addSuccessorsTo(set);
	}
/*
udanax-top.st:4276:BranchDescription methodsFor: 'accessing'!
{void} addSuccessorsTo: set {MuSet} 
	"Add the first useable positions for all successor branches to the set."
	set store: (TracePosition make: self with: 3).
	myLeft ~~ NULL ifTrue: [myLeft addSuccessorsTo: set].
	myRight ~~ NULL ifTrue: [myRight addSuccessorsTo: set]!
*/
}
public ImmuSet successorsOf(BoundedTrace trace) {
	MuSet set;
	set = fulltrace.successorsOf(trace);
	if (trace.position() != lastPosition) {
		set.store((TracePosition.make(this, trace.position() + 1)));
	}
	return set.asImmuSet();
/*
udanax-top.st:4283:BranchDescription methodsFor: 'accessing'!
{ImmuSet} successorsOf: trace {BoundedTrace}
	| set {MuSet} |
	set _ fulltrace successorsOf: trace.
	trace position ~~ lastPosition ifTrue: [set store: (TracePosition make: self with: trace position + 1)].
	^set asImmuSet!
*/
}
/**
 * Return a new successor to the receiver. The first successor is on the
 * same branch with a higher position. Further successors are allocated
 * in a binary-tree fashion along a new branch.
 */
public TracePosition createAfter(BoundedTrace trace) {
	if (lastPosition == trace.position()) {
		return nextPosition();
	}
	else {
		BranchDescription branch;
		branch = BranchDescription.make(fulltrace, trace);
		fulltrace.installBranchAfter(branch, trace);
		return branch.nextPosition();
	}
/*
udanax-top.st:4291:BranchDescription methodsFor: 'position making'!
{TracePosition} createAfter: trace {BoundedTrace}
	"Return a new successor to the receiver. The first successor is on the 
	same branch with a higher position. Further successors are allocated 
	in a binary-tree fashion along a new branch."
	lastPosition == trace position
		ifTrue: [^self nextPosition]
		ifFalse: 
			[| branch {BranchDescription} |
			branch _ BranchDescription make: fulltrace with: trace.
			fulltrace installBranch: branch after: trace.
			^branch nextPosition]!
*/
}
/**
 * Install branch as a descendant branch of myself. Walk down the binary tree of
 * branches to find a place to lodge it. This gets called if there was already a
 * branch existing off my root.
 */
public void installBranch(BranchDescription branch) {
	if (branch.isEqual(this)) {
		return ;
	}
	diskUpdate();
	if (myLeft == null) {
		myLeft = branch;
	}
	else {
		BranchDescription tmpBr;
		myLeft.installBranch(branch);
		tmpBr = myLeft;
		myLeft = myRight;
		myRight = tmpBr;
	}
/*
udanax-top.st:4304:BranchDescription methodsFor: 'position making'!
{void} installBranch: branch {BranchDescription} 
	"Install branch as a descendant branch of myself. Walk down the binary tree of 
	branches to find a place to lodge it. This gets called if there was already a 
	branch existing off my root."
	(branch isEqual: self) ifTrue: [^VOID].
	self diskUpdate.
	myLeft == NULL
		ifTrue: [myLeft _ branch]
		ifFalse: 
			[| tmpBr {BranchDescription} |
			myLeft installBranch: branch.
			tmpBr _ myLeft.
			myLeft _ myRight.
			myRight _ tmpBr]!
*/
}
public void installBranchAfter(BranchDescription branch, TracePosition trace) {
	fulltrace.installBranchAfter(branch, trace);
/*
udanax-top.st:4319:BranchDescription methodsFor: 'position making'!
{void} installBranch: branch {BranchDescription} after: trace {TracePosition}
	fulltrace installBranch: branch after: trace!
*/
}
/**
 * Create a dag branch that succeeds both trace1 and trace2.
 */
public BranchDescription makeBranch(TracePosition trace1, TracePosition trace2) {
	return BranchDescription.make(fulltrace, trace1, trace2);
/*
udanax-top.st:4322:BranchDescription methodsFor: 'position making'!
{BranchDescription} makeBranch: trace1 {TracePosition} with: trace2 {TracePosition}
	"Create a dag branch that succeeds both trace1 and trace2."
	^BranchDescription make: fulltrace with: trace1 with: trace2!
*/
}
/**
 * Return the first available tracePosition on this branch.
 */
public TracePosition nextPosition() {
	lastPosition = lastPosition + 1;
	diskUpdate();
	return TracePosition.make(this, lastPosition);
/*
udanax-top.st:4327:BranchDescription methodsFor: 'position making'!
{TracePosition} nextPosition
	"Return the first available tracePosition on this branch."
	lastPosition _ lastPosition + 1.
	self diskUpdate.
	^TracePosition make: self with: lastPosition!
*/
}
public BranchDescription(DagWood ft) {
	super();
	fulltrace = ft;
	myLeft = null;
	myRight = null;
	lastPosition = 2;
/*
udanax-top.st:4336:BranchDescription methodsFor: 'protected: protected create'!
create: ft {DagWood}
	super create.
	fulltrace _ ft.
	myLeft _ NULL.
	myRight _ NULL.
	lastPosition _ 2!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print(hashForEqual());
/*
udanax-top.st:4345:BranchDescription methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << self hashForEqual!
*/
}
/**
 * @deprecated
 */
public boolean equalsX(BranchDescription another) {
	throw new PasseException();
/*
udanax-top.st:4350:BranchDescription methodsFor: 'smalltalk: smalltalk passe'!
{Boolean} = another {BranchDescription}
	self passe!
*/
}
/*
udanax-top.st:4353:BranchDescription methodsFor: 'smalltalk: smalltalk passe'!
{UInt32} ohashForEqual
	"See the comment for isEqual:."
	
	"^myBranchNum * 945737"!
*/
/*
udanax-top.st:4358:BranchDescription methodsFor: 'smalltalk: smalltalk passe'!
{BooleanVar} oisEqual: another {Heaper}
	"^(another isKindOf: BranchDescription) and: [(another basicCast: BranchDescription) branchNum == myBranchNum]"!
*/
public BranchDescription(Rcvr receiver) {
	super(receiver);
	lastPosition = receiver.receiveUInt32();
	myLeft = (BranchDescription) receiver.receiveHeaper();
	myRight = (BranchDescription) receiver.receiveHeaper();
	fulltrace = (DagWood) receiver.receiveHeaper();
/*
udanax-top.st:4363:BranchDescription methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	lastPosition _ receiver receiveUInt32.
	myLeft _ receiver receiveHeaper.
	myRight _ receiver receiveHeaper.
	fulltrace _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendUInt32(lastPosition);
	xmtr.sendHeaper(myLeft);
	xmtr.sendHeaper(myRight);
	xmtr.sendHeaper(fulltrace);
/*
udanax-top.st:4370:BranchDescription methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendUInt32: lastPosition.
	xmtr sendHeaper: myLeft.
	xmtr sendHeaper: myRight.
	xmtr sendHeaper: fulltrace.!
*/
}
public static BranchDescription make(DagWood fulltrace) {
	return new RootBranch(fulltrace);
/*
udanax-top.st:4386:BranchDescription class methodsFor: 'instance creation'!
make: fulltrace {DagWood}
	^RootBranch create: fulltrace!
*/
}
public static BranchDescription make(DagWood fulltrace, TracePosition parent) {
	return new TreeBranch(fulltrace, parent);
/*
udanax-top.st:4389:BranchDescription class methodsFor: 'instance creation'!
make: fulltrace {DagWood} with: parent {TracePosition}
	^TreeBranch create: fulltrace with: parent!
*/
}
public static BranchDescription make(DagWood fulltrace, TracePosition parent1, TracePosition parent2) {
	return new DagBranch(fulltrace, parent1, parent2);
/*
udanax-top.st:4392:BranchDescription class methodsFor: 'instance creation'!
{BranchDescription} make: fulltrace {DagWood} with: parent1 {TracePosition} with: parent2 {TracePosition}
	^DagBranch create: fulltrace with: parent1 with: parent2!
*/
}
public BranchDescription() {
/*

Generated during transformation
*/
}
}
