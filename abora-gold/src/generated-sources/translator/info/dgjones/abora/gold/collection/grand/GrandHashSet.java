/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.grand;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.grand.ExponentialHashMap;
import info.dgjones.abora.gold.collection.grand.GrandEntry;
import info.dgjones.abora.gold.collection.grand.GrandHashSet;
import info.dgjones.abora.gold.collection.grand.GrandNode;
import info.dgjones.abora.gold.collection.grand.GrandSetEntry;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandHashSetStepper;
import info.dgjones.abora.gold.grantab.GrandNodeDoubler;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class GrandHashSet extends MuSet {

	protected PtrArray grandNodes;
	protected int numNodes;
	protected int nodeIndexShift;
	protected Counter myTally;
	protected Counter myDoublingFrontIndex;
	protected Counter myDoublingPasses;
	protected int cacheHash;
	protected Heaper cacheValue;
	protected int myOutstandingSteppers;
/*
udanax-top.st:45985:
MuSet subclass: #GrandHashSet
	instanceVariableNames: '
		grandNodes {PtrArray copy of: GrandNode}
		numNodes {Int32 copy}
		nodeIndexShift {Int32 copy}
		myTally {Counter copy}
		myDoublingFrontIndex {Counter copy}
		myDoublingPasses {Counter copy}
		cacheHash {UInt32 NOCOPY}
		cacheValue {Heaper wimpy NOCOPY}
		myOutstandingSteppers {IntegerVar NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:45998:
(GrandHashSet getOrMakeCxxClassDescription)
	friends:
'/- friends for class GrandHashSet -/
friend SPTR(GrandHashSet)  grandHashSet ();
friend SPTR(GrandHashSet)  grandHashSet (Int4 nNodes);
friend class GrandHashSetStepper;
';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:46234:
GrandHashSet class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:46237:
(GrandHashSet getOrMakeCxxClassDescription)
	friends:
'/- friends for class GrandHashSet -/
friend SPTR(GrandHashSet)  grandHashSet ();
friend SPTR(GrandHashSet)  grandHashSet (Int4 nNodes);
friend class GrandHashSetStepper;
';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandHashSet.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public void introduce(Heaper aHeaper) {
	int hash;
	GrandNode node;
	checkSteppers();
	if (aHeaper == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);
	}
	hash = ExponentialHashMap.exponentialMap(aHeaper.hashForEqual());
	node = (GrandNode) (grandNodes.fetch(hash / nodeIndexShift));
	if ((node.fetch(aHeaper, hash)) != null) {
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_SET);
	}
	else {
		GrandEntry newEntry;
		AboraBlockSupport.enterConsistent(6);
		try {
			newEntry = GrandSetEntry.make(aHeaper, hash);
			node.storeEntry(newEntry);
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	myTally.increment();
	considerNeedForDoubling();
	invalidateCache();
/*
udanax-top.st:46009:GrandHashSet methodsFor: 'adding-removing'!
{void} introduce: aHeaper {Heaper}
	| hash {UInt32} node {GrandNode} |
	self checkSteppers.
	aHeaper == NULL ifTrue: [Heaper BLAST: #NullInsertion].
	hash _ ExponentialHashMap exponentialMap: aHeaper hashForEqual.
	node _ (grandNodes fetch: hash // nodeIndexShift) cast: GrandNode.
	(node fetch: aHeaper with: hash) ~~ NULL
		ifTrue: [Heaper BLAST: #AlreadyInSet]
		ifFalse: [| newEntry {GrandEntry} |
				DiskManager consistent: 6 with:
					[newEntry _ GrandSetEntry make: aHeaper with: hash.
					node store.Entry: newEntry]].
	myTally increment.
	self considerNeedForDoubling.
	self invalidateCache.!
*/
}
public void remove(Heaper aHeaper) {
	if ( ! (hasMember(aHeaper))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_SET);
	}
	else {
		wipe(aHeaper);
	}
/*
udanax-top.st:46026:GrandHashSet methodsFor: 'adding-removing'!
{void} remove: aHeaper {Heaper} 
	((self hasMember: aHeaper) == NULL) 
		ifTrue: [Heaper BLAST: #NotInSet]
		ifFalse: [self wipe: aHeaper]!
*/
}
public void store(Heaper aHeaper) {
	int hash;
	GrandNode node;
	GrandEntry newEntry;
	boolean test;
	checkSteppers();
	if (aHeaper == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);
	}
	hash = ExponentialHashMap.exponentialMap(aHeaper.hashForEqual());
	node = (GrandNode) (grandNodes.fetch(hash / nodeIndexShift));
	test = (node.fetch(aHeaper, hash)) == null;
	AboraBlockSupport.enterConsistent(6);
	try {
		newEntry = GrandSetEntry.make(aHeaper, hash);
		node.storeEntry(newEntry);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	if (test) {
		myTally.increment();
		considerNeedForDoubling();
	}
	invalidateCache();
/*
udanax-top.st:46032:GrandHashSet methodsFor: 'adding-removing'!
{void} store: aHeaper {Heaper} 
	| hash {UInt32} node {GrandNode} newEntry {GrandEntry} test {BooleanVar} |
	self checkSteppers.
	aHeaper == NULL ifTrue: [Heaper BLAST: #NullInsertion].
	hash _ ExponentialHashMap exponentialMap: aHeaper hashForEqual.
	node _ (grandNodes fetch: hash // nodeIndexShift) cast: GrandNode.
	test _ (node fetch: aHeaper with: hash) == NULL.
	DiskManager consistent: 6 with:
		[newEntry _ GrandSetEntry make: aHeaper with: hash.
		node store.Entry: newEntry].
	test ifTrue: 
		[myTally increment.
		self considerNeedForDoubling].
	self invalidateCache.!
*/
}
public void wipe(Heaper aHeaper) {
	int hash;
	GrandNode node;
	checkSteppers();
	hash = ExponentialHashMap.exponentialMap(aHeaper.hashForEqual());
	node = (GrandNode) (grandNodes.fetch(hash / nodeIndexShift));
	if ((node.fetch(aHeaper, hash)) != null) {
		node.wipe(aHeaper, hash);
		myTally.decrement();
	}
/*
udanax-top.st:46047:GrandHashSet methodsFor: 'adding-removing'!
{void} wipe: aHeaper {Heaper} 
	| hash {UInt32} node {GrandNode} |
	self checkSteppers.
	hash _ ExponentialHashMap exponentialMap: aHeaper hashForEqual.
	node _ (grandNodes fetch: hash // nodeIndexShift) cast: GrandNode.
	(node fetch: aHeaper with: hash) ~~ NULL
		ifTrue: 
			[node wipe: aHeaper with: hash.
			myTally decrement]!
*/
}
public int count() {
	return myTally.count();
/*
udanax-top.st:46059:GrandHashSet methodsFor: 'accessing'!
{IntegerVar} count
	^myTally count!
*/
}
public boolean hasMember(Heaper aHeaper) {
	int hash;
	Heaper result;
	hash = ExponentialHashMap.exponentialMap(aHeaper.hashForEqual());
	/* (cacheKey ~~ NULL and: [cacheHash == hash and: [cacheKey isEqual: key]]) ifTrue: [ ^ cacheValue ]. */
	result = ((GrandNode) (grandNodes.fetch(hash / nodeIndexShift))).fetch(aHeaper, hash);
	/* result ~~ NULL ifTrue:
		[cacheHash _ hash.
		 cacheKey _ key.
		 cacheValue _ result]. */
	return result != null;
/*
udanax-top.st:46063:GrandHashSet methodsFor: 'accessing'!
{BooleanVar} hasMember: aHeaper {Heaper} 
	| hash {UInt32} result {Heaper} |
	hash _ ExponentialHashMap exponentialMap: aHeaper hashForEqual.
	"(cacheKey ~~ NULL and: [cacheHash == hash and: [cacheKey isEqual: key]]) ifTrue: [ ^ cacheValue ]."
	result _ ((grandNodes fetch: hash // nodeIndexShift) cast: GrandNode) fetch: aHeaper with: hash.
	"result ~~ NULL ifTrue:
		[cacheHash _ hash.
		 cacheKey _ key.
		 cacheValue _ result]."
	^ result ~~ NULL!
*/
}
public boolean isEmpty() {
	return myTally.count() == 0;
/*
udanax-top.st:46076:GrandHashSet methodsFor: 'testing'!
{BooleanVar} isEmpty
	^myTally count == IntegerVar0!
*/
}
public ImmuSet asImmuSet() {
	willNotImplement();
	return null;
/*
udanax-top.st:46082:GrandHashSet methodsFor: 'conversion'!
{ImmuSet} asImmuSet
	self willNotImplement.
	^ NULL!
*/
}
public MuSet asMuSet() {
	willNotImplement();
	return null;
/*
udanax-top.st:46086:GrandHashSet methodsFor: 'conversion'!
{MuSet} asMuSet
	self willNotImplement.
	^ NULL!
*/
}
public ScruSet copy() {
	MuSet newSet;
	newSet = GrandHashSet.make(numNodes);
	Stepper stomper = stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper e = (Heaper) stomper.fetch();
		if (e == null) {
			continue ;
		}
		newSet.store(e);
	}
	stomper.destroy();
	return newSet;
/*
udanax-top.st:46092:GrandHashSet methodsFor: 'creation'!
{ScruSet} copy
	| newSet {MuSet} |
	newSet _ GrandHashSet make: numNodes.
	self stepper forEach:
		[:e {Heaper} |
		newSet store: e].
	^ newSet!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print("GrandHashSet(");
	aStream.print(count());
	aStream.print(" entries over ");
	aStream.print(numNodes);
	aStream.print(" nodes)");
/*
udanax-top.st:46102:GrandHashSet methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << 'GrandHashSet(' << self count << ' entries over ' << numNodes << ' nodes)'!
*/
}
public void printOnWithSimpleSyntax(PrintWriter oo, String open, String sep, String close) {
	Stepper stomp;
	oo.print(open);
	if (isEmpty()) {
		oo.print("empty");
	}
	else {
		stomp = stepper();
		while (stomp.hasValue()) {
			oo.print(stomp.fetch());
			stomp.step();
			if (stomp.hasValue()) {
				oo.print(sep);
			}
		}
		stomp.destroy();
	}
	oo.print(close);
/*
udanax-top.st:46105:GrandHashSet methodsFor: 'printing'!
{void} printOnWithSimpleSyntax: oo {ostream reference} with: open {char star} with: sep {char star} with: close {char star} 
	| stomp {Stepper} |
	oo << open.
	self isEmpty
		ifTrue: [oo << 'empty']
		ifFalse: 
			[stomp _ self stepper.
			[stomp hasValue]
				whileTrue: 
					[oo << stomp fetch.
					stomp step.
					stomp hasValue ifTrue: [oo << sep]].
			stomp destroy].
	oo << close!
*/
}
public Stepper stepper() {
	return new GrandHashSetStepper(this);
/*
udanax-top.st:46122:GrandHashSet methodsFor: 'enumerating'!
{Stepper} stepper
	^ GrandHashSetStepper create: self!
*/
}
public GrandHashSet(int nNodes) {
	super();
	GrandNode aNode;
	numNodes = nNodes;
	nodeIndexShift = ExponentialHashMap.hashBits() / numNodes;
	grandNodes = PtrArray.nulls(numNodes);
	AboraBlockSupport.enterConsistent(2 * numNodes + 3);
	try {
		for (int i = 0; i < numNodes; i ++ ) {
			aNode = GrandNode.make();
			grandNodes.store(i, aNode);
		}
		myTally = Counter.make();
		myDoublingFrontIndex = Counter.make();
		myDoublingPasses = Counter.make();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	myOutstandingSteppers = 0;
	invalidateCache();
/*
udanax-top.st:46127:GrandHashSet methodsFor: 'protected: creation'!
create: nNodes {Int32}
	| aNode {GrandNode} |
	super create. 
	numNodes _ nNodes.
	nodeIndexShift _ ExponentialHashMap hashBits // numNodes.
	grandNodes _ PtrArray nulls: numNodes.
	DiskManager consistent: 2 * numNodes + 3 with:
		[Int32Zero almostTo: numNodes do: [:i {Int32} | 
			aNode _ GrandNode make.
			grandNodes at: i store: aNode].
		myTally _ Counter make.
		myDoublingFrontIndex _ Counter make.
		myDoublingPasses _ Counter make].
	myOutstandingSteppers _ IntegerVarZero.
	self invalidateCache!
*/
}
public void destruct() {
	Heaper temp;
	checkSteppers();
	AboraBlockSupport.enterConsistent(numNodes);
	try {
		for (int i = 0; i < numNodes; i ++ ) {
			if ((temp = grandNodes.fetch(i)) != null) {
				temp.destroy();
			}
		}
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	super.destruct();
/*
udanax-top.st:46143:GrandHashSet methodsFor: 'protected: creation'!
{void} destruct
 	| temp {Heaper} |
 	self checkSteppers.
 	DiskManager consistent: numNodes with:
		[UInt32Zero almostTo: numNodes do:
			[:i {UInt32} |
			(temp _ grandNodes fetch: i) ~~ NULL ifTrue: [temp destroy]]].
	super destruct!
*/
}
/**
 * Compute location of doubling front from tally.  If front crosses a node boundary
 */
public void considerNeedForDoubling() {
	/*  and that node has index higher than doublingFrontIndex then double that node. */
	/*  Then increase doublingFrontIndex.  If the front has hit the end of the table index */
	/*  reset it to zero.  This allows elements to be wiped from the table without causing */
	/*  extra node doubling to occur on later insertions.  This aims for 80% max table */
	/* loading using an approximation of the formula given in the Fagin paper. */
	int desiredDoublingIndex;
	double x;
	int dfi;
	x = 0.05
	/* Magic number */
	* numNodes * (1 << myDoublingPasses.count()) * GrandNode.primaryPageSize();
	desiredDoublingIndex = (int) ((float) myTally.count() / x)
	/* - 1 */
	;
	dfi = myDoublingFrontIndex.count();
	if (desiredDoublingIndex >= (dfi + 1)) {
		if ((grandNodes.fetch(dfi)) != null) {
			(GrandNodeDoubler.make(((GrandNode) (grandNodes.fetch(dfi))))).schedule();
		}
		dfi = myDoublingFrontIndex.increment();
	}
	if (dfi >= numNodes) {
		myDoublingFrontIndex.setCount(0);
		myDoublingPasses.increment();
	}
/*
udanax-top.st:46154:GrandHashSet methodsFor: 'private: housekeeping'!
{void} considerNeedForDoubling
	"Compute location of doubling front from tally.  If front crosses a node boundary"
	" and that node has index higher than doublingFrontIndex then double that node."
	" Then increase doublingFrontIndex.  If the front has hit the end of the table index"
	" reset it to zero.  This allows elements to be wiped from the table without causing"
	" extra node doubling to occur on later insertions.  This aims for 80% max table"
	"loading using an approximation of the formula given in the Fagin paper."
	| desiredDoublingIndex {Int32} x {IEEEDoubleVar} dfi {Int32} |
	x _ 0.05"Magic number" * numNodes * (1 bitShift: myDoublingPasses count DOTasLong) * GrandNode primaryPageSize.
	desiredDoublingIndex _ (myTally count DOTasLong asFloat / x) asInteger "- 1".
	dfi _ myDoublingFrontIndex count DOTasLong.
	desiredDoublingIndex >= (dfi + 1) ifTrue:
		[(grandNodes fetch: dfi) ~~ NULL ifTrue: [(GrandNodeDoubler make: ((grandNodes fetch: dfi) cast: GrandNode)) schedule].
		dfi _ myDoublingFrontIndex increment DOTasLong].
	dfi >= numNodes ifTrue:
		[myDoublingFrontIndex setCount: IntegerVar0.
		myDoublingPasses increment]!
*/
}
public void invalidateCache() {
	cacheValue = null;
/*
udanax-top.st:46172:GrandHashSet methodsFor: 'private: housekeeping'!
{void} invalidateCache
	cacheValue _ NULL.!
*/
}
/**
 * re-initialize the non-persistent part
 */
public void restartGrandHashSet(Rcvr trans) {
	cacheValue = null;
	myOutstandingSteppers = 0;
/*
udanax-top.st:46177:GrandHashSet methodsFor: 'receiver'!
{void RECEIVE.HOOK} restartGrandHashSet: trans {Rcvr unused default: NULL}
	"re-initialize the non-persistent part"
	cacheValue _ NULL.
	myOutstandingSteppers _ IntegerVar0!
*/
}
public GrandNode nodeAt(int idx) {
	return (GrandNode) (grandNodes.fetch(idx));
/*
udanax-top.st:46184:GrandHashSet methodsFor: 'private: friendly'!
{GrandNode} nodeAt: idx {IntegerVar}
	^ (grandNodes fetch: idx DOTasLong) cast: GrandNode!
*/
}
public int nodeCount() {
	return numNodes;
/*
udanax-top.st:46187:GrandHashSet methodsFor: 'private: friendly'!
{IntegerVar} nodeCount
	^ numNodes!
*/
}
/*
udanax-top.st:46192:GrandHashSet methodsFor: 'private: smalltalk: private'!
inspectPieces
	^grandNodes asOrderedCollection!
*/
public void checkSteppers() {
	if (myOutstandingSteppers > 0) {
		throw new AboraRuntimeException(AboraRuntimeException.MODIFY_BLOCKED_BY_OUTSTANDING_STEPPER);
	}
/*
udanax-top.st:46197:GrandHashSet methodsFor: 'private: enumerating'!
{void INLINE} checkSteppers
	myOutstandingSteppers > IntegerVar0 ifTrue:
		[ Heaper BLAST: #ModifyBlockedByOutstandingStepper ]!
*/
}
public void fewerSteppers() {
	myOutstandingSteppers = myOutstandingSteppers - 1;
	if (myOutstandingSteppers < 0) {
		throw new AboraRuntimeException(AboraRuntimeException.TOO_MANY_STEPPERS_RELEASED);
	}
/*
udanax-top.st:46201:GrandHashSet methodsFor: 'private: enumerating'!
{void} fewerSteppers
	myOutstandingSteppers _ myOutstandingSteppers - 1.
	myOutstandingSteppers < IntegerVar0 ifTrue:
		[ Heaper BLAST: #TooManySteppersReleased ]!
*/
}
public Stepper immuStepper() {
	Someone.hack();
	/* This will have to be fixed if GrandHashSet::stepper ever makes a copy */
	return stepper();
/*
udanax-top.st:46206:GrandHashSet methodsFor: 'private: enumerating'!
{Stepper} immuStepper
	self hack. "This will have to be fixed if GrandHashSet::stepper ever makes a copy"
	 ^ self stepper!
*/
}
public void moreSteppers() {
	myOutstandingSteppers = myOutstandingSteppers + 1;
/*
udanax-top.st:46210:GrandHashSet methodsFor: 'private: enumerating'!
{void} moreSteppers
	myOutstandingSteppers _ myOutstandingSteppers + 1!
*/
}
public GrandHashSet(Rcvr receiver) {
	super(receiver);
	grandNodes = (PtrArray) receiver.receiveHeaper();
	numNodes = receiver.receiveInt32();
	nodeIndexShift = receiver.receiveInt32();
	myTally = (Counter) receiver.receiveHeaper();
	myDoublingFrontIndex = (Counter) receiver.receiveHeaper();
	myDoublingPasses = (Counter) receiver.receiveHeaper();
/*
udanax-top.st:46215:GrandHashSet methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	grandNodes _ receiver receiveHeaper.
	numNodes _ receiver receiveInt32.
	nodeIndexShift _ receiver receiveInt32.
	myTally _ receiver receiveHeaper.
	myDoublingFrontIndex _ receiver receiveHeaper.
	myDoublingPasses _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(grandNodes);
	xmtr.sendInt32(numNodes);
	xmtr.sendInt32(nodeIndexShift);
	xmtr.sendHeaper(myTally);
	xmtr.sendHeaper(myDoublingFrontIndex);
	xmtr.sendHeaper(myDoublingPasses);
/*
udanax-top.st:46224:GrandHashSet methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: grandNodes.
	xmtr sendInt32: numNodes.
	xmtr sendInt32: nodeIndexShift.
	xmtr sendHeaper: myTally.
	xmtr sendHeaper: myDoublingFrontIndex.
	xmtr sendHeaper: myDoublingPasses.!
*/
}
public static MuSet make() {
	return new GrandHashSet(32
	/* A Very big table */
	);
/*
udanax-top.st:46248:GrandHashSet class methodsFor: 'pseudoConstructors'!
make 
	^ self create: 32 "A Very big table"!
*/
}
public static MuSet make(int nNodes) {
	return new GrandHashSet(nNodes);
/*
udanax-top.st:46251:GrandHashSet class methodsFor: 'pseudoConstructors'!
make: nNodes {Int32} 
	^ self create: nNodes!
*/
}
/**
 * GrandHashTable initTimeNonInherited
 */
public static void initTimeNonInherited() {
/*
udanax-top.st:46256:GrandHashSet class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	"GrandHashTable initTimeNonInherited"
	
	self REQUIRES: ExponentialHashMap!
*/
}
public GrandHashSet() {
/*

Generated during transformation
*/
}
}
