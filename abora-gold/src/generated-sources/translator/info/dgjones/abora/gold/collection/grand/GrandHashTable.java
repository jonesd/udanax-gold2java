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
import info.dgjones.abora.gold.collection.grand.GrandHashTable;
import info.dgjones.abora.gold.collection.grand.GrandNode;
import info.dgjones.abora.gold.collection.grand.GrandTableEntry;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ImmuTable;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandHashTableStepper;
import info.dgjones.abora.gold.grantab.GrandNodeDoubler;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class GrandHashTable extends MuTable {

	protected PtrArray grandNodes;
	protected int numNodes;
	protected int nodeIndexShift;
	protected Counter myTally;
	protected Counter myDoublingFrontIndex;
	protected Counter myDoublingPasses;
	protected CoordinateSpace myCs;
	protected int cacheHash;
	protected Position cacheKey;
	protected Heaper cacheValue;
	protected int myOutstandingSteppers;
/*
udanax-top.st:48154:
MuTable subclass: #GrandHashTable
	instanceVariableNames: '
		grandNodes {PtrArray of: GrandNode}
		numNodes {Int32}
		nodeIndexShift {Int32}
		myTally {Counter}
		myDoublingFrontIndex {Counter}
		myDoublingPasses {Counter}
		myCs {CoordinateSpace}
		cacheHash {UInt32 NOCOPY}
		cacheKey {Position NOCOPY wimpy}
		cacheValue {Heaper NOCOPY wimpy}
		myOutstandingSteppers {IntegerVar NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:48169:
(GrandHashTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class GrandHashTable -/
friend SPTR(GrandHashTable) grandHashTable (CoordinateSpace *);
friend SPTR(GrandHashTable) grandHashTable (CoordinateSpace *, Int4 nNodes);
friend class GrandHashTableStepper;';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:48428:
GrandHashTable class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:48431:
(GrandHashTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class GrandHashTable -/
friend SPTR(GrandHashTable) grandHashTable (CoordinateSpace *);
friend SPTR(GrandHashTable) grandHashTable (CoordinateSpace *, Int4 nNodes);
friend class GrandHashTableStepper;';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandHashTable.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper store(Position aKey, Heaper aHeaper) {
	int hash;
	GrandNode node;
	GrandEntry newEntry;
	Heaper old;
	checkSteppers();
	if (aHeaper == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);
	}
	hash = ExponentialHashMap.exponentialMap(aKey.hashForEqual());
	node = (GrandNode) (grandNodes.fetch(hash / nodeIndexShift));
	old = node.fetch(aKey, hash);
	AboraBlockSupport.enterConsistent(1);
	try {
		newEntry = GrandTableEntry.make(aHeaper, aKey, hash);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	node.storeEntry(newEntry);
	if (old == null) {
		myTally.increment();
		considerNeedForDoubling();
	}
	invalidateCache();
	return old;
/*
udanax-top.st:48179:GrandHashTable methodsFor: 'adding-removing'!
{Heaper} at: aKey {Position} store: aHeaper {Heaper} 
	| hash {UInt32} node {GrandNode} newEntry {GrandEntry} old {Heaper} |
	self checkSteppers.
	aHeaper == NULL ifTrue: [Heaper BLAST: #NullInsertion].
	hash _ ExponentialHashMap exponentialMap: aKey hashForEqual.
	node _ (grandNodes fetch: hash // nodeIndexShift) cast: GrandNode.
	old _ node fetch: aKey with: hash.
	DiskManager consistent: 1 with: [newEntry _ GrandTableEntry make: aHeaper with: aKey with: hash].
	node store.Entry: newEntry.
	old == NULL ifTrue: 
		[myTally increment.
		self considerNeedForDoubling].
	self invalidateCache.
	^ old!
*/
}
public boolean wipe(Position aKey) {
	int hash;
	GrandNode node;
	checkSteppers();
	hash = ExponentialHashMap.exponentialMap(aKey.hashForEqual());
	node = (GrandNode) (grandNodes.fetch(hash / nodeIndexShift));
	if ((node.fetch(aKey, hash)) != null) {
		node.wipe(aKey, hash);
		myTally.decrement();
		return true;
	}
	return false;
/*
udanax-top.st:48194:GrandHashTable methodsFor: 'adding-removing'!
{BooleanVar} wipe: aKey {Position} 
	| hash {UInt32} node {GrandNode} |
	self checkSteppers.
	hash _ ExponentialHashMap exponentialMap: aKey hashForEqual.
	node _ (grandNodes fetch: hash // nodeIndexShift) cast: GrandNode.
	(node fetch: aKey with: hash) ~~ NULL
		ifTrue: 
			[node wipe: aKey with: hash.
			myTally decrement.
			^ true].
	^ false!
*/
}
public CoordinateSpace coordinateSpace() {
	return myCs;
/*
udanax-top.st:48208:GrandHashTable methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^ myCs!
*/
}
public int count() {
	return myTally.count();
/*
udanax-top.st:48212:GrandHashTable methodsFor: 'accessing'!
{IntegerVar} count
	^myTally count!
*/
}
public XnRegion domain() {
	XnRegion result;
	TableStepper stepper;
	result = coordinateSpace().emptyRegion();
	Stepper stomper = (stepper = (TableStepper) stepper());
	for (; stomper.hasValue(); stomper.step()) {
		Heaper elem = (Heaper) stomper.fetch();
		if (elem == null) {
			continue ;
		}
		result = result.with((stepper.position()));
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:48216:GrandHashTable methodsFor: 'accessing'!
{XnRegion} domain
	| result {XnRegion} stepper {TableStepper} |
	
	result _ self coordinateSpace emptyRegion.
	(stepper _ self stepper cast: TableStepper) forEach:
		[ :elem {Heaper} |
		result _ result with: (stepper position)].
	^ result!
*/
}
public Heaper fetch(Position key) {
	int hash;
	Heaper result;
	hash = ExponentialHashMap.exponentialMap(key.hashForEqual());
	/* (cacheKey ~~ NULL 
	  and: [cacheHash == hash 
	  and: [cacheKey isEqual: key]]) 
	  	ifTrue: [ ^ cacheValue ].  */
	result = ((GrandNode) (grandNodes.fetch(hash / nodeIndexShift))).fetch(key, hash);
	/* result ~~ NULL ifTrue:
		[cacheHash _ hash.
		 cacheKey _ key.
		 cacheValue _ result]. */
	return result;
/*
udanax-top.st:48225:GrandHashTable methodsFor: 'accessing'!
{Heaper} fetch: key {Position} 
	| hash {UInt32} result {Heaper} |
	hash _ ExponentialHashMap exponentialMap: key hashForEqual.
	"(cacheKey ~~ NULL 
	  and: [cacheHash == hash 
	  and: [cacheKey isEqual: key]]) 
	  	ifTrue: [ ^ cacheValue ]. "
	  	
	result _ ((grandNodes fetch: hash // nodeIndexShift) cast: GrandNode) 
				fetch: key 
				with: hash.
	"result ~~ NULL ifTrue:
		[cacheHash _ hash.
		 cacheKey _ key.
		 cacheValue _ result]."
	^ result!
*/
}
public ScruTable subTable(XnRegion region) {
	GrandHashTable newTable;
	TableStepper elements;
	newTable = GrandHashTable.make(myCs, 8);
	elements = stepper();
	Stepper stomper = elements;
	for (; stomper.hasValue(); stomper.step()) {
		Heaper elemValue = (Heaper) stomper.fetch();
		if (elemValue == null) {
			continue ;
		}
		if (region.hasMember(elements.position())) {
			newTable.store(elements.position(), elemValue);
		}
	}
	stomper.destroy();
	return newTable;
/*
udanax-top.st:48244:GrandHashTable methodsFor: 'accessing'!
{ScruTable} subTable: region {XnRegion}
	| newTable {GrandHashTable} elements {TableStepper} |
	newTable _ GrandHashTable make.CoordinateSpace: myCs with: 8.
	elements _ self stepper.
	elements forEach:
		[:elemValue {Heaper} | (region hasMember: elements position) ifTrue:
			[newTable at: elements position store: elemValue]].
	^newTable!
*/
}
public boolean includesIntKey(int aKey) {
	return super.includesIntKey(aKey);
/*
udanax-top.st:48256:GrandHashTable methodsFor: 'testing'!
{BooleanVar} includesIntKey: aKey {IntegerVar}
	^super includesIntKey: aKey!
*/
}
public boolean includesKey(Position aKey) {
	return (fetch(aKey)) != null;
/*
udanax-top.st:48260:GrandHashTable methodsFor: 'testing'!
{BooleanVar} includesKey: aKey {Position}
	^ (self fetch: aKey) ~~ NULL!
*/
}
public boolean isEmpty() {
	return myTally.count() == 0;
/*
udanax-top.st:48264:GrandHashTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	^myTally count == IntegerVar0!
*/
}
public ScruTable copy() {
	GrandHashTable newTable;
	TableStepper s;
	newTable = GrandHashTable.make(myCs, numNodes);
	Stepper stomper = (s = stepper());
	for (; stomper.hasValue(); stomper.step()) {
		Heaper e = (Heaper) stomper.fetch();
		if (e == null) {
			continue ;
		}
		newTable.store((s.position()), e);
	}
	stomper.destroy();
	return newTable;
/*
udanax-top.st:48270:GrandHashTable methodsFor: 'creation'!
{ScruTable} copy
	| newTable {GrandHashTable} s {TableStepper} |
	newTable _ GrandHashTable make: myCs with: numNodes.
	(s _ self stepper) forEach:
		[:e {Heaper} |
		newTable at: (s position) store: e].
	^ newTable!
*/
}
public ScruTable emptySize(int size) {
	return GrandHashTable.make(myCs);
/*
udanax-top.st:48279:GrandHashTable methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar unused}
	^ GrandHashTable make: myCs!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print("GrandHashTable(");
	aStream.print(count());
	aStream.print(" entries over ");
	aStream.print(numNodes);
	aStream.print(" nodes)");
/*
udanax-top.st:48285:GrandHashTable methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << 'GrandHashTable(' << self count << ' entries over ' << numNodes << ' nodes)'!
*/
}
public XnRegion runAt(Position index) {
	if (includesKey(index)) {
		return index.asRegion();
	}
	else {
		return myCs.emptyRegion();
	}
/*
udanax-top.st:48290:GrandHashTable methodsFor: 'runs'!
{XnRegion} runAt: index {Position}
	(self includesKey: index)
		ifTrue: [^ index asRegion]
		ifFalse: [^ myCs emptyRegion]!
*/
}
public XnRegion runAtInt(int index) {
	return super.runAtInt(index);
/*
udanax-top.st:48296:GrandHashTable methodsFor: 'runs'!
{XnRegion} runAtInt: index {IntegerVar}
	
	^super runAtInt: index!
*/
}
public void checkSteppers() {
	if (myOutstandingSteppers > 0) {
		throw new AboraRuntimeException(AboraRuntimeException.MODIFY_BLOCKED_BY_OUTSTANDING_STEPPER);
	}
/*
udanax-top.st:48302:GrandHashTable methodsFor: 'private: enumerating'!
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
udanax-top.st:48306:GrandHashTable methodsFor: 'private: enumerating'!
{void} fewerSteppers
	myOutstandingSteppers _ myOutstandingSteppers - 1.
	myOutstandingSteppers < IntegerVar0 ifTrue:
		[ Heaper BLAST: #TooManySteppersReleased ]!
*/
}
public void moreSteppers() {
	myOutstandingSteppers = myOutstandingSteppers + 1;
/*
udanax-top.st:48311:GrandHashTable methodsFor: 'private: enumerating'!
{void} moreSteppers
	myOutstandingSteppers _ myOutstandingSteppers + 1!
*/
}
public TableStepper stepper(OrderSpec order) {
	return new GrandHashTableStepper(this);
/*
udanax-top.st:48316:GrandHashTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec unused default: NULL}
	^ GrandHashTableStepper create: self!
*/
}
public GrandHashTable(CoordinateSpace cs, int nNodes) {
	super();
	GrandNode aNode;
	myCs = cs;
	numNodes = nNodes;
	nodeIndexShift = ExponentialHashMap.hashBits() / (numNodes - 1);
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
udanax-top.st:48321:GrandHashTable methodsFor: 'protected: creation'!
create: cs {CoordinateSpace} with: nNodes {Int32}
	| aNode {GrandNode} |
	super create.
	myCs _ cs.
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
udanax-top.st:48338:GrandHashTable methodsFor: 'protected: creation'!
{void} destruct
 	| temp {Heaper} |
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
udanax-top.st:48348:GrandHashTable methodsFor: 'private: housekeeping'!
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
	cacheKey = null;
/*
udanax-top.st:48366:GrandHashTable methodsFor: 'private: housekeeping'!
{void} invalidateCache
	cacheKey _ NULL.!
*/
}
/**
 * re-initialize the non-persistent part
 */
public void restartGrandHashTable(Rcvr trans) {
	cacheKey = null;
	myOutstandingSteppers = 0;
/*
udanax-top.st:48371:GrandHashTable methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartGrandHashTable: trans {Rcvr unused default: NULL}
	"re-initialize the non-persistent part"
	cacheKey _ NULL.
	myOutstandingSteppers _ IntegerVar0!
*/
}
public GrandNode nodeAt(int idx) {
	return (GrandNode) (grandNodes.fetch(idx));
/*
udanax-top.st:48378:GrandHashTable methodsFor: 'private: friendly'!
{GrandNode} nodeAt: idx {IntegerVar}
	^ (grandNodes fetch: idx DOTasLong) cast: GrandNode!
*/
}
public int nodeCount() {
	return numNodes;
/*
udanax-top.st:48381:GrandHashTable methodsFor: 'private: friendly'!
{IntegerVar} nodeCount
	^ numNodes!
*/
}
/*
udanax-top.st:48386:GrandHashTable methodsFor: 'private: smalltalk: private'!
{void} inspect
	(Sensor ctrlDown) ifTrue:
		[^EntView make: self].
	 ^InspectorView open: (HashTableInspector inspect: self)!
*/
/*
udanax-top.st:48391:GrandHashTable methodsFor: 'private: smalltalk: private'!
inspectPieces
	^grandNodes asOrderedCollection!
*/
public ImmuTable asImmuTable() {
	willNotImplement();
	return null;
/*
udanax-top.st:48396:GrandHashTable methodsFor: 'conversion'!
{ImmuTable}  asImmuTable
	self willNotImplement.
	^ NULL!
*/
}
public MuTable asMuTable() {
	willNotImplement();
	return null;
/*
udanax-top.st:48400:GrandHashTable methodsFor: 'conversion'!
{MuTable}  asMuTable
	self willNotImplement.
	^ NULL!
*/
}
public GrandHashTable(Rcvr receiver) {
	super(receiver);
	grandNodes = (PtrArray) receiver.receiveHeaper();
	numNodes = receiver.receiveInt32();
	nodeIndexShift = receiver.receiveInt32();
	myTally = (Counter) receiver.receiveHeaper();
	myDoublingFrontIndex = (Counter) receiver.receiveHeaper();
	myDoublingPasses = (Counter) receiver.receiveHeaper();
	myCs = (CoordinateSpace) receiver.receiveHeaper();
	restartGrandHashTable(receiver);
/*
udanax-top.st:48406:GrandHashTable methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	grandNodes _ receiver receiveHeaper.
	numNodes _ receiver receiveInt32.
	nodeIndexShift _ receiver receiveInt32.
	myTally _ receiver receiveHeaper.
	myDoublingFrontIndex _ receiver receiveHeaper.
	myDoublingPasses _ receiver receiveHeaper.
	myCs _ receiver receiveHeaper.
	self restartGrandHashTable: receiver.!
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
	xmtr.sendHeaper(myCs);
/*
udanax-top.st:48417:GrandHashTable methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: grandNodes.
	xmtr sendInt32: numNodes.
	xmtr sendInt32: nodeIndexShift.
	xmtr sendHeaper: myTally.
	xmtr sendHeaper: myDoublingFrontIndex.
	xmtr sendHeaper: myDoublingPasses.
	xmtr sendHeaper: myCs.!
*/
}
public static MuTable make(CoordinateSpace cs) {
	return new GrandHashTable(cs, 32
	/* A Very big table */
	);
/*
udanax-top.st:48441:GrandHashTable class methodsFor: 'pseudoConstructors'!
{GrandHashTable} make: cs {CoordinateSpace}
	^ GrandHashTable create: cs with: 32 "A Very big table"!
*/
}
public static GrandHashTable make(CoordinateSpace cs, int nNodes) {
	return new GrandHashTable(cs, nNodes);
/*
udanax-top.st:48444:GrandHashTable class methodsFor: 'pseudoConstructors'!
{GrandHashTable} make: cs {CoordinateSpace} with: nNodes {Int32}
	^ GrandHashTable create: cs with: nNodes!
*/
}
/**
 * GrandHashTable initTimeNonInherited
 */
public static void initTimeNonInherited() {
/*
udanax-top.st:48449:GrandHashTable class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	"GrandHashTable initTimeNonInherited"
	
	self REQUIRES: ExponentialHashMap!
*/
}
public GrandHashTable() {
/*

Generated during transformation
*/
}
}
