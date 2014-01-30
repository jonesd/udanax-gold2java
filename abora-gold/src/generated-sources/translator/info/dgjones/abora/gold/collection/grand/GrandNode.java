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
import info.dgjones.abora.gold.collection.grand.GrandDataPage;
import info.dgjones.abora.gold.collection.grand.GrandEntry;
import info.dgjones.abora.gold.collection.grand.GrandNode;
import info.dgjones.abora.gold.collection.grand.GrandOverflow;
import info.dgjones.abora.gold.grantab.GrandNodeReinserter;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * oldOverflowRoot holds onto the overflow tree that was in place when a node doubling
 * starts.
 * It allows an object stored to be found at any time during the doubling.
 */
public class GrandNode extends Abraham {

	protected PtrArray primaryPages;
	protected int numPrimaries;
	protected GrandOverflow overflowRoot;
	protected GrandOverflow oldOverflowRoot;
	protected int numReinserters;
	protected static int OverflowPageSize;
/*
udanax-top.st:6678:
Abraham subclass: #GrandNode
	instanceVariableNames: '
		primaryPages {PtrArray of: GrandDataPage}
		numPrimaries {Int32}
		overflowRoot {GrandOverflow}
		oldOverflowRoot {GrandOverflow}
		numReinserters {Int32}'
	classVariableNames: 'OverflowPageSize {Int32} '
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:6687:
GrandNode comment:
'oldOverflowRoot holds onto the overflow tree that was in place when a node doubling starts.
It allows an object stored to be found at any time during the doubling.'!
*/
/*
udanax-top.st:6690:
(GrandNode getOrMakeCxxClassDescription)
	friends:
'/- friends for class GrandNode -/
friend class GrandNodeStepper;
friend class GrandNodeDoubler;
friend class GrandNodeReinserter;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:6892:
GrandNode class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:6895:
(GrandNode getOrMakeCxxClassDescription)
	friends:
'/- friends for class GrandNode -/
friend class GrandNodeStepper;
friend class GrandNodeDoubler;
friend class GrandNodeReinserter;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandNode.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch(Heaper toMatch, int aHash) {
	Heaper result;
	result = ((GrandDataPage) (primaryPages.fetch(AboraSupport.modulo(aHash, numPrimaries)))).fetch(toMatch, aHash, numPrimaries);
	if (result != null) {
		return result;
	}
	if (oldOverflowRoot != null) {
		return oldOverflowRoot.fetch(toMatch, aHash);
	}
	return null;
/*
udanax-top.st:6701:GrandNode methodsFor: 'accessing'!
{Heaper} fetch: toMatch {Heaper | Position} with: aHash {UInt32}
	| result {Heaper} |
	result _ ((primaryPages fetch: aHash \\ numPrimaries) cast: GrandDataPage)
		fetch: toMatch with: aHash with: numPrimaries.
	result ~~ NULL ifTrue:
		[ ^ result ].
	oldOverflowRoot ~~ NULL ifTrue:
		[^oldOverflowRoot fetch: toMatch with: aHash].
	^ NULL!
*/
}
public void storeEntry(GrandEntry newEntry) {
	((GrandDataPage) (primaryPages.fetch(AboraSupport.modulo(newEntry.hashForEqual(), numPrimaries)))).storeEntry(newEntry, numPrimaries);
/*
udanax-top.st:6711:GrandNode methodsFor: 'accessing'!
{void} store.Entry: newEntry {GrandEntry}
	((primaryPages fetch: newEntry hashForEqual \\ numPrimaries) cast: GrandDataPage)
		store.Entry: newEntry with: numPrimaries!
*/
}
public void wipe(Heaper toMatch, int aHash) {
	((GrandDataPage) (primaryPages.fetch(AboraSupport.modulo(aHash, numPrimaries)))).wipe(toMatch, aHash, numPrimaries);
	if (oldOverflowRoot != null) {
		oldOverflowRoot.wipe(toMatch, aHash);
	}
/*
udanax-top.st:6715:GrandNode methodsFor: 'accessing'!
{void} wipe: toMatch {Heaper | Position} with: aHash {UInt32} 
	((primaryPages fetch: aHash \\ numPrimaries) cast: GrandDataPage)
		wipe: toMatch with: aHash with: numPrimaries.
	oldOverflowRoot ~~ NULL ifTrue:
		[oldOverflowRoot wipe: toMatch with: aHash]!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print("GrandNode(numPages=");
	aStream.print(numPrimaries);
	aStream.print(")");
/*
udanax-top.st:6723:GrandNode methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << 'GrandNode(numPages=' << numPrimaries << ')'!
*/
}
public GrandNode() {
	super();
	GrandDataPage aPage;
	overflowRoot = null;
	oldOverflowRoot = null;
	numReinserters = 0;
	numPrimaries = 1;
	primaryPages = PtrArray.nulls(1);
	aPage = GrandDataPage.make(GrandNode.primaryPageSize(), this, 0);
	primaryPages.store(0, aPage);
	newShepherd();
	remember();
/*
udanax-top.st:6728:GrandNode methodsFor: 'protected: creation'!
create
	| aPage {GrandDataPage} |
	super create.
	overflowRoot _ NULL.
	oldOverflowRoot _ NULL.
	numReinserters _ Int32Zero.
	numPrimaries _ 1.
	primaryPages _ PtrArray nulls: 1.
	aPage _ GrandDataPage make: GrandNode primaryPageSize with: self with: UInt32Zero.
	primaryPages at: Int32Zero store: aPage.
	self newShepherd.
	self remember!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(2 + numPrimaries);
	try {
		Heaper page;
		if (primaryPages != null) {
			for (int i = 0; i < numPrimaries; i ++ ) {
				page = (primaryPages.fetch(i));
				if (page != null) {
					page.destroy();
				}
			}
			primaryPages.destroy();
		}
		if (overflowRoot != null) {
			overflowRoot.destroy();
		}
		if (oldOverflowRoot != null) {
			oldOverflowRoot.destroy();
		}
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:6741:GrandNode methodsFor: 'protected: creation'!
{void} dismantle
	DiskManager consistent: 2 + numPrimaries with:
		[| page {Heaper} |
		primaryPages ~~ NULL ifTrue:
			[Int32Zero almostTo: numPrimaries do: [:i {Int32} |
				page _ (primaryPages fetch: i).
				page ~~ NULL ifTrue:
				[page destroy]].
			primaryPages destroy].
		overflowRoot ~~ NULL 
			ifTrue: [overflowRoot destroy].
		oldOverflowRoot ~~ NULL
			ifTrue: [oldOverflowRoot destroy].
		super dismantle]!
*/
}
public void addReinserter() {
	AboraBlockSupport.enterConsistent(1);
	try {
		numReinserters = numReinserters + 1;
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:6758:GrandNode methodsFor: 'node doubling'!
{void} addReinserter
	DiskManager consistent: 1 with:
		[numReinserters _ numReinserters + 1.
		self diskUpdate]!
*/
}
public void doubleNode() {
	GrandDataPage newPage;
	int newNumPrimaries;
	PtrArray newPrimaries;
	AboraBlockSupport.enterConsistent(doubleNodeConsistency());
	try {
		newNumPrimaries = numPrimaries * 2;
		newPrimaries = PtrArray.nulls(newNumPrimaries);
		for (int i = 0; i < numPrimaries; i ++ ) {
			newPage = ((GrandDataPage) (primaryPages.fetch(i))).makeDouble(newNumPrimaries);
			newPrimaries.store(i, (primaryPages.fetch(i)));
			newPrimaries.store(newPage.lowHashBits(), newPage);
		}
		primaryPages.destroy();
		primaryPages = newPrimaries;
		numPrimaries = newNumPrimaries;
		/* At this point, the structure is consistent, but still doesn't have the full benefit of the node
		doubling.  Inserts will be faster now, but reinsertion of the overflow data is required for fetch
		to improve. */
		if (overflowRoot != null) {
			if (oldOverflowRoot != null) {
				throw new AboraRuntimeException(AboraRuntimeException.FALLEN_BEHIND_IN_NODE_DOUBLING);
			}
			oldOverflowRoot = overflowRoot;
			overflowRoot = null;
			(GrandNodeReinserter.make(this, oldOverflowRoot)).schedule();
		}
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:6763:GrandNode methodsFor: 'node doubling'!
{void} doubleNode
	| newPage {GrandDataPage} newNumPrimaries {Int32} newPrimaries {PtrArray of: GrandDataPage} |
	
	DiskManager consistent: self doubleNodeConsistency with: 
		[newNumPrimaries _ numPrimaries * 2.
		newPrimaries _ PtrArray nulls: newNumPrimaries.
		Int32Zero almostTo: numPrimaries do:
			[:i {Int32} | 
			newPage _ ((primaryPages fetch: i) cast: GrandDataPage) makeDouble: newNumPrimaries.
			newPrimaries at: i store: (primaryPages fetch: i).
			newPrimaries at: newPage lowHashBits store: newPage].
		primaryPages destroy.
		primaryPages _ newPrimaries.
		numPrimaries _ newNumPrimaries.
		"At this point, the structure is consistent, but still doesn't have the full benefit of the node
		doubling.  Inserts will be faster now, but reinsertion of the overflow data is required for fetch
		to improve."
		overflowRoot ~~ NULL ifTrue:
			[oldOverflowRoot ~~ NULL ifTrue:
				[Heaper BLAST: #FallenBehindInNodeDoubling].
			oldOverflowRoot _ overflowRoot.
			overflowRoot _ NULL.
			(GrandNodeReinserter make: self with: oldOverflowRoot) schedule].
		self diskUpdate].!
*/
}
public int doubleNodeConsistency() {
	Eric.knownBug();
	/* Sometimes this is off by one in either direction */
	return 2 * numPrimaries + 2;
/*
udanax-top.st:6790:GrandNode methodsFor: 'node doubling'!
{IntegerVar} doubleNodeConsistency
	Eric knownBug. "Sometimes this is off by one in either direction"
	^ 2 * numPrimaries + 2!
*/
}
public void removeReinserter() {
	AboraBlockSupport.enterConsistent(1);
	try {
		numReinserters = numReinserters - 1;
		if (numReinserters == 0) {
			oldOverflowRoot.destroy();
			oldOverflowRoot = null;
		}
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:6794:GrandNode methodsFor: 'node doubling'!
{void} removeReinserter
	DiskManager consistent: 1 with:
		[numReinserters _ numReinserters - 1.
		numReinserters == Int32Zero ifTrue:
			[oldOverflowRoot destroy.
			oldOverflowRoot _ NULL].
		self diskUpdate]!
*/
}
public GrandDataPage pageAt(int idx) {
	return (GrandDataPage) (primaryPages.fetch(idx));
/*
udanax-top.st:6804:GrandNode methodsFor: 'private: friendly access'!
{GrandDataPage} pageAt: idx {IntegerVar}
	^ (primaryPages fetch: idx DOTasLong) cast: GrandDataPage!
*/
}
public int pageCount() {
	return numPrimaries;
/*
udanax-top.st:6807:GrandNode methodsFor: 'private: friendly access'!
{IntegerVar} pageCount
	^ numPrimaries!
*/
}
public int contentsHash() {
	int result;
	result = ((super.contentsHash() ^ primaryPages.contentsHash()) ^ (IntegerPos.integerHash(numPrimaries)));
	if (overflowRoot != null) {
		result = result ^ overflowRoot.hashForEqual();
	}
	if (oldOverflowRoot != null) {
		result = result ^ oldOverflowRoot.hashForEqual();
	}
	return result;
/*
udanax-top.st:6812:GrandNode methodsFor: 'testing'!
{UInt32} contentsHash
	| result {UInt32} |
	
	result _ ((super contentsHash
		bitXor: primaryPages contentsHash)
		bitXor: (IntegerPos integerHash: numPrimaries)).
	
	overflowRoot ~~ NULL ifTrue:
		[result _ result bitXor: overflowRoot hashForEqual].
		
	oldOverflowRoot ~~ NULL ifTrue:
		[result _ result bitXor: oldOverflowRoot hashForEqual].
		
	^ result!
*/
}
public boolean isEmpty() {
	for (int i = 0; i < numPrimaries; i ++ ) {
		if ( ! (((GrandDataPage) (primaryPages.fetch(i))).isEmpty())) {
			return false;
		}
	}
	return overflowRoot == null && (oldOverflowRoot == null);
/*
udanax-top.st:6827:GrandNode methodsFor: 'testing'!
{BooleanVar} isEmpty
	UInt32Zero almostTo: numPrimaries do: [ :i {UInt32} |
		((primaryPages fetch: i) cast: GrandDataPage) isEmpty ifFalse: [ ^ false ]].
	^ overflowRoot == NULL and: [oldOverflowRoot == NULL]!
*/
}
/*
udanax-top.st:6834:GrandNode methodsFor: 'smalltalk: inspection'!
inspect
	EntView make: self!
*/
/*
udanax-top.st:6837:GrandNode methodsFor: 'smalltalk: inspection'!
inspectPieces
	| result |
	result _ primaryPages asOrderedCollection.
	overflowRoot ~~ NULL ifTrue:
		[result add: overflowRoot].
	oldOverflowRoot ~~ NULL ifTrue:
		[result add: oldOverflowRoot].
	^result!
*/
public GrandOverflow fetchOldOverflow() {
	return oldOverflowRoot;
/*
udanax-top.st:6848:GrandNode methodsFor: 'overflow'!
{GrandOverflow} fetchOldOverflow
	^ oldOverflowRoot!
*/
}
public GrandOverflow fetchOverflow() {
	return overflowRoot;
/*
udanax-top.st:6851:GrandNode methodsFor: 'overflow'!
{GrandOverflow} fetchOverflow
	
	^overflowRoot!
*/
}
public GrandOverflow getOverflow() {
	if (overflowRoot == null) {
		AboraBlockSupport.enterConsistent(2);
		try {
			overflowRoot = new GrandOverflow(OverflowPageSize, 1);
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return overflowRoot;
/*
udanax-top.st:6855:GrandNode methodsFor: 'overflow'!
{GrandOverflow} getOverflow
	
	overflowRoot == NULL
		ifTrue: 
			[DiskManager consistent: 2 with:
				[overflowRoot _ GrandOverflow create: OverflowPageSize with: 1.
				self diskUpdate]].
	^overflowRoot!
*/
}
public double loadFactor() {
	double loadSum;
	loadSum = 0.0;
	for (int i = 0; i < numPrimaries; i ++ ) {
		loadSum = loadSum + (((GrandDataPage) (primaryPages.fetch(i))).loadFactor());
	}
	return loadSum / numPrimaries;
/*
udanax-top.st:6866:GrandNode methodsFor: 'special'!
{IEEEDoubleVar} loadFactor
	| loadSum {IEEEDoubleVar} |
	loadSum _ 0.0.
	Int32Zero almostTo: numPrimaries do: [ :i {Int32} |
		loadSum _ loadSum + (((primaryPages fetch: i) cast: GrandDataPage) loadFactor)].
	^ loadSum / numPrimaries!
*/
}
public GrandNode(Rcvr receiver) {
	super(receiver);
	primaryPages = (PtrArray) receiver.receiveHeaper();
	numPrimaries = receiver.receiveInt32();
	overflowRoot = (GrandOverflow) receiver.receiveHeaper();
	oldOverflowRoot = (GrandOverflow) receiver.receiveHeaper();
	numReinserters = receiver.receiveInt32();
/*
udanax-top.st:6875:GrandNode methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	primaryPages _ receiver receiveHeaper.
	numPrimaries _ receiver receiveInt32.
	overflowRoot _ receiver receiveHeaper.
	oldOverflowRoot _ receiver receiveHeaper.
	numReinserters _ receiver receiveInt32.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(primaryPages);
	xmtr.sendInt32(numPrimaries);
	xmtr.sendHeaper(overflowRoot);
	xmtr.sendHeaper(oldOverflowRoot);
	xmtr.sendInt32(numReinserters);
/*
udanax-top.st:6883:GrandNode methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: primaryPages.
	xmtr sendInt32: numPrimaries.
	xmtr sendHeaper: overflowRoot.
	xmtr sendHeaper: oldOverflowRoot.
	xmtr sendInt32: numReinserters.!
*/
}
public static void linkTimeNonInherited() {
	OverflowPageSize = 8;
/*
udanax-top.st:6906:GrandNode class methodsFor: 'smalltalk: smalltalk initialization'!
linkTimeNonInherited
	OverflowPageSize _ 8!
*/
}
public static GrandNode make() {
	return new GrandNode();
/*
udanax-top.st:6911:GrandNode class methodsFor: 'create'!
make
	^ self create!
*/
}
public static int primaryPageSize() {
	return 128;
/*
udanax-top.st:6916:GrandNode class methodsFor: 'static functions'!
{Int32 INLINE} primaryPageSize
	^ 128!
*/
}
}
