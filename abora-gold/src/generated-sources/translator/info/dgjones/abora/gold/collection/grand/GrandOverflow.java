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
import info.dgjones.abora.gold.collection.grand.GrandEntry;
import info.dgjones.abora.gold.collection.grand.GrandNode;
import info.dgjones.abora.gold.collection.grand.GrandOverflow;
import info.dgjones.abora.gold.grantab.GrandNodeReinserter;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * This class has a comment
 * The instance variable depth actually holds the value OTreeArity ^ depth.
 */
public class GrandOverflow extends Abraham {

	protected int numEntries;
	protected PtrArray entries;
	protected PtrArray children;
	protected int depth;
	protected static int OTreeArity;
/*
udanax-top.st:6919:
Abraham subclass: #GrandOverflow
	instanceVariableNames: '
		numEntries {Int32}
		entries {PtrArray of: GrandEntry}
		children {PtrArray of: GrandOverflow}
		depth {Int32}'
	classVariableNames: 'OTreeArity {Int32} '
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:6927:
GrandOverflow comment:
'This class has a comment
The instance variable depth actually holds the value OTreeArity ^ depth.'!
*/
/*
udanax-top.st:6930:
(GrandOverflow getOrMakeCxxClassDescription)
	friends:
'/- friends for class GrandOverflow -/
friend class GrandOverflowStepper;';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:7140:
GrandOverflow class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:7143:
(GrandOverflow getOrMakeCxxClassDescription)
	friends:
'/- friends for class GrandOverflow -/
friend class GrandOverflowStepper;';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandOverflow.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch(Heaper toMatch, int aHash) {
	int localIndex;
	int originalIndex;
	GrandEntry entry;
	int childIndex;
	localIndex = originalIndex = AboraSupport.modulo(aHash / depth, numEntries);
	entry = (GrandEntry) (entries.fetch(localIndex));
	while (entry != null) {
		if (aHash == entry.hashForEqual()) {
			if (entry.compare(toMatch)) {
				return entry.value();
			}
		}
		localIndex = AboraSupport.modulo(localIndex + 1, numEntries);
		entry = (GrandEntry) (entries.fetch(localIndex));
		if (localIndex == originalIndex) {
			entry = null
			/* break from loop */
			;
		}
	}
	childIndex = AboraSupport.modulo(aHash / depth, OTreeArity);
	if ((children.fetch(childIndex)) != null) {
		return ((GrandOverflow) (children.fetch(childIndex))).fetch(toMatch, aHash);
	}
	return null;
/*
udanax-top.st:6938:GrandOverflow methodsFor: 'accessing'!
{Heaper} fetch: toMatch {Heaper | Position} with: aHash {UInt32}
	| localIndex {Int32} originalIndex {Int32} entry {GrandEntry}  childIndex {UInt32} |
	localIndex _ originalIndex _ aHash // depth \\ numEntries.
	entry _ (entries fetch: localIndex) cast: GrandEntry.
	[entry ~~ NULL] whileTrue:
		[(aHash == entry hashForEqual) ifTrue: [(entry compare: toMatch) ifTrue: [^ entry value]].
		localIndex _ localIndex + 1 \\ numEntries.
		entry _ (entries fetch: localIndex) cast: GrandEntry.
		localIndex == originalIndex ifTrue:
			[entry _ NULL "break from loop"]].
	childIndex _ aHash // depth \\ OTreeArity.
	(children fetch: childIndex) ~~ NULL ifTrue:
			[^ ((children fetch: childIndex) cast: GrandOverflow) fetch: toMatch with: aHash].
	^NULL!
*/
}
public GrandOverflow storeEntry(GrandEntry newEntry) {
	int localIndex;
	int originalIndex;
	GrandEntry entry;
	localIndex = originalIndex = AboraSupport.modulo(newEntry.hashForEqual() / depth, numEntries);
	entry = (GrandEntry) (entries.fetch(localIndex));
	while (entry != null) {
		if (newEntry.hashForEqual() == entry.hashForEqual()) {
			if (newEntry.matches(entry)) {
				/* Note that this does not delete the contents */
				AboraBlockSupport.enterConsistent(2);
				try {
					entry.destroy();
					entries.store(localIndex, newEntry);
					diskUpdate();
				}
				finally {
					AboraBlockSupport.exitConsistent();
				}
				return this;
			}
		}
		localIndex = AboraSupport.modulo(localIndex + 1, numEntries);
		if (localIndex == originalIndex) {
			GrandOverflow newChild;
			int childIndex;
			/* This page is now full. Descend overflow tree further. */
			childIndex = AboraSupport.modulo(newEntry.hashForEqual() / depth, OTreeArity);
			if ((children.fetch(childIndex)) == null) {
				AboraBlockSupport.enterConsistent(2);
				try {
					newChild = new GrandOverflow(numEntries, depth * OTreeArity);
					children.store(childIndex, newChild);
					diskUpdate();
				}
				finally {
					AboraBlockSupport.exitConsistent();
				}
			}
			return ((GrandOverflow) (children.fetch(childIndex))).storeEntry(newEntry);
		}
		entry = (GrandEntry) (entries.fetch(localIndex));
	}
	/* Found empty slot. */
	AboraBlockSupport.enterConsistent(1);
	try {
		entries.store(localIndex, newEntry);
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return this;
/*
udanax-top.st:6953:GrandOverflow methodsFor: 'accessing'!
{GrandOverflow} store.Entry: newEntry {GrandEntry} 
	
	| localIndex {Int32} originalIndex {Int32} entry {GrandEntry wimpy} |
	localIndex _ originalIndex _ newEntry hashForEqual // depth \\ numEntries.
	entry _ (entries fetch: localIndex) cast: GrandEntry.
	[entry ~~ NULL] whileTrue:
		[newEntry hashForEqual == entry hashForEqual ifTrue:
			[(newEntry matches: entry) ifTrue:
				["Note that this does not delete the contents"
				DiskManager consistent: 2 with:
					[entry destroy.
					entries at: localIndex store: newEntry.
					self diskUpdate].
				^self]].
		localIndex _ localIndex + 1 \\ numEntries.
		localIndex == originalIndex ifTrue:
			[| newChild {GrandOverflow} childIndex {UInt32} |
			"This page is now full. Descend overflow tree further."
			childIndex _ newEntry hashForEqual // depth \\ OTreeArity.
			(children fetch: childIndex) == NULL ifTrue:
				[DiskManager consistent: 2 with:
					[newChild _ GrandOverflow create: numEntries with: depth * OTreeArity.
					children at: childIndex store: newChild.
					self diskUpdate]].
			^((children fetch: childIndex) cast: GrandOverflow) store.Entry: newEntry].
		entry _ (entries fetch: localIndex) cast: GrandEntry].
	"Found empty slot."
	DiskManager consistent: 1 with:
		[entries at: localIndex store: newEntry.
		self diskUpdate].
	^self!
*/
}
public void wipe(Heaper toMatch, int aHash) {
	int localIndex;
	int originalIndex;
	int childIndex;
	GrandEntry entry;
	localIndex = originalIndex = AboraSupport.modulo(aHash / depth, numEntries);
	entry = (GrandEntry) (entries.fetch(localIndex));
	while (entry != null) {
		if (aHash == entry.hashForEqual()) {
			if (entry.compare(toMatch)) {
				/* Note that this does not delete the contents */
				AboraBlockSupport.enterConsistent(2);
				try {
					entry.destroy();
					entries.store(localIndex, null);
					repack();
					diskUpdate();
				}
				finally {
					AboraBlockSupport.exitConsistent();
				}
				return ;
			}
		}
		localIndex = AboraSupport.modulo(localIndex + 1, numEntries);
		entry = (GrandEntry) (entries.fetch(localIndex));
		if (localIndex == originalIndex) {
			/* break from loop */
			entry = null;
		}
	}
	childIndex = AboraSupport.modulo(aHash / depth, OTreeArity);
	if ((children.fetch(childIndex)) != null) {
		((GrandOverflow) (children.fetch(childIndex))).wipe(toMatch, aHash);
	}
/*
udanax-top.st:6991:GrandOverflow methodsFor: 'accessing'!
{void} wipe: toMatch {Heaper | Position} with: aHash {UInt32} 
	
	| localIndex {Int32} originalIndex {Int32} childIndex {Int32} entry {GrandEntry wimpy} |
	localIndex _ originalIndex _ aHash // depth \\ numEntries.
	entry _ (entries fetch: localIndex) cast: GrandEntry.
	[entry ~~ NULL]
		whileTrue: 
			[aHash == entry hashForEqual ifTrue:
				[(entry compare: toMatch) ifTrue:
					["Note that this does not delete the contents"
					DiskManager consistent: 2 with:
						[entry destroy.
						entries at: localIndex store: NULL.
						self repack.
						self diskUpdate].
					^ VOID]].
			localIndex _ localIndex + 1 \\ numEntries.
			entry _ (entries fetch: localIndex) cast: GrandEntry.
			localIndex == originalIndex ifTrue:
				["break from loop"
				entry _ NULL]].
	childIndex _ aHash // depth \\ OTreeArity.
	(children fetch: childIndex) ~~ NULL
		ifTrue: [((children fetch: childIndex) cast: GrandOverflow) wipe: toMatch with: aHash]!
*/
}
public GrandOverflow(int maxEntries, int someDepth) {
	super();
	numEntries = maxEntries;
	entries = PtrArray.nulls(numEntries);
	children = PtrArray.nulls(OTreeArity);
	depth = someDepth;
	newShepherd();
	remember();
/*
udanax-top.st:7019:GrandOverflow methodsFor: 'creation'!
create: maxEntries {Int32} with: someDepth {UInt32}
	super create.
	numEntries _ maxEntries.
	entries _ PtrArray nulls: numEntries.
	children _ PtrArray nulls: OTreeArity.
	depth _ someDepth.
	self newShepherd.
	self remember!
*/
}
/**
 * This repacks the entry table after a wipe to keep the table consistent with
 */
public void repack() {
	/* the linear hash collision resolution technique. */
	PtrArray newEntries;
	GrandEntry entry;
	int preferedIndex;
	newEntries = PtrArray.nulls(numEntries);
	for (int i = 0; i < numEntries; i ++ ) {
		if ((entry = (GrandEntry) (entries.fetch(i))) != null) {
			preferedIndex = AboraSupport.modulo(entry.hashForEqual() / depth, numEntries);
			if ((newEntries.fetch(preferedIndex)) != null) {
				while ((newEntries.fetch(preferedIndex)) != null) {
					preferedIndex = AboraSupport.modulo(preferedIndex + 1, numEntries);
				}
			}
			newEntries.store(preferedIndex, entry);
		}
	}
	entries.destroy();
	entries = newEntries;
/*
udanax-top.st:7030:GrandOverflow methodsFor: 'private:'!
{void} repack
	"This repacks the entry table after a wipe to keep the table consistent with"
	"the linear hash collision resolution technique."
	| newEntries {PtrArray of: GrandEntry} entry {GrandEntry} preferedIndex {Int32} |
	newEntries _ PtrArray nulls: numEntries.
	Int32Zero almostTo: numEntries do: [ :i {Int32} |
		(entry _ (entries fetch: i) cast: GrandEntry) ~~ NULL ifTrue:
			[preferedIndex _ entry hashForEqual // depth \\ numEntries.
			(newEntries fetch: preferedIndex) ~~ NULL ifTrue:
				[[(newEntries fetch: preferedIndex) ~~ NULL] whileTrue:
					[preferedIndex _ preferedIndex + 1 \\ numEntries]].
			newEntries at: preferedIndex store: entry]].
	entries destroy.
	entries _ newEntries!
*/
}
/**
 * Recursively insert all overflowed entries into a newly doubled node.
 */
public void reinsertEntries(GrandNode node) {
	GrandEntry entry;
	GrandOverflow child;
	AboraBlockSupport.enterConsistent(reinsertEntriesConsistency());
	try {
		for (int i = 0; i < numEntries; i ++ ) {
			entry = (GrandEntry) (entries.fetch(i));
			if (entry != null) {
				node.storeEntry(entry);
				entries.store(i, null);
				diskUpdate();
			}
		}
		for (int j = 0; j < OTreeArity; j ++ ) {
			child = (GrandOverflow) (children.fetch(j));
			if (child != null) {
				(GrandNodeReinserter.make(node, child)).schedule();
			}
		}
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:7047:GrandOverflow methodsFor: 'node doubling'!
{void} reinsertEntries: node {GrandNode}
	"Recursively insert all overflowed entries into a newly doubled node."
	| entry {GrandEntry} child {GrandOverflow} |
	DiskManager consistent: self reinsertEntriesConsistency with:
		[Int32Zero almostTo: numEntries do:
			[ :i {Int32} |
			entry _ (entries fetch: i) cast: GrandEntry.
			entry ~~ NULL ifTrue:
				[node store.Entry: entry.
				entries at: i store: NULL.
				self diskUpdate]].
		Int32Zero almostTo: OTreeArity do: [ :j {Int32} |
			child _ (children fetch: j) cast: GrandOverflow.
			child ~~ NULL ifTrue: [(GrandNodeReinserter make: node with: child) schedule]]]!
*/
}
public int reinsertEntriesConsistency() {
	return 4 * numEntries + OTreeArity + 2;
/*
udanax-top.st:7064:GrandOverflow methodsFor: 'node doubling'!
{IntegerVar} reinsertEntriesConsistency
	^ 4 * numEntries + OTreeArity + 2!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print("GrandOverflow(depth=");
	aStream.print(depth);
	aStream.print(")");
/*
udanax-top.st:7069:GrandOverflow methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << 'GrandOverflow(depth=' << depth << ')'!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(1 + numEntries + OTreeArity);
	try {
		if (entries != null) {
			for (int i = 0; i < numEntries; i ++ ) {
				GrandEntry entry;
				entry = (GrandEntry) (entries.fetch(i));
				if (entry != null) {
					entry.destroy();
				}
			}
			entries.destroy();
		}
		if (children != null) {
			for (int j = 0; j < OTreeArity; j ++ ) {
				GrandOverflow child;
				child = (GrandOverflow) (children.fetch(j));
				if (child != null) {
					child.destroy();
				}
			}
			children.destroy();
		}
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:7074:GrandOverflow methodsFor: 'protected: creation'!
{void} dismantle
	DiskManager consistent: 1 + numEntries + OTreeArity with:
		[entries ~~ NULL ifTrue:
			[Int32Zero almostTo: numEntries do: [ :i {Int32} |
				| entry {GrandEntry} |
				entry _ (entries fetch: i) cast: GrandEntry.
				entry ~~ NULL ifTrue:
					[entry destroy]].
			entries destroy].
		children ~~ NULL ifTrue:
			[Int32Zero almostTo: OTreeArity do: [ :j {Int32} |
				| child {GrandOverflow} |
				child _ (children fetch: j) cast: GrandOverflow.
				child ~~ NULL ifTrue:
					[child destroy]].
			children destroy].
		super dismantle]!
*/
}
public GrandOverflow childAt(int idx) {
	return (GrandOverflow) (children.fetch(idx));
/*
udanax-top.st:7094:GrandOverflow methodsFor: 'private: friendly'!
{GrandOverflow} childAt: idx {IntegerVar}
	^ (children fetch: idx DOTasLong) cast: GrandOverflow!
*/
}
public int childCount() {
	return OTreeArity;
/*
udanax-top.st:7097:GrandOverflow methodsFor: 'private: friendly'!
{IntegerVar} childCount
	^ OTreeArity!
*/
}
public GrandEntry entryAt(int idx) {
	return (GrandEntry) (entries.fetch(idx));
/*
udanax-top.st:7100:GrandOverflow methodsFor: 'private: friendly'!
{GrandEntry} entryAt: idx {IntegerVar}
	^ (entries fetch: idx DOTasLong) cast: GrandEntry!
*/
}
public int entryCount() {
	return numEntries;
/*
udanax-top.st:7103:GrandOverflow methodsFor: 'private: friendly'!
{IntegerVar} entryCount
	^ numEntries!
*/
}
/*
udanax-top.st:7108:GrandOverflow methodsFor: 'private: smalltalk: private'!
inspectPieces
	^(entries asOrderedCollection)
		addAll: children asOrderedCollection;
		yourself!
*/
public int contentsHash() {
	return (((super.contentsHash() ^ (IntegerPos.integerHash(numEntries))) ^ entries.contentsHash()) ^ children.contentsHash()) ^ (IntegerPos.integerHash(depth));
/*
udanax-top.st:7115:GrandOverflow methodsFor: 'testing'!
{UInt32} contentsHash
	^(((super contentsHash
		bitXor: (IntegerPos integerHash: numEntries))
		bitXor: entries contentsHash)
		bitXor: children contentsHash)
		bitXor: (IntegerPos integerHash: depth)!
*/
}
public GrandOverflow(Rcvr receiver) {
	super(receiver);
	numEntries = receiver.receiveInt32();
	entries = (PtrArray) receiver.receiveHeaper();
	children = (PtrArray) receiver.receiveHeaper();
	depth = receiver.receiveInt32();
/*
udanax-top.st:7125:GrandOverflow methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	numEntries _ receiver receiveInt32.
	entries _ receiver receiveHeaper.
	children _ receiver receiveHeaper.
	depth _ receiver receiveInt32.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendInt32(numEntries);
	xmtr.sendHeaper(entries);
	xmtr.sendHeaper(children);
	xmtr.sendInt32(depth);
/*
udanax-top.st:7132:GrandOverflow methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendInt32: numEntries.
	xmtr sendHeaper: entries.
	xmtr sendHeaper: children.
	xmtr sendInt32: depth.!
*/
}
public static void linkTimeNonInherited() {
	OTreeArity = 4;
/*
udanax-top.st:7151:GrandOverflow class methodsFor: 'smalltalk: smalltalk initialization'!
linkTimeNonInherited
	OTreeArity _ 4!
*/
}
public GrandOverflow() {
/*

Generated during transformation
*/
}
}
