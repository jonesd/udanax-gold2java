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
 * GrandDataPage behaves as a small hash table.
 * Linear hashing and the GrandOverflow structure are used to resolve collisions.
 * The shift argument to the various methods is the number of pages in the
 * parent node to indicate how many low bits of the hash are ignored.
 */
public class GrandDataPage extends Abraham {

	protected int myLowHashBits;
	protected int numEntries;
	protected PtrArray entries;
	protected GrandOverflow overflow;
	protected GrandNode myGroup;
/*
udanax-top.st:6261:
Abraham subclass: #GrandDataPage
	instanceVariableNames: '
		myLowHashBits {UInt32}
		numEntries {Int32}
		entries {PtrArray of: GrandEntry}
		overflow {GrandOverflow}
		myGroup {GrandNode}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:6270:
GrandDataPage comment:
'GrandDataPage behaves as a small hash table.
Linear hashing and the GrandOverflow structure are used to resolve collisions.
The shift argument to the various methods is the number of pages in the
parent node to indicate how many low bits of the hash are ignored.'!
*/
/*
udanax-top.st:6275:
(GrandDataPage getOrMakeCxxClassDescription)
	friends:
'/- friends for class GrandDataPage -/
friend class GrandDataPageStepper;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:6502:
GrandDataPage class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:6505:
(GrandDataPage getOrMakeCxxClassDescription)
	friends:
'/- friends for class GrandDataPage -/
friend class GrandDataPageStepper;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandDataPage.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch(Heaper toMatch, int aHash, int shift) {
	int localIndex;
	int originalIndex;
	GrandEntry entry;
	localIndex = originalIndex = AboraSupport.modulo(aHash / shift, numEntries);
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
			/* break */
			;
		}
	}
	if (overflow != null) {
		return overflow.fetch(toMatch, aHash);
	}
	return null;
/*
udanax-top.st:6284:GrandDataPage methodsFor: 'accessing'!
{Heaper} fetch: toMatch {Heaper | Position} with: aHash {UInt32} with: shift {Int32}
	| localIndex {Int32} originalIndex {Int32} entry {GrandEntry} |
	localIndex _ originalIndex _ aHash // shift \\ numEntries.
	entry _ (entries fetch: localIndex) cast: GrandEntry.
	[entry ~~ NULL] whileTrue:
		[(aHash == entry hashForEqual) ifTrue: [(entry compare: toMatch) ifTrue: [^entry value]].
		localIndex _ localIndex + 1 \\ numEntries.
		entry _ (entries fetch: localIndex) cast: GrandEntry.
		localIndex == originalIndex ifTrue: [ entry _ NULL "break" ]].
	overflow ~~ NULL ifTrue: [ ^ overflow fetch: toMatch with: aHash].
	^NULL!
*/
}
public void storeEntry(GrandEntry newEntry, int shift) {
	int localIndex;
	int originalIndex;
	GrandEntry entry;
	localIndex = originalIndex = AboraSupport.modulo(newEntry.hashForEqual() / shift, numEntries);
	entry = (GrandEntry) (entries.fetch(localIndex));
	while (entry != null) {
		if (newEntry.hashForEqual() == entry.hashForEqual()) {
			if (newEntry.matches(entry)) {
				/* Note that this does not delete the contents */
				AboraBlockSupport.enterConsistent(1);
				try {
					entry.destroy();
					entries.store(localIndex, newEntry);
					diskUpdate();
				}
				finally {
					AboraBlockSupport.exitConsistent();
				}
				return ;
			}
		}
		localIndex = AboraSupport.modulo(localIndex + 1, numEntries);
		if (localIndex == originalIndex) {
			/* This page is now full */
			if (overflow == null) {
				AboraBlockSupport.enterConsistent(4);
				try {
					overflow = myGroup.getOverflow().storeEntry(newEntry);
					diskUpdate();
				}
				finally {
					AboraBlockSupport.exitConsistent();
				}
			}
			else {
				overflow.storeEntry(newEntry);
			}
			return ;
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
/*
udanax-top.st:6296:GrandDataPage methodsFor: 'accessing'!
{void} store.Entry: newEntry {GrandEntry} with: shift {Int32} 
	
	| localIndex {UInt32} originalIndex {UInt32} entry {GrandEntry wimpy} |
	localIndex _ originalIndex _ newEntry hashForEqual // shift \\ numEntries.
	entry _ (entries fetch: localIndex) cast: GrandEntry.
	
	[entry ~~ NULL] whileTrue:
		[newEntry hashForEqual == entry hashForEqual ifTrue:
			[(newEntry matches: entry) ifTrue:
				["Note that this does not delete the contents"
				DiskManager consistent: 1 with:
					[entry destroy.
					entries at: localIndex store: newEntry.
					self diskUpdate].
				^VOID]].
		localIndex _ localIndex + 1 \\ numEntries.
		localIndex == originalIndex ifTrue:
			["This page is now full"
			overflow == NULL
				ifTrue:
					[DiskManager consistent: 4 with:
						[overflow _ myGroup getOverflow store.Entry: newEntry.
						self diskUpdate]]
				ifFalse:
					[overflow store.Entry: newEntry].
				^VOID].
			entry _ (entries fetch: localIndex) cast: GrandEntry].
	"Found empty slot."
	DiskManager consistent: 1 with:
		[entries at: localIndex store: newEntry.
		self diskUpdate]!
*/
}
public void wipe(Heaper toMatch, int aHash, int shift) {
	int localIndex;
	int originalIndex;
	GrandEntry entry;
	localIndex = originalIndex = AboraSupport.modulo(aHash / shift, numEntries);
	entry = (GrandEntry) (entries.fetch(localIndex));
	while (entry != null) {
		if (aHash == entry.hashForEqual()) {
			if (entry.compare(toMatch)) {
				AboraBlockSupport.enterConsistent(2);
				try {
					entry.destroy();
					/* Note that this does not delete the contents */
					entries.store(localIndex, null);
					repack(shift);
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
			/* break */
			entry = null;
		}
	}
	if (overflow != null) {
		overflow.wipe(toMatch, aHash);
	}
/*
udanax-top.st:6331:GrandDataPage methodsFor: 'accessing'!
{void} wipe: toMatch {Heaper | Position} with: aHash {UInt32} with: shift {Int32} 
	
	| localIndex {Int32} originalIndex {Int32} entry {GrandEntry wimpy} |
	localIndex _ originalIndex _ aHash // shift \\ numEntries.
	entry _ (entries fetch: localIndex) cast: GrandEntry.
	[entry ~~ NULL] whileTrue:
		[aHash == entry hashForEqual ifTrue:
			[(entry compare: toMatch) ifTrue:
				[DiskManager consistent: 2 with:
					[entry destroy.
					"Note that this does not delete the contents"
					entries at: localIndex store: NULL.
					self repack: shift.
					self diskUpdate].
				^VOID]].
		localIndex _ localIndex + 1 \\ numEntries.
		entry _ (entries fetch: localIndex) cast: GrandEntry.
		localIndex = originalIndex ifTrue:
			["break"
			entry _ NULL]].
			
	overflow ~~ NULL ifTrue:
		[overflow wipe: toMatch with: aHash]!
*/
}
public GrandDataPage(int nEntries, GrandNode node, int lowHashBits) {
	super();
	myLowHashBits = lowHashBits;
	numEntries = nEntries;
	entries = PtrArray.nulls(numEntries);
	myGroup = node;
	overflow = null;
	newShepherd();
	remember();
/*
udanax-top.st:6359:GrandDataPage methodsFor: 'protected: creation'!
create: nEntries {Int32} with: node {GrandNode} with: lowHashBits {UInt32}
	super create.
	myLowHashBits _ lowHashBits.
	numEntries _ nEntries.
	entries _ PtrArray nulls: numEntries.
	myGroup _ node.
	overflow _ NULL.
	self newShepherd.
	self remember!
*/
}
/**
 * This repacks the entry table after a wipe to keep the table consistent with
 */
public void repack(int shift) {
	/* the linear hash collision resolution technique. */
	PtrArray newEntries;
	GrandEntry entry;
	int preferedIndex;
	newEntries = PtrArray.nulls(numEntries);
	for (int i = 0; i < numEntries; i ++ ) {
		if ((entry = (GrandEntry) (entries.fetch(i))) != null) {
			preferedIndex = AboraSupport.modulo(entry.hashForEqual() / shift, numEntries);
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
udanax-top.st:6371:GrandDataPage methodsFor: 'private: private'!
{void} repack: shift {Int32}
	"This repacks the entry table after a wipe to keep the table consistent with"
	"the linear hash collision resolution technique."
	| newEntries {PtrArray of: GrandEntry} entry {GrandEntry} preferedIndex {Int32} |
	newEntries _ PtrArray nulls: numEntries.
	Int32Zero almostTo: numEntries do: [ :i {Int32} |
		(entry _ (entries fetch: i) cast: GrandEntry) ~~ NULL ifTrue:
			[preferedIndex _ entry hashForEqual // shift \\ numEntries.
			(newEntries fetch: preferedIndex) ~~ NULL ifTrue:
				[[(newEntries fetch: preferedIndex) ~~ NULL] whileTrue:
					[preferedIndex _ preferedIndex + 1 \\ numEntries]].
			newEntries at: preferedIndex store: entry]].
	entries destroy.
	entries _ newEntries.!
*/
}
/**
 * Create a new page with all entries of current page that have a
 */
public GrandDataPage makeDouble(int newNumPages) {
	/* '1' in the new lowest significant bit of the hash. */
	/* Retain all '0' entries in this page. */
	GrandDataPage newPage;
	GrandEntry oldEntry;
	int oldNumPages;
	AboraBlockSupport.enterConsistent(2);
	try {
		oldNumPages = newNumPages / 2;
		newPage = GrandDataPage.make(numEntries, myGroup, myLowHashBits + oldNumPages);
		overflow = null;
		for (int i = 
		/* Reset overflow structure. Old one is held by parent node. */
		0; i < numEntries; i ++ ) {
			oldEntry = (GrandEntry) (entries.fetch(i));
			/* This test is necessary since page to be doubled may not be full. */
			if (oldEntry != null) {
				if ((oldEntry.hashForEqual() / oldNumPages & 1) == 1) {
					newPage.storeEntry(oldEntry, newNumPages);
					entries.store(i, null);
				}
			}
		}
		/* Now let pages sort themselves out. */
		repack(newNumPages);
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return newPage;
/*
udanax-top.st:6388:GrandDataPage methodsFor: 'node doubling'!
{GrandDataPage} makeDouble: newNumPages {Int32} 
	"Create a new page with all entries of current page that have a"
	"'1' in the new lowest significant bit of the hash."
	"Retain all '0' entries in this page."
	
	| newPage {GrandDataPage} oldEntry {GrandEntry wimpy} oldNumPages {Int32} |
	DiskManager consistent: 2 with:
		[oldNumPages _ newNumPages / 2.
		newPage _ GrandDataPage make: numEntries with: myGroup with: myLowHashBits + oldNumPages.
		overflow _ NULL.
		"Reset overflow structure. Old one is held by parent node."
		Int32Zero
			almostTo: numEntries
			do: [:i {Int32} | 
				oldEntry _ (entries fetch: i) cast: GrandEntry.
				"This test is necessary since page to be doubled may not be full."
				oldEntry ~~ NULL
					ifTrue: 
						[(oldEntry hashForEqual // oldNumPages bitAnd: 1) == 1
							ifTrue: 
								[newPage store.Entry: oldEntry with: newNumPages.
								entries at: i store: NULL]]].
		"Now let pages sort themselves out."
		self repack: newNumPages.
		self diskUpdate].
	^newPage!
*/
}
public double loadFactor() {
	int loadCount;
	loadCount = 0;
	for (int i = 0; i < numEntries; i ++ ) {
		if ((entries.fetch(i)) != null) {
			loadCount = loadCount + 1;
		}
	}
	return (float) loadCount / (float) numEntries;
/*
udanax-top.st:6417:GrandDataPage methodsFor: 'special'!
{IEEEDoubleVar} loadFactor
	| loadCount {Int32} |
	loadCount _ Int32Zero.
	Int32Zero almostTo: numEntries do: [ :i {Int32} |
		(entries fetch: i) ~~ NULL ifTrue: [ loadCount _ loadCount + 1]].
	^ loadCount asFloat / numEntries asFloat!
*/
}
public int lowHashBits() {
	return myLowHashBits;
/*
udanax-top.st:6424:GrandDataPage methodsFor: 'special'!
{UInt32} lowHashBits
	^ myLowHashBits!
*/
}
public void printOn(PrintWriter aStream) {
	int count;
	aStream.print("GrandDataPage(");
	aStream.print(numEntries);
	aStream.print(" slots, ");
	count = 0;
	for (int i = 0; i < numEntries; i ++ ) {
		if ((entries.fetch(i)) != null) {
			count = count + 1;
		}
	}
	aStream.print(count);
	aStream.print(" full");
	if (overflow != null) {
		aStream.print(" and overflow");
	}
	aStream.print(")");
/*
udanax-top.st:6429:GrandDataPage methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	| count {Int32} |
	aStream << 'GrandDataPage(' << numEntries << ' slots, '.
	count _ Int32Zero.
	Int32Zero almostTo: numEntries do: [ :i {Int32} |
		(entries fetch: i) ~~ NULL ifTrue: [ count _ count + 1 ]].
	aStream << count << ' full'.
	overflow ~~ NULL ifTrue: [ aStream << ' and overflow'].
	aStream << ')'!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(1 + numEntries);
	try {
		Heaper entry;
		if (entries != null) {
			for (int i = 0; i < numEntries; i ++ ) {
				entry = entries.fetch(i);
				if (entry != null) {
					entry.destroy();
					entries.store(i, null);
				}
			}
			entries.destroy();
			entries = null;
		}
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:6441:GrandDataPage methodsFor: 'protected: destruction'!
{void} dismantle
	DiskManager consistent: 1 + numEntries with:
		[| entry {Heaper} |
		entries ~~ NULL ifTrue:
			[Int32Zero almostTo: numEntries do: [ :i {Int32} |
				entry _ entries fetch: i.
				entry ~~ NULL ifTrue:
					[entry destroy.
					entries at: i store: NULL]].
			entries destroy.
			entries _ NULL].
		super dismantle]!
*/
}
public int contentsHash() {
	return ((((super.contentsHash() ^ (IntegerPos.integerHash(myLowHashBits))) ^ (IntegerPos.integerHash(numEntries))) ^ entries.contentsHash()) ^ overflow.hashForEqual()) ^ myGroup.hashForEqual();
/*
udanax-top.st:6456:GrandDataPage methodsFor: 'testing'!
{UInt32} contentsHash
	^((((super contentsHash
		bitXor: (IntegerPos integerHash: myLowHashBits))
		bitXor: (IntegerPos integerHash: numEntries))
		bitXor: entries contentsHash)
		bitXor: overflow hashForEqual)
		bitXor: myGroup hashForEqual!
*/
}
public boolean isEmpty() {
	for (int i = 0; i < numEntries; i ++ ) {
		if ((entries.fetch(i)) != null) {
			return false;
		}
	}
	return true;
/*
udanax-top.st:6465:GrandDataPage methodsFor: 'testing'!
{BooleanVar} isEmpty
	UInt32Zero almostTo: numEntries do: [ :i {UInt32} |
		(entries fetch: i) ~~ NULL ifTrue: [ ^ false ]].
	^ true!
*/
}
public GrandEntry entryAt(int idx) {
	return (GrandEntry) (entries.fetch(idx));
/*
udanax-top.st:6472:GrandDataPage methodsFor: 'private: friendly'!
{GrandEntry} entryAt: idx {IntegerVar}
	^(entries fetch: idx DOTasLong) cast: GrandEntry!
*/
}
public int entryCount() {
	return numEntries;
/*
udanax-top.st:6475:GrandDataPage methodsFor: 'private: friendly'!
{IntegerVar} entryCount
	^ numEntries!
*/
}
/*
udanax-top.st:6480:GrandDataPage methodsFor: 'private: smalltalk: private'!
inspectPieces
	^entries asOrderedCollection!
*/
public GrandDataPage(Rcvr receiver) {
	super(receiver);
	myLowHashBits = receiver.receiveUInt32();
	numEntries = receiver.receiveInt32();
	entries = (PtrArray) receiver.receiveHeaper();
	overflow = (GrandOverflow) receiver.receiveHeaper();
	myGroup = (GrandNode) receiver.receiveHeaper();
/*
udanax-top.st:6485:GrandDataPage methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myLowHashBits _ receiver receiveUInt32.
	numEntries _ receiver receiveInt32.
	entries _ receiver receiveHeaper.
	overflow _ receiver receiveHeaper.
	myGroup _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendUInt32(myLowHashBits);
	xmtr.sendInt32(numEntries);
	xmtr.sendHeaper(entries);
	xmtr.sendHeaper(overflow);
	xmtr.sendHeaper(myGroup);
/*
udanax-top.st:6493:GrandDataPage methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendUInt32: myLowHashBits.
	xmtr sendInt32: numEntries.
	xmtr sendHeaper: entries.
	xmtr sendHeaper: overflow.
	xmtr sendHeaper: myGroup.!
*/
}
public static GrandDataPage make(int nEntries, GrandNode node, int lowHashBits) {
	return new GrandDataPage(nEntries, node, lowHashBits);
/*
udanax-top.st:6514:GrandDataPage class methodsFor: 'creation'!
make: nEntries {Int32} with: node {GrandNode} with: lowHashBits {UInt32}
	^ self create: nEntries with: node with: lowHashBits!
*/
}
public GrandDataPage() {
/*

Generated during transformation
*/
}
}
