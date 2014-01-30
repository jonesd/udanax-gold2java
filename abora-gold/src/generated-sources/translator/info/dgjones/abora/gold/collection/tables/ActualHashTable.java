/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.tables;

import info.dgjones.abora.gold.collection.basic.SharedPtrArray;
import info.dgjones.abora.gold.collection.settable.TableEntry;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ActualHashTable;
import info.dgjones.abora.gold.collection.tables.HashTable;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.tabtool.LPPrimeSizeProvider;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * The HashTable is an implementation class that is intended to provide the weakest
 * Position->Object mapping.  It can map from arbitrary Position classes (such as
 * HeaperAsPosition or TreePosition).  HashTable can also be used for very sparse integer
 * domains.
 * HashTable, and the entire hashtab module, is private implementation.  Not to be included
 * by clients.
 */
public class ActualHashTable extends HashTable {

	protected SharedPtrArray myHashEntries;
	protected int myTally;
	protected CoordinateSpace myCoordinateSpace;
/*
udanax-top.st:48552:
HashTable subclass: #ActualHashTable
	instanceVariableNames: '
		myHashEntries {SharedPtrArray NOCOPY}
		myTally {Int32}
		myCoordinateSpace {CoordinateSpace}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:48559:
ActualHashTable comment:
'The HashTable is an implementation class that is intended to provide the weakest Position->Object mapping.  It can map from arbitrary Position classes (such as HeaperAsPosition or TreePosition).  HashTable can also be used for very sparse integer domains.
	
	HashTable, and the entire hashtab module, is private implementation.  Not to be included by clients.'!
*/
/*
udanax-top.st:48563:
(ActualHashTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class HashTable -/
friend SPTR(HashTable)  actualHashTable (APTR(CoordinateSpace) cs);
friend SPTR(HashTable)  actualHashTable (APTR(CoordinateSpace) cs, IntegerVar size);
';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:48878:
ActualHashTable class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:48881:
(ActualHashTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class HashTable -/
friend SPTR(HashTable)  actualHashTable (APTR(CoordinateSpace) cs);
friend SPTR(HashTable)  actualHashTable (APTR(CoordinateSpace) cs, IntegerVar size);
';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ActualHashTable.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper store(Position key, Heaper aHeaper) {
	int offset;
	TableEntry entry;
	TableEntry prev;
	if (aHeaper == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);
	}
	aboutToWrite();
	checkSize();
	offset = AboraSupport.modulo(key.hashForEqual(), myHashEntries.count());
	entry = (TableEntry) (myHashEntries.fetch(offset));
	prev = null;
	while (entry != null) {
		if (entry.match(key)) {
			Heaper result;
			result = entry.value();
			if ( ! (entry.replaceValue(aHeaper))) {
				/* Replace the whole entry object if it cannot be side-effected in place. */
				TableEntry newEntry;
				newEntry = TableEntry.make(key, aHeaper);
				newEntry.setNext(entry.fetchNext());
				if (prev == null) {
					myHashEntries.store(offset, newEntry);
				}
				else {
					prev.setNext(newEntry);
				}
				entry.destroy();
			}
			return result;
		}
		prev = entry;
		entry = entry.fetchNext();
	}
	entry = TableEntry.make(key, aHeaper);
	entry.setNext(((TableEntry) (myHashEntries.fetch(offset))));
	myHashEntries.store(offset, entry);
	myTally = myTally + 1;
	return null;
/*
udanax-top.st:48573:ActualHashTable methodsFor: 'accessing'!
{Heaper | NULL} at: key {Position} store: aHeaper {Heaper} 
	| offset {Int32} entry {TableEntry | NULL} prev {TableEntry | NULL} |
	aHeaper == NULL ifTrue: [Heaper BLAST: #NullInsertion].
	self aboutToWrite.
	self checkSize.
	offset _ key hashForEqual \\ myHashEntries count.
	entry _ (myHashEntries fetch: offset) cast: TableEntry.
	prev _ NULL.
	[entry ~~ NULL] whileTrue:
		[(entry match: key) ifTrue:
			[| result {Heaper} |
			result _ entry value.
			(entry replaceValue: aHeaper) ifFalse:
				["Replace the whole entry object if it cannot be side-effected in place."
				| newEntry {TableEntry} |
				newEntry _ TableEntry make: key with: aHeaper.
				newEntry setNext: entry fetchNext.
				prev == NULL
					ifTrue: [myHashEntries at: offset store: newEntry]
					ifFalse: [prev setNext: newEntry].
				entry destroy.].
			^result].
		prev _ entry.
		entry _ entry fetchNext].
	entry _ TableEntry make: key with: aHeaper.
	entry setNext: ((myHashEntries fetch: offset) cast: TableEntry).
	myHashEntries at: offset store: entry.
	myTally _ myTally + 1.
	^NULL!
*/
}
public Heaper intStore(int key, Heaper aHeaper) {
	int offset;
	TableEntry entry;
	TableEntry prev;
	if (aHeaper == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);
	}
	aboutToWrite();
	checkSize();
	offset = AboraSupport.modulo((IntegerPos.integerHash(key)), myHashEntries.count());
	entry = (TableEntry) (myHashEntries.fetch(offset));
	prev = null;
	while (entry != null) {
		if (entry.matchInt(key)) {
			Heaper result;
			result = entry.value();
			if ( ! (entry.replaceValue(aHeaper))) {
				/* Replace the whole entry object if it cannot be side-effected in place. */
				TableEntry newEntry;
				newEntry = TableEntry.makeIntegerVar(key, aHeaper);
				newEntry.setNext(entry.fetchNext());
				if (prev == null) {
					myHashEntries.store(offset, newEntry);
				}
				else {
					prev.setNext(newEntry);
				}
				entry.destroy();
			}
			return result;
		}
		prev = entry;
		entry = entry.fetchNext();
	}
	entry = TableEntry.makeIntegerVar(key, aHeaper);
	entry.setNext(((TableEntry) (myHashEntries.fetch(offset))));
	myHashEntries.store(offset, entry);
	myTally = myTally + 1;
	return null;
/*
udanax-top.st:48603:ActualHashTable methodsFor: 'accessing'!
{Heaper | NULL} atInt: key {IntegerVar} store: aHeaper {Heaper} 
	| offset {Int32} entry {TableEntry | NULL} prev {TableEntry | NULL} |
	aHeaper == NULL ifTrue: [Heaper BLAST: #NullInsertion].
	self aboutToWrite.
	self checkSize.
	offset _ (IntegerPos integerHash: key) \\ myHashEntries count.
	entry _ (myHashEntries fetch: offset) cast: TableEntry.
	prev _ NULL.
	[entry ~~ NULL] whileTrue:
		[(entry matchInt: key) ifTrue:
			[| result {Heaper} |
			result _ entry value.
			(entry replaceValue: aHeaper) ifFalse:
				["Replace the whole entry object if it cannot be side-effected in place."
				| newEntry {TableEntry} |
				newEntry _ TableEntry make.IntegerVar: key with: aHeaper.
				newEntry setNext: entry fetchNext.
				prev == NULL
					ifTrue: [myHashEntries at: offset store: newEntry]
					ifFalse: [prev setNext: newEntry].
				entry destroy.].
			^result].
		prev _ entry.
		entry _ entry fetchNext].
	entry _ TableEntry make.IntegerVar: key with: aHeaper.
	entry setNext: ((myHashEntries fetch: offset) cast: TableEntry).
	myHashEntries at: offset store: entry.
	myTally _ myTally + 1.
	^NULL!
*/
}
public CoordinateSpace coordinateSpace() {
	return myCoordinateSpace;
/*
udanax-top.st:48633:ActualHashTable methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^ myCoordinateSpace!
*/
}
public int count() {
	return myTally;
/*
udanax-top.st:48637:ActualHashTable methodsFor: 'accessing'!
{IntegerVar} count
	^ Integer IntegerVar: myTally!
*/
}
public XnRegion domain() {
	TableStepper keys;
	keys = stepper();
	if (coordinateSpace() == IntegerSpace.make()) {
		IntegerRegion result;
		result = IntegerRegion.make();
		while (keys.hasValue()) {
			result = (IntegerRegion) (result.withInt(keys.index()));
			/* This is stupid, I should not need a cast here */
			keys.step();
		}
		keys.destroy();
		return result;
	}
	else {
		XnRegion result1;
		result1 = coordinateSpace().emptyRegion();
		while (keys.hasValue()) {
			result1 = result1.with(keys.position());
			keys.step();
		}
		keys.destroy();
		return result1;
	}
/*
udanax-top.st:48641:ActualHashTable methodsFor: 'accessing'!
{XnRegion} domain
	|keys {TableStepper} |
	keys _ self stepper.
	(self coordinateSpace == IntegerSpace make)
		ifTrue: [|result {IntegerRegion} |
			result _ IntegerRegion make.
			[keys hasValue] whileTrue:
				[result _ (result withInt: keys index) cast: IntegerRegion. "This is stupid, I should not need a cast here"
				keys step].
			keys destroy.
			^result]
		ifFalse: [|result {XnRegion} |
			result _ self coordinateSpace emptyRegion.
			[keys hasValue] whileTrue:
				[result _ result with: keys position.
				keys step].
			keys destroy.
			^ result].!
*/
}
public Heaper fetch(Position key) {
	int offset;
	TableEntry entry;
	offset = AboraSupport.modulo(key.hashForEqual(), myHashEntries.count());
	entry = (TableEntry) (myHashEntries.fetch(offset));
	while (entry != null) {
		if (entry.match(key)) {
			return entry.value();
		}
		entry = entry.fetchNext();
	}
	return null;
/*
udanax-top.st:48660:ActualHashTable methodsFor: 'accessing'!
{Heaper | NULL} fetch: key {Position} 
	| offset {Int32} entry {TableEntry wimpy} |
	offset _ key hashForEqual \\ myHashEntries count.
	entry _ (myHashEntries fetch: offset) cast: TableEntry.
	[entry ~~ NULL] whileTrue:
		[(entry match: key) ifTrue: [^entry value].
		entry _ entry fetchNext].
	^NULL!
*/
}
public Heaper intFetch(int key) {
	int offset;
	TableEntry entry;
	offset = AboraSupport.modulo((IntegerPos.integerHash(key)), myHashEntries.count());
	entry = (TableEntry) (myHashEntries.fetch(offset));
	while (entry != null) {
		if (entry.matchInt(key)) {
			return entry.value();
		}
		entry = entry.fetchNext();
	}
	return null;
/*
udanax-top.st:48670:ActualHashTable methodsFor: 'accessing'!
{Heaper | NULL} intFetch: key {IntegerVar} 
	| offset {Int32} entry {TableEntry wimpy} |
	offset _ (IntegerPos integerHash: key) \\ myHashEntries count.
	entry _ (myHashEntries fetch: offset) cast: TableEntry.
	[entry ~~ NULL] whileTrue:
		[(entry matchInt: key) ifTrue: [^entry value].
		entry _ entry fetchNext].
	^NULL!
*/
}
public ScruTable subTable(XnRegion region) {
	HashTable newTable;
	TableStepper elements;
	newTable = HashTable.makeCoordinateSpace(myCoordinateSpace, region.count());
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
udanax-top.st:48680:ActualHashTable methodsFor: 'accessing'!
{ScruTable} subTable: region {XnRegion}
	| newTable {HashTable} elements {TableStepper} |
	newTable _ HashTable make.CoordinateSpace: myCoordinateSpace with: region count.
	elements _ self stepper.
	elements forEach:
		[:elemValue {Heaper} | (region hasMember: elements position) ifTrue:
			[newTable at: elements position store: elemValue]].
	^newTable!
*/
}
public boolean wipe(Position aKey) {
	int offset;
	TableEntry prev;
	TableEntry entry;
	offset = AboraSupport.modulo(aKey.hashForEqual(), myHashEntries.count());
	entry = (TableEntry) (myHashEntries.fetch(offset));
	prev = null;
	while (entry != null) {
		if (entry.match(aKey)) {
			aboutToWrite();
			if (prev == null) {
				myHashEntries.store(offset, entry.fetchNext());
			}
			else {
				prev.setNext(entry.fetchNext());
			}
			entry.destroy();
			myTally = myTally - 1;
			return true;
		}
		prev = entry;
		entry = entry.fetchNext();
	}
	return false;
/*
udanax-top.st:48690:ActualHashTable methodsFor: 'accessing'!
{BooleanVar} wipe: aKey {Position} 
	| offset {UInt32} prev {TableEntry wimpy | NULL} entry {TableEntry wimpy | NULL} |
	offset _ aKey hashForEqual \\ myHashEntries count.
	entry _ (myHashEntries fetch: offset) cast: TableEntry.
	prev _ NULL.
	[entry ~~ NULL] whileTrue:
		[(entry match: aKey) ifTrue: 
			[self aboutToWrite.
			prev == NULL
				ifTrue: [myHashEntries at: offset store: entry fetchNext]
				ifFalse: [prev setNext: entry fetchNext].
			entry destroy.
			myTally _ myTally - 1.
			^true].
		prev _ entry.
		entry _ entry fetchNext].
	^false!
*/
}
public int fastHash() {
	int result;
	TableEntry entry;
	result = getCategory().hashForEqual() + myTally;
	for (int i = 0; i < myHashEntries.count(); i ++ ) {
		entry = (TableEntry) (myHashEntries.fetch(i));
		while (entry != null) {
			result = result + entry.hashForEqual();
			entry = entry.fetchNext();
		}
	}
	return result;
/*
udanax-top.st:48710:ActualHashTable methodsFor: 'testing'!
{UInt32} fastHash
	| result {UInt32 register} entry {TableEntry wimpy} |
	result _ self getCategory hashForEqual + myTally.
	Int32Zero almostTo: myHashEntries count do: 
		[:i {Int32} | 
		entry _ (myHashEntries fetch: i) cast: TableEntry.
		[entry ~~ NULL]
			whileTrue: 
				[result _ result + entry hashForEqual.
				entry _ entry fetchNext]].
	^result!
*/
}
public boolean includesKey(Position aKey) {
	return (fetch(aKey)) != null;
/*
udanax-top.st:48722:ActualHashTable methodsFor: 'testing'!
{BooleanVar} includesKey: aKey {Position}
	^ (self fetch: aKey) ~~ NULL!
*/
}
public boolean isEmpty() {
	return myTally == 0;
/*
udanax-top.st:48725:ActualHashTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	^ myTally == Int32Zero!
*/
}
public ScruTable copy() {
	return new ActualHashTable(myHashEntries, count(), coordinateSpace());
/*
udanax-top.st:48730:ActualHashTable methodsFor: 'creation'!
{ScruTable} copy 
	^ActualHashTable create: myHashEntries with: self count DOTasLong with: self coordinateSpace!
*/
}
public ScruTable emptySize(int size) {
	return ActualHashTable.make(myCoordinateSpace, size);
/*
udanax-top.st:48733:ActualHashTable methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar}
	^ActualHashTable make: myCoordinateSpace with: size!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	printOnWithSimpleSyntax(oo, "[", ", ", "]");
/*
udanax-top.st:48738:ActualHashTable methodsFor: 'printing'!
{void} printOn: oo {ostream reference} 
	oo << self getCategory name.
	self printOnWithSimpleSyntax: oo with: '[' with: ', ' with: ']'!
*/
}
public XnRegion runAt(Position index) {
	if (includesKey(index)) {
		return index.asRegion();
	}
	else {
		return myCoordinateSpace.emptyRegion();
	}
/*
udanax-top.st:48744:ActualHashTable methodsFor: 'runLength'!
{XnRegion} runAt: index {Position}
	
	(self includesKey: index)
		ifTrue: [^ index asRegion]
		ifFalse: [^ myCoordinateSpace emptyRegion]!
*/
}
/**
 * ignore order spec for now
 */
public TableStepper stepper(OrderSpec order) {
	if (order == null) {
		return TableEntry.bucketStepper(myHashEntries);
	}
	else {
		throw new UnimplementedException();
	}
/*
udanax-top.st:48752:ActualHashTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL}
	"ignore order spec for now"
	order == NULL 
		ifTrue: [^ TableEntry bucketStepper: myHashEntries]
		ifFalse: [self unimplemented].
	^NULL "fodder"!
*/
}
public Heaper theOne() {
	TableEntry entry;
	if (myTally != 1) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_ONE_ELEMENT);
	}
	for (int index = 0; index < myHashEntries.count(); index ++ ) {
		if ((entry = (TableEntry) (myHashEntries.fetch(index))) != null) {
			return entry.value();
		}
	}
	return null;
/*
udanax-top.st:48759:ActualHashTable methodsFor: 'enumerating'!
{Heaper} theOne
	| entry {TableEntry wimpy} |
	myTally ~~ 1 ifTrue: [Heaper BLAST: #NotOneElement].
	Int32Zero almostTo: myHashEntries count do: [:index {Int32} |
		(entry _ (myHashEntries fetch: index) cast: TableEntry) ~~ NULL ifTrue: [^entry value]].
	^ NULL. "Keep the compiler quiet"!
*/
}
/**
 * This currently doesn't take advantage of the optimizations in TableEntries.  It should.
 */
public void receiveHashTable(Rcvr rcvr) {
	myHashEntries = (SharedPtrArray) SharedPtrArray.make(myTally / 2 + 1);
	myHashEntries.shareMore();
	if (myCoordinateSpace.isEqual(IntegerSpace.make())) {
		for (int i = 0; i < myTally; i ++ ) {
			int index;
			index = rcvr.receiveIntegerVar();
			storeEntry((TableEntry.makeIntegerVar(index, rcvr.receiveHeaper())));
		}
	}
	else {
		for (int i1 = 0; i1 < myTally; i1 ++ ) {
			Position key;
			key = (Position) rcvr.receiveHeaper();
			storeEntry((TableEntry.make(key, rcvr.receiveHeaper())));
		}
	}
/*
udanax-top.st:48768:ActualHashTable methodsFor: 'hooks:'!
{void RECEIVE.HOOK} receiveHashTable: rcvr {Rcvr} 
	"This currently doesn't take advantage of the optimizations in TableEntries.  It should."
	myHashEntries _ SharedPtrArray make: myTally // 2 + 1.
	myHashEntries shareMore.
	(myCoordinateSpace isEqual: IntegerSpace make)
		ifTrue: [myTally timesRepeat: 
				[| index {IntegerVar} |
				index _ rcvr receiveIntegerVar.
				self storeEntry: (TableEntry make.IntegerVar: index with: rcvr receiveHeaper)]]
		ifFalse: [myTally timesRepeat: 
				[| key {Position} |
				key _ rcvr receiveHeaper cast: Position.
				self storeEntry: (TableEntry make: key with: rcvr receiveHeaper)]]!
*/
}
/**
 * This currently doesn't take advantage of the optimizations in TableEntries.  It should.
 */
public void sendHashTable(Xmtr xmtr) {
	if (myCoordinateSpace.isEqual(IntegerSpace.make())) {
		TableStepper stomper = stepper();
		for (; stomper.hasValue(); stomper.step()) {
			int index = (int) stomper.index();
			Heaper value = (Heaper) stomper.fetch();
			if (value == null) {
				continue ;
			}
			xmtr.sendIntegerVar(index);
			xmtr.sendHeaper(value);
		}
		stomper.destroy();
	}
	else {
		TableStepper stomper2 = stepper();
		for (; stomper2.hasValue(); stomper2.step()) {
			Position p = (Position) stomper2.position();
			Heaper v = (Heaper) stomper2.fetch();
			if (v == null) {
				continue ;
			}
			xmtr.sendHeaper(p);
			xmtr.sendHeaper(v);
		}
		stomper2.destroy();
	}
/*
udanax-top.st:48783:ActualHashTable methodsFor: 'hooks:'!
{void SEND.HOOK} sendHashTable: xmtr {Xmtr}
	"This currently doesn't take advantage of the optimizations in TableEntries.  It should."
	
	(myCoordinateSpace isEqual: IntegerSpace make)
		ifTrue:
			[self stepper forIndices: [:index {IntegerVar} :value {Heaper} |
				xmtr sendIntegerVar: index.
				xmtr sendHeaper: value]]
		ifFalse:
			[self stepper forPositions: [:p {Position} :v {Heaper} |
				xmtr sendHeaper: p.
				xmtr sendHeaper: v]]!
*/
}
public ActualHashTable(SharedPtrArray entries, int tally, CoordinateSpace cs) {
	super();
	myHashEntries = entries;
	myTally = tally;
	myCoordinateSpace = cs;
	myHashEntries.shareMore();
/*
udanax-top.st:48798:ActualHashTable methodsFor: 'protected:'!
create: entries {SharedPtrArray of: TableEntry} with: tally {Int32} with: cs {CoordinateSpace} 
	super create.
	myHashEntries _ entries.
	myTally _ tally.
	myCoordinateSpace _ cs.
	myHashEntries shareMore!
*/
}
public void destruct() {
	myHashEntries.shareLess();
	super.destruct();
/*
udanax-top.st:48805:ActualHashTable methodsFor: 'protected:'!
{void} destruct
	myHashEntries shareLess.
	super destruct!
*/
}
/**
 * If my contents are shared, and I'm about to change them, make a copy of them.
 */
public void aboutToWrite() {
	if (myHashEntries.shareCount() > 1) {
		SharedPtrArray newEntries;
		int entryCount;
		entryCount = myHashEntries.count();
		newEntries = (SharedPtrArray) SharedPtrArray.make(entryCount);
		for (int index = 0; index < entryCount; index ++ ) {
			TableEntry entry;
			if ((entry = (TableEntry) (myHashEntries.fetch(index))) != null) {
				TableEntry newEntry;
				newEntry = entry.copy();
				newEntries.store(index, newEntry);
				entry = entry.fetchNext();
				while (entry != null) {
					newEntry.setNext(entry.copy());
					newEntry = newEntry.fetchNext();
					entry = entry.fetchNext();
				}
			}
		}
		myHashEntries.shareLess();
		myHashEntries = newEntries;
		myHashEntries.shareMore();
	}
/*
udanax-top.st:48811:ActualHashTable methodsFor: 'private:'!
{void} aboutToWrite
	"If my contents are shared, and I'm about to change them, make a copy of them."
	myHashEntries shareCount > 1 ifTrue:
		[| newEntries {SharedPtrArray of: TableEntry} entryCount {Int32} |
		entryCount _ myHashEntries count.
		newEntries _ SharedPtrArray make: entryCount.
		Int32Zero almostTo: entryCount do: [:index {Int32} | 
			| entry {TableEntry wimpy} |
			(entry _ (myHashEntries fetch: index) cast: TableEntry) ~~ NULL ifTrue: 
				[| newEntry {TableEntry} |
				newEntry _ entry copy.
				newEntries at: index store: newEntry.
				entry _ entry fetchNext.
				[entry ~~ NULL] whileTrue: 
					[newEntry setNext: entry copy.
					newEntry _ newEntry fetchNext.
					entry _ entry fetchNext]]].
		myHashEntries shareLess.
		myHashEntries _ newEntries.
		myHashEntries shareMore]!
*/
}
public void checkSize() {
	SharedPtrArray oldEntries;
	int oldSize;
	int newSize;
	if (myTally > (myHashEntries.count() * 2)) {
		oldSize = myHashEntries.count();
		newSize = LPPrimeSizeProvider.make().uInt32PrimeAfter((oldSize * 4));
		myHashEntries.shareLess();
		oldEntries = myHashEntries;
		myHashEntries = (SharedPtrArray) SharedPtrArray.make(newSize);
		myHashEntries.shareMore();
		for (int j = 0; j < oldSize; j ++ ) {
			TableEntry cur;
			TableEntry next;
			cur = (TableEntry) (oldEntries.fetch(j));
			while (cur != null) {
				next = cur.fetchNext();
				storeEntry(cur);
				cur = next;
			}
		}
		oldEntries.destroy();
	}
/*
udanax-top.st:48832:ActualHashTable methodsFor: 'private:'!
{void} checkSize
	| oldEntries {SharedPtrArray} oldSize {Int32} newSize {Int32} |
	myTally > (myHashEntries count * 2)
		ifTrue: 
			[oldSize _ myHashEntries count.
			newSize _ LPPrimeSizeProvider make uInt32PrimeAfter: (oldSize * 4).
			myHashEntries shareLess.
			oldEntries _ myHashEntries.
			myHashEntries _ SharedPtrArray make: newSize.
			myHashEntries shareMore.
			Int32Zero almostTo: oldSize do: [:j {Int32} |
				| cur {TableEntry} next {TableEntry} |
				cur _ (oldEntries fetch: j) cast: TableEntry.
				[cur ~~ NULL] whileTrue: 
					[next _ cur fetchNext.
					self storeEntry: cur.
					cur _ next]].
			oldEntries destroy]!
*/
}
/**
 * Store the tableentry into the entry table
 */
public void storeEntry(TableEntry anEntry) {
	int index;
	index = AboraSupport.modulo(anEntry.position().hashForEqual(), myHashEntries.count());
	anEntry.setNext(((TableEntry) (myHashEntries.fetch(index))));
	myHashEntries.store(index, anEntry);
/*
udanax-top.st:48851:ActualHashTable methodsFor: 'private:'!
{void} storeEntry: anEntry {TableEntry}
	"Store the tableentry into the entry table"
	| index {UInt32} |
	index _ anEntry position hashForEqual \\ myHashEntries count.
	anEntry setNext: ((myHashEntries fetch: index) cast: TableEntry).
	myHashEntries at: index store: anEntry!
*/
}
/*
udanax-top.st:48860:ActualHashTable methodsFor: 'smalltalk: private:'!
{void} inspect
	^InspectorView open: (HashTableInspector inspect: self)!
*/
public ActualHashTable(Rcvr receiver) {
	super(receiver);
	myTally = receiver.receiveInt32();
	myCoordinateSpace = (CoordinateSpace) receiver.receiveHeaper();
	receiveHashTable(receiver);
/*
udanax-top.st:48865:ActualHashTable methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myTally _ receiver receiveInt32.
	myCoordinateSpace _ receiver receiveHeaper.
	self receiveHashTable: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendInt32(myTally);
	xmtr.sendHeaper(myCoordinateSpace);
	sendHashTable(xmtr);
/*
udanax-top.st:48871:ActualHashTable methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendInt32: myTally.
	xmtr sendHeaper: myCoordinateSpace.
	self sendHashTable: xmtr.!
*/
}
public static MuTable make(CoordinateSpace cs) {
	return new ActualHashTable(((SharedPtrArray) SharedPtrArray.make(7)), 0, cs);
/*
udanax-top.st:48891:ActualHashTable class methodsFor: 'creation'!
{HashTable} make: cs {CoordinateSpace}
	^self create: (SharedPtrArray make: 7) with: Int32Zero with: cs!
*/
}
public static HashTable make(CoordinateSpace cs, int size) {
	return new ActualHashTable(((SharedPtrArray) SharedPtrArray.make((LPPrimeSizeProvider.make().uInt32PrimeAfter((size))))), 0, cs);
/*
udanax-top.st:48894:ActualHashTable class methodsFor: 'creation'!
{HashTable} make: cs {CoordinateSpace} with: size {IntegerVar}
	^self create: (SharedPtrArray make: (LPPrimeSizeProvider make uInt32PrimeAfter: (size DOTasLong))) with: Int32Zero with: cs!
*/
}
public ActualHashTable() {
/*

Generated during transformation
*/
}
}
