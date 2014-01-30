/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.settable;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.SharedPtrArray;
import info.dgjones.abora.gold.collection.settable.SetTable;
import info.dgjones.abora.gold.collection.settable.TableEntry;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimSet;
import info.dgjones.abora.gold.settab.SetTableStepper;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.tabtool.PrimeSizeProvider;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * SetTable is a table-like object (NOT at true table) that can store multiple values at a
 * single position.  See MuTable for comments on the protocol.
 * The reason that this is not a table subclass is because of several ambiguities in the
 * contract.  For example, replace for a table implies that the position must be previously
 * occupied, but in a settable the position is occupied only if the exact association
 * (key->value) is present.
 */
public class SetTable extends Heaper {

	protected SharedPtrArray myHashEntries;
	protected int myTally;
	protected CoordinateSpace myCoordinateSpace;
/*
udanax-top.st:51147:
Heaper subclass: #SetTable
	instanceVariableNames: '
		myHashEntries {SharedPtrArray}
		myTally {Int32}
		myCoordinateSpace {CoordinateSpace}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-SetTable'!
*/
/*
udanax-top.st:51154:
SetTable comment:
'SetTable is a table-like object (NOT at true table) that can store multiple values at a single position.  See MuTable for comments on the protocol.
  The reason that this is not a table subclass is because of several ambiguities in the contract.  For example, replace for a table implies that the position must be previously occupied, but in a settable the position is occupied only if the exact association (key->value) is present.'!
*/
/*
udanax-top.st:51157:
(SetTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #COPY; yourself)!
*/
/*
udanax-top.st:51446:
SetTable class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:51449:
(SetTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SetTable.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Store anObject at position aKey; BLAST if position is already occupied
 * (for SetTable, there must be an object that isEqual to anObject at aKey
 * for the position to be considered occupied)
 */
public void introduce(Position aKey, Heaper anObject) {
	if ( ! (store(aKey, anObject))) {
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
	}
/*
udanax-top.st:51162:SetTable methodsFor: 'accessing'!
{void} at: aKey {Position} introduce: anObject {Heaper}
	"Store anObject at position aKey; BLAST if position is already occupied
	 (for SetTable, there must be an object that isEqual to anObject at aKey 
	 for the position to be considered occupied)"
	(self at: aKey store: anObject) ifFalse: [Heaper BLAST: #AlreadyInTable]!
*/
}
/**
 * Store anObject at position aKey; return TRUE if store accomplished, FALSE otherwise
 */
public boolean store(Position aKey, Heaper anObject) {
	int index;
	TableEntry entry;
	if (anObject == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);
	}
	aboutToWrite();
	checkSize();
	index = AboraSupport.modulo(aKey.hashForEqual(), myHashEntries.count());
	entry = (TableEntry) (myHashEntries.fetch(index));
	while (entry != null) {
		if ((entry.match(aKey)) && (entry.matchValue(anObject))) {
			return false;
		}
		entry = entry.fetchNext();
	}
	entry = TableEntry.make(aKey, anObject);
	entry.setNext(((TableEntry) (myHashEntries.fetch(index))));
	myHashEntries.store(index, entry);
	myTally = myTally + 1;
	return true;
/*
udanax-top.st:51168:SetTable methodsFor: 'accessing'!
{BooleanVar} at: aKey {Position} store: anObject {Heaper}
	"Store anObject at position aKey; return TRUE if store accomplished, FALSE otherwise"
	| index {Int32 register} entry {TableEntry} |
	anObject == NULL ifTrue: [Heaper BLAST: #NullInsertion].
	self aboutToWrite.
	self checkSize.
	index _ aKey hashForEqual \\ myHashEntries count.
	entry _ (myHashEntries fetch: index) cast: TableEntry.
	[entry ~~ NULL] whileTrue:
		[((entry match: aKey) and: [entry matchValue: anObject]) ifTrue: [^false].
		entry _ entry fetchNext].
	entry _ TableEntry make: aKey with: anObject.
	entry setNext: ((myHashEntries fetch: index) cast: TableEntry).
	myHashEntries at: index store: entry.
	myTally _ myTally + 1.
	^true!
*/
}
public void intIntroduce(int index, Heaper anObject) {
	if ( ! (intStore(index, anObject))) {
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
	}
/*
udanax-top.st:51185:SetTable methodsFor: 'accessing'!
{void} atInt: index {IntegerVar} introduce: anObject {Heaper}
	(self atInt: index store: anObject) ifFalse: [Heaper BLAST: #AlreadyInTable]!
*/
}
public boolean intStore(int index, Heaper anObject) {
	int offset;
	TableEntry entry;
	if (anObject == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);
	}
	aboutToWrite();
	checkSize();
	offset = AboraSupport.modulo((IntegerPos.integerHash(index)), myHashEntries.count());
	entry = (TableEntry) (myHashEntries.fetch(offset));
	while (entry != null) {
		if ((entry.matchInt(index)) && (entry.matchValue(anObject))) {
			return false;
		}
		entry = entry.fetchNext();
	}
	entry = TableEntry.makeIntegerVar(index, anObject);
	entry.setNext(((TableEntry) (myHashEntries.fetch(offset))));
	myHashEntries.store(offset, entry);
	myTally = myTally + 1;
	return true;
/*
udanax-top.st:51188:SetTable methodsFor: 'accessing'!
{BooleanVar} atInt: index {IntegerVar} store: anObject {Heaper} 
	| offset {Int32 register} entry {TableEntry} |
	anObject == NULL ifTrue: [Heaper BLAST: #NullInsertion].
	self aboutToWrite.
	self checkSize.
	offset _ (IntegerPos integerHash: index) \\ myHashEntries count.
	entry _ (myHashEntries fetch: offset) cast: TableEntry.
	[entry ~~ NULL] whileTrue:
		[((entry matchInt: index) and: [entry matchValue: anObject]) ifTrue: [^false].
		entry _ entry fetchNext].
	entry _ TableEntry make.IntegerVar: index with: anObject.
	entry setNext: ((myHashEntries fetch: offset) cast: TableEntry).
	myHashEntries at: offset store: entry.
	myTally _ myTally + 1.
	^true!
*/
}
public CoordinateSpace coordinateSpace() {
	return myCoordinateSpace;
/*
udanax-top.st:51204:SetTable methodsFor: 'accessing'!
{CoordinateSpace INLINE} coordinateSpace
	^ myCoordinateSpace!
*/
}
public int count() {
	return myTally;
/*
udanax-top.st:51208:SetTable methodsFor: 'accessing'!
{IntegerVar INLINE} count
	^ Integer IntegerVar: myTally!
*/
}
public XnRegion domain() {
	XnRegion result;
	TableStepper keys;
	result = coordinateSpace().emptyRegion();
	Stepper stomper = (keys = stepper());
	for (; stomper.hasValue(); stomper.step()) {
		Heaper element = (Heaper) stomper.fetch();
		if (element == null) {
			continue ;
		}
		result = result.with(keys.position());
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:51212:SetTable methodsFor: 'accessing'!
{XnRegion} domain
	| result {XnRegion} keys {TableStepper} |
	result _ self coordinateSpace emptyRegion.
	(keys _ self stepper) forEach: [ :element {Heaper} |
		result _ result with: keys position].
	^result!
*/
}
public void intRemove(int index, Heaper value) {
	if ( ! (wipeIntegerVar(index, value))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
/*
udanax-top.st:51219:SetTable methodsFor: 'accessing'!
{void} intRemove: index {IntegerVar} with: value {Heaper}
	(self wipe.IntegerVar: index with: value) ifFalse: [Heaper BLAST: #NotInTable]!
*/
}
public void remove(Position key, Heaper value) {
	if ( ! (wipeAssociation(key, value))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
/*
udanax-top.st:51222:SetTable methodsFor: 'accessing'!
{void} remove: key {Position} with: value {Heaper}
	(self wipeAssociation: key with: value) ifFalse: [Heaper BLAST: #NotInTable]!
*/
}
public boolean wipeIntegerVar(int index, Heaper value) {
	int offset;
	TableEntry prev;
	TableEntry entry;
	offset = AboraSupport.modulo((IntegerPos.integerHash(index)), myHashEntries.count());
	entry = (TableEntry) (myHashEntries.fetch(offset));
	prev = entry;
	while (entry != null) {
		if ((entry.matchInt(index)) && (entry.matchValue(value))) {
			aboutToWrite();
			if (entry.isEqual(prev)) {
				myHashEntries.store(offset, entry.fetchNext());
			}
			else {
				prev.setNext(entry.fetchNext());
			}
			entry.destroy();
			entry = null;
			prev = null;
			myTally = myTally - 1;
			return true;
		}
		prev = entry;
		entry = entry.fetchNext();
	}
	return false;
/*
udanax-top.st:51225:SetTable methodsFor: 'accessing'!
{BooleanVar} wipe.IntegerVar: index {IntegerVar} with: value {Heaper}
	| offset {Int32 register} prev {TableEntry} entry {TableEntry} |
	offset _ (IntegerPos integerHash: index) \\ myHashEntries count.
	entry _ (myHashEntries fetch: offset) cast: TableEntry.
	prev _ entry.
	[entry ~~ NULL] whileTrue:
		[((entry matchInt: index) and: [entry matchValue: value]) ifTrue:
			[self aboutToWrite.
			(entry isEqual: prev)
				ifTrue: [myHashEntries at: offset store: entry fetchNext]
				ifFalse: [prev setNext: entry fetchNext].
			entry destroy.
			entry _ NULL.
			prev _ NULL.
			myTally _ myTally - 1.
			^true].
		prev _ entry.
		entry _ entry fetchNext].
	^false!
*/
}
public boolean wipeAssociation(Position key, Heaper value) {
	int offset;
	TableEntry prev;
	TableEntry entry;
	offset = AboraSupport.modulo(key.hashForEqual(), myHashEntries.count());
	entry = (TableEntry) (myHashEntries.fetch(offset));
	prev = null;
	while (entry != null) {
		if ((entry.match(key)) && (entry.matchValue(value))) {
			aboutToWrite();
			if (prev == null) {
				myHashEntries.store(offset, entry.fetchNext());
			}
			else {
				prev.setNext(entry.fetchNext());
			}
			entry.destroy();
			entry = prev = null;
			myTally = myTally - 1;
			return true;
		}
		prev = entry;
		entry = entry.fetchNext();
	}
	return false;
/*
udanax-top.st:51245:SetTable methodsFor: 'accessing'!
{BooleanVar} wipeAssociation: key {Position} with: value {Heaper}
	| offset {Int32 register} prev {TableEntry} entry {TableEntry} |
	offset _ key hashForEqual \\ myHashEntries count.
	entry _ (myHashEntries fetch: offset) cast: TableEntry.
	prev _ NULL.
	[entry ~~ NULL] whileTrue:
		[((entry match: key) and: [entry matchValue: value]) ifTrue:
			[self aboutToWrite.
			prev == NULL
				ifTrue: [myHashEntries at: offset store: entry fetchNext]
				ifFalse: [prev setNext: entry fetchNext].
			entry destroy.
			entry _ prev _ NULL.
			myTally _ myTally - 1.
			^true].
		prev _ entry.
		entry _ entry fetchNext].
	^false!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	printOnWithSimpleSyntax(oo, "[", ", ", "]");
/*
udanax-top.st:51266:SetTable methodsFor: 'printing'!
{void} printOn: oo {ostream reference} 
	oo << self getCategory name.
	self printOnWithSimpleSyntax: oo with: '[' with: ', ' with: ']'!
*/
}
public void printOnWithSimpleSyntax(PrintWriter oo, String open, String sep, String close) {
	TableStepper stomp;
	oo.print(open);
	if (isEmpty()) {
		oo.print("empty");
	}
	else {
		stomp = stepper();
		oo.print(stomp.position());
		oo.print("->");
		oo.print(stomp.fetch());
		stomp.step();
		Stepper stomper = stomp;
		for (; stomper.hasValue(); stomper.step()) {
			Heaper val = (Heaper) stomper.fetch();
			if (val == null) {
				continue ;
			}
			oo.print(sep);
			oo.print(stomp.position());
			oo.print("->");
			oo.print(val);
		}
		stomper.destroy();
	}
	oo.print(close);
/*
udanax-top.st:51270:SetTable methodsFor: 'printing'!
{void} printOnWithSimpleSyntax: oo {ostream reference} with: open {char star} with: sep {char star} with: close {char star} 
	| stomp {TableStepper} |
	oo << open.
	self isEmpty
		ifTrue: [oo << 'empty']
		ifFalse: 
			[stomp _ self stepper.
			oo << stomp position << '->' << stomp fetch.
			stomp step.
			stomp forEach: [:val {Heaper} |
				oo << sep << stomp position << '->' << val]].
	oo << close!
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
udanax-top.st:51285:SetTable methodsFor: 'runLength'!
{XnRegion} runAt: index {Position}
	
	(self includesKey: index)
		ifTrue: [^ index asRegion]
		ifFalse: [^ myCoordinateSpace emptyRegion]!
*/
}
public XnRegion runAtInt(int index) {
	return runAt(IntegerPos.make(index));
/*
udanax-top.st:51291:SetTable methodsFor: 'runLength'!
{XnRegion} runAtInt: index {IntegerVar}
	
	^ self runAt: index integer!
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
udanax-top.st:51297:SetTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL}
	"ignore order spec for now"
	order == NULL 
		ifTrue: [^TableEntry bucketStepper: myHashEntries]
		ifFalse: [self unimplemented.
			^NULL "fodder"]!
*/
}
public Stepper stepperAt(Position key) {
	int offset;
	PrimSet elements;
	TableEntry entry;
	offset = AboraSupport.modulo(key.hashForEqual(), myHashEntries.count());
	elements = PrimSet.make();
	entry = (TableEntry) (myHashEntries.fetch(offset));
	while (entry != null) {
		if (entry.match(key)) {
			elements.introduce(entry.value());
		}
		entry = entry.fetchNext();
	}
	return elements.stepper();
/*
udanax-top.st:51304:SetTable methodsFor: 'enumerating'!
{Stepper} stepperAt: key {Position}
	| offset {Int32 register} elements {PrimSet} entry {TableEntry wimpy} |
	offset _ key hashForEqual \\ myHashEntries count.
	elements _ PrimSet make.
	entry _ (myHashEntries fetch: offset) cast: TableEntry.
	[entry ~~ NULL] whileTrue:
		[(entry match: key) 
			ifTrue: [elements introduce: entry value].
		entry _ entry fetchNext].
	^elements stepper!
*/
}
public Stepper stepperAtInt(int index) {
	int offset;
	PtrArray elements;
	TableEntry entry;
	int i;
	offset = AboraSupport.modulo((IntegerPos.integerHash(index)), myHashEntries.count());
	elements = SetTableStepper.array();
	i = 0;
	entry = (TableEntry) (myHashEntries.fetch(offset));
	while (entry != null) {
		if (entry.matchInt(index)) {
			if (i >= elements.count()) {
				elements = (PtrArray) (elements.copyGrow(4));
			}
			elements.store(i, entry.value());
			i = i + 1;
		}
		entry = entry.fetchNext();
	}
	return SetTableStepper.make(elements);
/*
udanax-top.st:51316:SetTable methodsFor: 'enumerating'!
{Stepper} stepperAtInt: index {IntegerVar}
	| offset {Int32 register} elements {PtrArray} entry {TableEntry wimpy} i {Int32} |
	offset _ (IntegerPos integerHash: index) \\ myHashEntries count.
	elements _ SetTableStepper array.
	i _ Int32Zero.
	entry _ (myHashEntries fetch: offset) cast: TableEntry.
	[entry ~~ NULL] whileTrue:
		[(entry matchInt: index) 
			ifTrue: [
				i >= elements count ifTrue: [elements _ (elements copyGrow: 4) cast: PtrArray].
				elements at: i store: entry value.
				i := i + 1].
		entry _ entry fetchNext].
	^SetTableStepper make: elements.!
*/
}
public SetTable(SharedPtrArray entries, int tally, CoordinateSpace cs) {
	super();
	myHashEntries = entries;
	myTally = tally;
	myCoordinateSpace = cs;
	myHashEntries.shareMore();
/*
udanax-top.st:51334:SetTable methodsFor: 'creation'!
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
udanax-top.st:51341:SetTable methodsFor: 'creation'!
{void} destruct
	myHashEntries shareLess.
	super destruct!
*/
}
/**
 * return an empty table just like the current one
 */
public SetTable emptySize(int size) {
	return SetTable.make(myCoordinateSpace, size);
/*
udanax-top.st:51345:SetTable methodsFor: 'creation'!
{SetTable INLINE} emptySize: size {IntegerVar}
	"return an empty table just like the current one"
	^SetTable make: myCoordinateSpace with: size!
*/
}
public TableStepper stepper() {
	return stepper(null);
/*
udanax-top.st:51352:SetTable methodsFor: 'smalltalk:'!
stepper
	^self stepper: NULL!
*/
}
public boolean includes(Position key, Heaper value) {
	Stepper stomper = (stepperAt(key));
	for (; stomper.hasValue(); stomper.step()) {
		Heaper val = (Heaper) stomper.fetch();
		if (val == null) {
			continue ;
		}
		if (val.isEqual(value)) {
			return true;
		}
	}
	stomper.destroy();
	return false;
/*
udanax-top.st:51357:SetTable methodsFor: 'testing'!
{BooleanVar} at: key {Position} includes: value {Heaper}
	(self stepperAt: key) forEach: [:val {Heaper} | (val isEqual: value) ifTrue: [^true]].
	^false!
*/
}
public boolean includesKey(Position aKey) {
	Stepper stp;
	boolean result;
	stp = stepperAt(aKey);
	result = stp.hasValue();
	stp.destroy();
	return result;
/*
udanax-top.st:51361:SetTable methodsFor: 'testing'!
{BooleanVar} includesKey: aKey {Position}
	| stp {Stepper} result {BooleanVar} |
	stp _ self stepperAt: aKey.
	result _ stp hasValue.
	stp destroy.
	^result!
*/
}
public boolean isEmpty() {
	return count() == 0;
/*
udanax-top.st:51368:SetTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	^self count == IntegerVar0!
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
udanax-top.st:51373:SetTable methodsFor: 'private: resize'!
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
	if (myTally > (myHashEntries.count() * 3)) {
		oldSize = myHashEntries.count();
		newSize = PrimeSizeProvider.make().uInt32PrimeAfter((oldSize * 4));
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
udanax-top.st:51394:SetTable methodsFor: 'private: resize'!
{void} checkSize
	| oldEntries {SharedPtrArray} oldSize {Int32} newSize {Int32} |
	myTally > (myHashEntries count * 3)
		ifTrue:
			[oldSize _ myHashEntries count.
			newSize _ PrimeSizeProvider make uInt32PrimeAfter: (oldSize * 4).
			myHashEntries shareLess.
			oldEntries _ myHashEntries.
			myHashEntries _ SharedPtrArray make: newSize.
			myHashEntries shareMore.
			Int32Zero almostTo: oldSize do: [:j {Int32 register} |
				| cur {TableEntry} next {TableEntry} |
				cur _ (oldEntries fetch: j) cast: TableEntry.
				[cur ~~ NULL] whileTrue: 
					[next _ cur fetchNext.
					self storeEntry: cur.
					cur _ next]].
			oldEntries destroy]!
*/
}
public void storeEntry(TableEntry entry) {
	int idx;
	if (myCoordinateSpace.isEqual(IntegerSpace.make())) {
		idx = IntegerPos.integerHash(entry.index());
	}
	else {
		idx = entry.position().hashForEqual();
	}
	idx = AboraSupport.modulo(idx, myHashEntries.count());
	entry.setNext(((TableEntry) (myHashEntries.fetch(idx))));
	myHashEntries.store(idx, entry);
/*
udanax-top.st:51413:SetTable methodsFor: 'private: resize'!
{void} storeEntry: entry {TableEntry}
	| idx {UInt32} |
	(myCoordinateSpace isEqual: IntegerSpace make)
		ifTrue: [idx _ IntegerPos integerHash: entry index ]
		ifFalse: [idx _ entry position hashForEqual].
	idx _ idx \\ myHashEntries count.
	entry setNext: ((myHashEntries fetch: idx) cast: TableEntry).
	myHashEntries at: idx store: entry!
*/
}
/*
udanax-top.st:51424:SetTable methodsFor: 'smalltalk: private: smalltalk private'!
{void} inspect
	^InspectorView open: (SetTableInspector inspect: self)!
*/
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:51429:SetTable methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public SetTable(Rcvr receiver) {
	super(receiver);
	myHashEntries = (SharedPtrArray) receiver.receiveHeaper();
	myTally = receiver.receiveInt32();
	myCoordinateSpace = (CoordinateSpace) receiver.receiveHeaper();
/*
udanax-top.st:51431:SetTable methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myHashEntries _ receiver receiveHeaper.
	myTally _ receiver receiveInt32.
	myCoordinateSpace _ receiver receiveHeaper.!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:51437:SetTable methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myHashEntries);
	xmtr.sendInt32(myTally);
	xmtr.sendHeaper(myCoordinateSpace);
/*
udanax-top.st:51439:SetTable methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myHashEntries.
	xmtr sendInt32: myTally.
	xmtr sendHeaper: myCoordinateSpace.!
*/
}
public static SetTable make(CoordinateSpace cs) {
	return make(cs, 7);
/*
udanax-top.st:51454:SetTable class methodsFor: 'creation'!
{SetTable INLINE} make: cs {CoordinateSpace}
	^self make: cs with: 7!
*/
}
public static SetTable make(CoordinateSpace cs, int size) {
	return new SetTable(((SharedPtrArray) SharedPtrArray.make((size | 1))), 0, cs);
/*
udanax-top.st:51457:SetTable class methodsFor: 'creation'!
make: cs {CoordinateSpace} with: size {IntegerVar}
	^self create: (SharedPtrArray make: (size DOTasLong bitOr: 1)) with: Int32Zero with: cs!
*/
}
public SetTable() {
/*

Generated during transformation
*/
}
}
