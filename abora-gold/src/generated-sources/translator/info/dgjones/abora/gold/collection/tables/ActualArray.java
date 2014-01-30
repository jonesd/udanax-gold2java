/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.tables;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.AscendingArrayStepper;
import info.dgjones.abora.gold.collection.steppers.IntegerTableStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ActualArray;
import info.dgjones.abora.gold.collection.tables.MuArray;
import info.dgjones.abora.gold.collection.tables.OffsetScruArray;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerMapping;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class ActualArray extends MuArray {

	protected PtrArray elements;
	protected int tally;
/*
udanax-top.st:49324:
MuArray subclass: #ActualArray
	instanceVariableNames: '
		elements {PtrArray}
		tally {UInt32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:49330:
(ActualArray getOrMakeCxxClassDescription)
	friends:
'/- friends for class ActualArray -/
friend class AscendingArrayStepper;
friend SPTR(MuArray) MuArray::make(IntegerVar);';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ActualArray.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public int fastHash() {
	return tally + HashHelper.hashForEqual(this.getClass());
/*
udanax-top.st:49339:ActualArray methodsFor: 'testing'!
{UInt32} fastHash
	^tally + #cat.U.ActualArray hashForEqual!
*/
}
public boolean isEmpty() {
	return tally == 0;
/*
udanax-top.st:49342:ActualArray methodsFor: 'testing'!
{BooleanVar} isEmpty
	^tally == UInt32Zero!
*/
}
/**
 * store the new value at the specified position.  Note that this is an insertion iff
 * the index is the same as the tally (that is, we're adding at the first empty position
 * at the end of the array).
 */
public Heaper intStore(int index, Heaper value) {
	int reali;
	Heaper old;
	if (value == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);
	}
	if (index < 0 || (index > tally)) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_DOMAIN);
	}
	reali = index;
	if (reali == tally) {
		if (reali >= elements.count()) {
			enlarge();
		}
		tally = tally + 1;
	}
	old = elements.fetch(reali);
	elements.store(reali, value);
	return old;
/*
udanax-top.st:49347:ActualArray methodsFor: 'accessing'!
{Heaper} atInt: index {IntegerVar} store: value {Heaper} 
	"store the new value at the specified position.  Note that this is an insertion iff
	the index is the same as the tally (that is, we're adding at the first empty position
	at the end of the array)."
	
	| reali {UInt32} old {Heaper} |
	value == NULL ifTrue: [Heaper BLAST: #NullInsertion].
	(index < IntegerVar0 or: [index > tally])
		ifTrue: [Heaper BLAST: #NotInDomain].
	reali _ index DOTasLong.
	reali = tally
		ifTrue: 
			[reali >= elements count ifTrue: [self enlarge].
			tally _ tally + 1].
	old _ elements fetch: reali.
	elements at: reali store: value.
	^ old!
*/
}
public CoordinateSpace coordinateSpace() {
	return IntegerSpace.make();
/*
udanax-top.st:49365:ActualArray methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^IntegerSpace make!
*/
}
public int count() {
	return tally;
/*
udanax-top.st:49369:ActualArray methodsFor: 'accessing'!
{IntegerVar} count
	^tally!
*/
}
public XnRegion domain() {
	return IntegerRegion.make(0, tally);
/*
udanax-top.st:49373:ActualArray methodsFor: 'accessing'!
{XnRegion} domain
	^IntegerRegion make: IntegerVar0 with: tally!
*/
}
public int highestIndex() {
	return tally - 1;
/*
udanax-top.st:49377:ActualArray methodsFor: 'accessing'!
{IntegerVar} highestIndex
	^ tally - 1!
*/
}
public Heaper intFetch(int index) {
	int idx;
	if ((idx = index) >= tally || (index < 0)) {
		return null;
	}
	else {
		return elements.fetch(idx);
	}
/*
udanax-top.st:49380:ActualArray methodsFor: 'accessing'!
{Heaper} intFetch: index {IntegerVar} 
	| idx {UInt32 register} |
	((idx _ index DOTasLong) >= tally or: [index < IntegerVar0])
		ifTrue: [^NULL]
		ifFalse: [^elements fetch: idx]!
*/
}
/**
 * Remove if the index is the last thing in the table.
 * Blast if the index is some other element of the table.
 * *Ignore* the request if it is any element not in the table.
 */
public boolean intWipe(int index) {
	int reali;
	reali = index;
	if (reali == (tally - 1)) {
		elements.store(reali, null);
		tally = tally - 1;
		return true;
	}
	/* Now the error that results from a specialized implementation. */
	if (reali >= 0 && (reali < tally)) {
		throw new AboraRuntimeException(AboraRuntimeException.INCOMPLETE_ABSTRACTION);
	}
	return false;
/*
udanax-top.st:49386:ActualArray methodsFor: 'accessing'!
{BooleanVar} intWipe: index {IntegerVar} 
	"Remove if the index is the last thing in the table. 
	 Blast if the index is some other element of the table.  
	 *Ignore* the request if it is any element not in the table."
	
	| reali {Int32 register} |
	reali _ index DOTasLong.
	reali == (tally - 1) ifTrue:
		[elements at: reali store: NULL.
		tally _ tally - 1.
		^true].
	"Now the error that results from a specialized implementation."
	(reali >= Int32Zero and: [reali < tally]) ifTrue: [Heaper BLAST: #IncompleteAbstraction].
	^false!
*/
}
public int lowestIndex() {
	return 0;
/*
udanax-top.st:49401:ActualArray methodsFor: 'accessing'!
{IntegerVar} lowestIndex
	^ IntegerVar0!
*/
}
public ScruTable offsetSubTableBetween(int startIndex, int stopIndex, int firstIndex) {
	return super.offsetSubTableBetween(startIndex, stopIndex, firstIndex);
/*
udanax-top.st:49405:ActualArray methodsFor: 'accessing'!
{ScruTable} offsetSubTableBetween: startIndex {IntegerVar} 
	with: stopIndex {IntegerVar} 
	with: firstIndex {IntegerVar} 
	^super offsetSubTableBetween: startIndex with: stopIndex with: firstIndex!
*/
}
public ScruTable subTable(XnRegion reg) {
	return subTableBetween(((IntegerRegion) reg).start(), ((IntegerRegion) reg).stop());
/*
udanax-top.st:49411:ActualArray methodsFor: 'accessing'!
{ScruTable} subTable: reg {XnRegion} 
	
	^self subTableBetween: (reg cast: IntegerRegion) start 
		with: (reg cast: IntegerRegion) stop!
*/
}
public ScruTable subTableBetween(int start, int stop) {
	int begin;
	int end;
	MuArray newArray;
	XnRegion reg;
	if (start < 0) {
		begin = 0;
	}
	else {
		begin = start;
	}
	if (stop > count()) {
		end = count();
	}
	else {
		end = stop;
	}
	newArray = (MuArray) MuArray.makeIntegerVar(end - begin);
	reg = IntegerRegion.make(begin, end);
	Stepper stomper = reg.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		IntegerPos pos = (IntegerPos) stomper.fetch();
		if (pos == null) {
			continue ;
		}
		newArray.intIntroduce((pos.asIntegerVar() - begin), (intFetch(pos.asIntegerVar())));
	}
	stomper.destroy();
	if (begin > 0) {
		return OffsetScruArray.make(newArray, (IntegerMapping.make(begin)));
	}
	else {
		return newArray;
	}
/*
udanax-top.st:49416:ActualArray methodsFor: 'accessing'!
{ScruTable} subTableBetween: start {IntegerVar} with: stop {IntegerVar} 
	| begin {IntegerVar} end {IntegerVar} newArray {MuArray} reg {XnRegion} |
	start < IntegerVar0 ifTrue: [begin _ IntegerVar0] ifFalse: [begin _ start].
	stop > self count
		ifTrue: [end _ self count]
		ifFalse: [end _ stop].
	newArray _ MuArray make.IntegerVar: end - begin.
	reg _ IntegerRegion make: begin with: end.
	reg stepper forEach: [:pos {IntegerPos} |
		newArray atInt: (pos asIntegerVar - begin)
					introduce: (self intFetch: pos asIntegerVar)].
	begin > IntegerVar0
		ifTrue: [^OffsetScruArray make: newArray with: (IntegerMapping make: begin)]
		ifFalse: [^newArray]!
*/
}
public ScruTable copy() {
	return new ActualArray(((PtrArray) elements.copy()), tally);
/*
udanax-top.st:49433:ActualArray methodsFor: 'creation'!
{ScruTable} copy
	^ ActualArray create: (elements copy cast: PtrArray) with: tally!
*/
}
public ScruTable emptySize(int size) {
	return MuArray.makeIntegerVar((elements.count()));
/*
udanax-top.st:49436:ActualArray methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar unused}
	^MuArray make.IntegerVar: (elements count)!
*/
}
/**
 * The optional argument just hints at the number of elements
 * to eventually be added.  It makes no difference semantically.
 */
public ActualArray() {
	super();
	elements = PtrArray.nulls(4);
	tally = 0;
/*
udanax-top.st:49442:ActualArray methodsFor: 'private: creation'!
create
	"The optional argument just hints at the number of elements
	 to eventually be added.  It makes no difference semantically."
	super create.
	elements _ PtrArray nulls: 4.
	tally _ UInt32Zero!
*/
}
/**
 * The optional argument just hints at the number of elements
 * to eventually be added.  It makes no difference semantically.
 */
public ActualArray(int size) {
	super();
	int newSize;
	if (size > 4) {
		newSize = size;
	}
	else {
		newSize = 4;
	}
	elements = PtrArray.nulls(newSize);
	tally = 0;
/*
udanax-top.st:49449:ActualArray methodsFor: 'private: creation'!
create.IntegerVar: size {IntegerVar} 
	"The optional argument just hints at the number of elements
	 to eventually be added.  It makes no difference semantically."
	| newSize {UInt32} |
	super create.
	size > 4 ifTrue: [newSize _ size DOTasLong] ifFalse: [newSize _ 4].
	elements _ PtrArray nulls: newSize.
	tally _ UInt32Zero!
*/
}
public ActualArray(PtrArray newElems, int newTally) {
	super();
	elements = newElems;
	tally = newTally;
/*
udanax-top.st:49458:ActualArray methodsFor: 'private: creation'!
create: newElems {PtrArray of: Heaper} with: newTally {UInt32} 
	super create.
	elements _ newElems.
	tally _ newTally!
*/
}
public void destruct() {
	elements.destroy();
	elements = null;
	super.destruct();
/*
udanax-top.st:49463:ActualArray methodsFor: 'private: creation'!
{void} destruct
	elements destroy.
	elements _ NULL.
	super destruct!
*/
}
public XnRegion runAtInt(int anIdx) {
	int idx;
	Heaper lastObj;
	boolean notDone;
	idx = anIdx;
	if (idx < 0 || (idx >= tally)) {
		return IntegerRegion.make();
	}
	lastObj = intGet(idx);
	notDone = true;
	while (idx < tally && (notDone)) {
		if ((intGet(idx)).isEqual(lastObj)) {
			idx = idx + 1;
		}
		else {
			notDone = false;
		}
	}
	return IntegerRegion.make(anIdx, idx);
/*
udanax-top.st:49470:ActualArray methodsFor: 'runs'!
{XnRegion} runAtInt: anIdx {IntegerVar} 
	| idx {IntegerVar} lastObj {Heaper} notDone {BooleanVar} |
	
	idx _ anIdx.
	(idx < IntegerVar0 or: [idx >= tally]) ifTrue:
		[ ^ IntegerRegion make ].
	lastObj _ self intGet: idx.
	notDone _ true.
	[idx < tally and: [notDone]] whileTrue:
		[((self intGet: idx) isEqual: lastObj)
			ifTrue: [ idx _ idx + 1 ]
			ifFalse: [ notDone _ false ]].
	^ IntegerRegion make: anIdx with: idx!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print(getAboraClass().name());
	printOnWithSimpleSyntax(aStream, "[", ",", "]");
/*
udanax-top.st:49486:ActualArray methodsFor: 'printing'!
{void} printOn: aStream {ostream reference} 
	aStream << self getCategory name.
	self printOnWithSimpleSyntax: aStream
		with: '['
		with: ','
		with: ']'!
*/
}
/**
 * return the elements array for rapid processing
 */
public PtrArray elementsArray() {
	return elements;
/*
udanax-top.st:49495:ActualArray methodsFor: 'private: private'!
{PtrArray} elementsArray
	"return the elements array for rapid processing"
	^ elements!
*/
}
/**
 * return the size of the elements array for rapid processing
 */
public int endOffset() {
	return tally-1;
/*
udanax-top.st:49499:ActualArray methodsFor: 'private: private'!
{UInt32} endOffset
	"return the size of the elements array for rapid processing"
	^ tally-1!
*/
}
/**
 * Enlarge the receiver to contain more slots filled with nil.
 */
public void enlarge() {
	PtrArray newElements;
	PtrArray oldElements;
	newElements = (PtrArray) (elements.copyGrow(elements.count()));
	/* Just for the hell of it, I make this robust for asynchronous readers... */
	oldElements = elements;
	elements = newElements;
	oldElements.destroy();
/*
udanax-top.st:49503:ActualArray methodsFor: 'private: private'!
{void} enlarge
	"Enlarge the receiver to contain more slots filled with nil."
	| newElements {PtrArray of: Heaper} oldElements {PtrArray wimpy of: Heaper} |
	newElements _ (elements copyGrow: elements count) cast: PtrArray.
	"Just for the hell of it, I make this robust for asynchronous readers..."
	oldElements _ elements.
	elements _ newElements.
	oldElements destroy!
*/
}
/**
 * return the size of the elements array for rapid processing
 */
public int maxElements() {
	return elements.count();
/*
udanax-top.st:49513:ActualArray methodsFor: 'private: private'!
{UInt32} maxElements
	"return the size of the elements array for rapid processing"
	^ elements count!
*/
}
/**
 * return the size of the elements array for rapid processing
 */
public int startOffset() {
	return 0;
/*
udanax-top.st:49517:ActualArray methodsFor: 'private: private'!
{UInt32} startOffset
	"return the size of the elements array for rapid processing"
	^ UInt32Zero!
*/
}
/*
udanax-top.st:49523:ActualArray methodsFor: 'smalltalk: private:'!
{void} inspect
	^InspectorView open: (IntegerTableInspector inspect: self)!
*/
/*
udanax-top.st:49526:ActualArray methodsFor: 'smalltalk: private:'!
{IntegerVar} search: item {Integer}
	| low {IntegerVar} high {IntegerVar} curr {IntegerVar} elem {IntegerVar} |
	self isEmpty ifTrue: [^ Integer IntegerVar: 0].
	low _ self lowestIndex.
	high _ self highestIndex.
	[high >= low] whileTrue: [
		curr _ (high + low // 2).
		(item > (elem _ (self fetch: curr)))
			ifTrue: [low _ curr + 1]
			ifFalse: [item == elem 
				ifTrue: [low _ (high _ curr) + 1]
				ifFalse: [high _ curr - 1]]].
	^ high!
*/
public TableStepper stepper(OrderSpec order) {
	if (order == null) {
		return AscendingArrayStepper.make(this, 0, tally - 1);
	}
	else {
		if (order.followsInt(1, 0)) {
			return AscendingArrayStepper.make(this);
		}
		else {
			return IntegerTableStepper.make(this, order);
		}
	}
/*
udanax-top.st:49543:ActualArray methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL} 
	order == NULL
		ifTrue: [^AscendingArrayStepper
				make: self
				with: IntegerVar0
				with: tally - 1]
		ifFalse: [(order followsInt: 1 with: IntegerVar0)
				ifTrue: [^AscendingArrayStepper make: self]
				ifFalse: [^IntegerTableStepper make: self with: order]]!
*/
}
public Heaper store(Position key, Heaper value) {
	return intStore(((IntegerPos) key).asIntegerVar(), value);
/*
udanax-top.st:49556:ActualArray methodsFor: 'overload junk'!
{Heaper} at: key {Position} store: value {Heaper} 
	^ self atInt: (key cast: IntegerPos) asIntegerVar store: value!
*/
}
public Heaper fetch(Position key) {
	return intFetch((((IntegerPos) key).asIntegerVar()));
/*
udanax-top.st:49560:ActualArray methodsFor: 'overload junk'!
{Heaper} fetch: key {Position} 
	^ self intFetch: ((key cast: IntegerPos) asIntegerVar)!
*/
}
public XnRegion runAt(Position anIdx) {
	return runAtInt((((IntegerPos) anIdx).asIntegerVar()));
/*
udanax-top.st:49564:ActualArray methodsFor: 'overload junk'!
{XnRegion} runAt: anIdx {Position} 
	^ self runAtInt: ((anIdx cast: IntegerPos) asIntegerVar)!
*/
}
public boolean wipe(Position key) {
	return intWipe((((IntegerPos) key).asIntegerVar()));
/*
udanax-top.st:49567:ActualArray methodsFor: 'overload junk'!
{BooleanVar} wipe: key {Position}
	^ self intWipe: ((key cast: IntegerPos) asIntegerVar)!
*/
}
public ActualArray(Rcvr receiver) {
	super(receiver);
	elements = (PtrArray) receiver.receiveHeaper();
	tally = receiver.receiveUInt32();
/*
udanax-top.st:49572:ActualArray methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	elements _ receiver receiveHeaper.
	tally _ receiver receiveUInt32.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(elements);
	xmtr.sendUInt32(tally);
/*
udanax-top.st:49577:ActualArray methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: elements.
	xmtr sendUInt32: tally.!
*/
}
}
