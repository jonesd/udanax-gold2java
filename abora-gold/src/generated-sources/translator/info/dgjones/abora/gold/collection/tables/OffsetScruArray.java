/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.tables;

import info.dgjones.abora.gold.collection.steppers.OffsetArrayStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ImmuTable;
import info.dgjones.abora.gold.collection.tables.MuArray;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.OffsetScruArray;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class OffsetScruArray extends ScruTable {

	protected MuArray myArray;
	protected Dsp myDsp;
/*
udanax-top.st:50363:
ScruTable subclass: #OffsetScruArray
	instanceVariableNames: '
		myArray {MuArray}
		myDsp {Dsp}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:50369:
(OffsetScruArray getOrMakeCxxClassDescription)
	friends:
'friend class XuArray;';
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
/*
udanax-top.st:50517:
OffsetScruArray class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:50520:
(OffsetScruArray getOrMakeCxxClassDescription)
	friends:
'friend class XuArray;';
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OffsetScruArray.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public CoordinateSpace coordinateSpace() {
	return myArray.coordinateSpace();
/*
udanax-top.st:50376:OffsetScruArray methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^myArray coordinateSpace!
*/
}
public int count() {
	return myArray.count();
/*
udanax-top.st:50380:OffsetScruArray methodsFor: 'accessing'!
{IntegerVar} count
	^myArray count!
*/
}
public XnRegion domain() {
	return myDsp.ofAll(myArray.domain());
/*
udanax-top.st:50384:OffsetScruArray methodsFor: 'accessing'!
{XnRegion} domain
	^myDsp ofAll: myArray domain!
*/
}
public Heaper fetch(Position anIndex) {
	return myArray.intFetch((myDsp.inverseOfInt(((IntegerPos) anIndex).asIntegerVar())));
/*
udanax-top.st:50388:OffsetScruArray methodsFor: 'accessing'!
{Heaper} fetch: anIndex {Position} 
	^myArray intFetch: (myDsp inverseOfInt: (anIndex cast: IntegerPos) asIntegerVar)!
*/
}
public Heaper intFetch(int idx) {
	return myArray.intFetch((myDsp.inverseOfInt(idx)));
/*
udanax-top.st:50392:OffsetScruArray methodsFor: 'accessing'!
{Heaper} intFetch: idx {IntegerVar} 
	^myArray intFetch: (myDsp inverseOfInt: idx)!
*/
}
public ScruTable subTable(XnRegion encl) {
	IntegerRegion lr;
	lr = (IntegerRegion) encl;
	return myArray.subTableBetween((myDsp.inverseOfInt(lr.start())), (myDsp.inverseOfInt(lr.stop())));
/*
udanax-top.st:50395:OffsetScruArray methodsFor: 'accessing'!
{ScruTable} subTable: encl {XnRegion}
	| lr {IntegerRegion} |
	lr _ encl cast: IntegerRegion.
	^myArray
		subTableBetween: (myDsp inverseOfInt: lr start)
			with: (myDsp inverseOfInt: lr stop)!
*/
}
public ScruTable subTableBetween(int startLoc, int endLoc) {
	return OffsetScruArray.make(((MuArray) (myArray.subTableBetween((myDsp.inverseOfInt(startLoc)), (myDsp.inverseOfInt(endLoc))))), myDsp);
/*
udanax-top.st:50403:OffsetScruArray methodsFor: 'accessing'!
{ScruTable} subTableBetween: startLoc {IntegerVar} with: endLoc {IntegerVar} 
	^OffsetScruArray make: ((myArray
			subTableBetween: (myDsp inverseOfInt: startLoc)
			with: (myDsp inverseOfInt: endLoc)) cast: MuArray)
		with: myDsp!
*/
}
public ScruTable transformedBy(Dsp dsp) {
	if (myDsp.inverse().isEqual(dsp)) {
		return myArray;
	}
	else {
		return OffsetScruArray.make(myArray, (dsp.compose(myDsp)));
	}
/*
udanax-top.st:50409:OffsetScruArray methodsFor: 'accessing'!
{ScruTable} transformedBy: dsp {Dsp} 
	(myDsp inverse isEqual: dsp) 
		ifTrue: [^myArray]
		ifFalse: [^OffsetScruArray make: myArray with: (dsp compose: myDsp)]!
*/
}
public XnRegion runAt(Position key) {
	return runAtInt(((IntegerPos) key).asIntegerVar());
/*
udanax-top.st:50417:OffsetScruArray methodsFor: 'runs'!
{XnRegion} runAt: key {Position} 
	^self runAtInt: (key quickCast: IntegerPos) asIntegerVar!
*/
}
public XnRegion runAtInt(int anIdx) {
	return myDsp.ofAll((myArray.runAtInt((myDsp.inverseOfInt(anIdx)))));
/*
udanax-top.st:50421:OffsetScruArray methodsFor: 'runs'!
{XnRegion} runAtInt: anIdx {IntegerVar} 
	^myDsp ofAll: (myArray runAtInt: (myDsp inverseOfInt: anIdx))!
*/
}
public int actualHashForEqual() {
	return HashHelper.hashForEqual(this.getClass()) + myArray.hashForEqual() + myDsp.hashForEqual();
/*
udanax-top.st:50426:OffsetScruArray methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^#cat.U.OffsetScruArray hashForEqual + myArray hashForEqual + myDsp hashForEqual!
*/
}
public boolean includesIntKey(int aKey) {
	return myArray.includesIntKey((myDsp.inverseOfInt(aKey)));
/*
udanax-top.st:50429:OffsetScruArray methodsFor: 'testing'!
{BooleanVar} includesIntKey: aKey {IntegerVar}
	^myArray includesIntKey: (myDsp inverseOfInt: aKey)!
*/
}
public boolean includesKey(Position aKey) {
	return includesIntKey((((IntegerPos) aKey).asIntegerVar()));
/*
udanax-top.st:50432:OffsetScruArray methodsFor: 'testing'!
{BooleanVar} includesKey: aKey {Position}
	
	^ self includesIntKey: ((aKey cast: IntegerPos) asIntegerVar)!
*/
}
public boolean isEmpty() {
	return myArray.isEmpty();
/*
udanax-top.st:50436:OffsetScruArray methodsFor: 'testing'!
{BooleanVar} isEmpty
	^myArray isEmpty!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof OffsetScruArray) {
		OffsetScruArray osa = (OffsetScruArray) other;
		return (osa.innerArray().isEqual(myArray)) && (osa.innerArray().isEqual(myArray));
	}
	else {
		return false;
	}
/*
udanax-top.st:50439:OffsetScruArray methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper} 
	other 
		cast: OffsetScruArray into: [:osa |
			^(osa innerArray isEqual: myArray)
			 and: [osa innerArray isEqual: myArray]]
		others: [^false].
	^false "fodder"!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print(getAboraClass().name());
	aStream.print("(");
	aStream.print(myDsp);
	aStream.print(", ");
	aStream.print(myArray);
	aStream.print(")");
/*
udanax-top.st:50450:OffsetScruArray methodsFor: 'printing'!
{void} printOn: aStream {ostream reference} 
	aStream << self getCategory name << '(' << myDsp << ', ' << myArray << ')'!
*/
}
public OffsetScruArray(MuArray array, Dsp dsp) {
	super();
	myArray = array;
	myDsp = dsp;
/*
udanax-top.st:50455:OffsetScruArray methodsFor: 'protected: create'!
create: array {MuArray} with: dsp {Dsp}
	super create.
	myArray _ array.
	myDsp _ dsp!
*/
}
public ScruTable copy() {
	return OffsetScruArray.make(((MuArray) myArray.copy()), myDsp);
/*
udanax-top.st:50462:OffsetScruArray methodsFor: 'creation'!
{ScruTable} copy
	^ OffsetScruArray make: (myArray copy cast: MuArray) with: myDsp!
*/
}
public ScruTable empty() {
	return myArray.emptySize(4);
/*
udanax-top.st:50465:OffsetScruArray methodsFor: 'creation'!
{ScruTable} empty
	^ myArray emptySize: 4!
*/
}
public ScruTable emptySize(int size) {
	return myArray.emptySize(size);
/*
udanax-top.st:50468:OffsetScruArray methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar}
	^ myArray emptySize: size!
*/
}
public ImmuTable asImmuTable() {
	return ImmuTable.offsetImmuTable(myArray.asImmuTable(), myDsp);
/*
udanax-top.st:50474:OffsetScruArray methodsFor: 'conversion'!
{ImmuTable} asImmuTable
	^ ImmuTable offsetImmuTable: myArray asImmuTable with: myDsp!
*/
}
public MuTable asMuTable() {
	MuTable newArray;
	TableStepper s;
	newArray = (myArray.emptySize(myArray.count())).asMuTable();
	Stepper stomper = (s = myArray.stepper());
	for (; stomper.hasValue(); stomper.step()) {
		Heaper e = (Heaper) stomper.fetch();
		if (e == null) {
			continue ;
		}
		newArray.intStore((myDsp.ofInt(s.index())), e);
	}
	stomper.destroy();
	return newArray;
/*
udanax-top.st:50478:OffsetScruArray methodsFor: 'conversion'!
{MuTable} asMuTable
	| newArray {MuTable} s {TableStepper} |
	newArray _ (myArray emptySize: myArray count) asMuTable.
	(s _ myArray stepper) forEach: [ :e {Heaper} |
		newArray atInt: (myDsp ofInt: s index) store: e].
	^ newArray!
*/
}
public TableStepper stepper() {
	return OffsetArrayStepper.make((myArray.stepper()), myDsp);
/*
udanax-top.st:50487:OffsetScruArray methodsFor: 'smalltalk: private'!
{TableStepper} stepper
	^ OffsetArrayStepper make: (myArray stepper) with: myDsp!
*/
}
public TableStepper stepper(OrderSpec order) {
	return OffsetArrayStepper.make((myArray.stepper(order)), myDsp);
/*
udanax-top.st:50492:OffsetScruArray methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL} 
	^OffsetArrayStepper make: (myArray stepper: order) with: myDsp!
*/
}
public MuArray innerArray() {
	return myArray;
/*
udanax-top.st:50498:OffsetScruArray methodsFor: 'private: private'!
{MuArray} innerArray
	^myArray!
*/
}
public Dsp innerDsp() {
	return myDsp;
/*
udanax-top.st:50501:OffsetScruArray methodsFor: 'private: private'!
{Dsp} innerDsp
	^myDsp!
*/
}
public OffsetScruArray(Rcvr receiver) {
	super(receiver);
	myArray = (MuArray) receiver.receiveHeaper();
	myDsp = (Dsp) receiver.receiveHeaper();
/*
udanax-top.st:50506:OffsetScruArray methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myArray _ receiver receiveHeaper.
	myDsp _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myArray);
	xmtr.sendHeaper(myDsp);
/*
udanax-top.st:50511:OffsetScruArray methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myArray.
	xmtr sendHeaper: myDsp.!
*/
}
public static ScruTable make(MuArray array, Dsp dsp) {
	return new OffsetScruArray(array, dsp);
/*
udanax-top.st:50527:OffsetScruArray class methodsFor: 'create'!
{ScruTable} make: array {MuArray} with: dsp {Dsp}
	^ self create: array with: dsp!
*/
}
public OffsetScruArray() {
/*

Generated during transformation
*/
}
}
