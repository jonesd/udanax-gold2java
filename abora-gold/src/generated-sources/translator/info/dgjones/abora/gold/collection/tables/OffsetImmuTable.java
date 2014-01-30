/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.tables;

import info.dgjones.abora.gold.collection.steppers.OffsetScruTableStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ImmuTable;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.OffsetImmuTable;
import info.dgjones.abora.gold.collection.tables.OffsetScruTable;
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
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class OffsetImmuTable extends ImmuTable {

	protected ImmuTable myTable;
	protected Dsp myDsp;
/*
udanax-top.st:47533:
ImmuTable subclass: #OffsetImmuTable
	instanceVariableNames: '
		myTable {ImmuTable}
		myDsp {Dsp}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:47539:
(OffsetImmuTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OffsetImmuTable.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public CoordinateSpace coordinateSpace() {
	return myTable.coordinateSpace();
/*
udanax-top.st:47544:OffsetImmuTable methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^myTable coordinateSpace!
*/
}
public int count() {
	return myTable.count();
/*
udanax-top.st:47548:OffsetImmuTable methodsFor: 'accessing'!
{IntegerVar} count
	^myTable count!
*/
}
public XnRegion domain() {
	return myDsp.ofAll(myTable.domain());
/*
udanax-top.st:47552:OffsetImmuTable methodsFor: 'accessing'!
{XnRegion} domain
	^myDsp ofAll: myTable domain!
*/
}
public Heaper fetch(Position anIndex) {
	return myTable.intFetch((myDsp.inverseOfInt(((IntegerPos) anIndex).asIntegerVar())));
/*
udanax-top.st:47556:OffsetImmuTable methodsFor: 'accessing'!
{Heaper} fetch: anIndex {Position} 
	^myTable intFetch: (myDsp inverseOfInt: (anIndex cast: IntegerPos) asIntegerVar)!
*/
}
public Heaper intFetch(int idx) {
	return myTable.intFetch((myDsp.inverseOfInt(idx)));
/*
udanax-top.st:47560:OffsetImmuTable methodsFor: 'accessing'!
{Heaper} intFetch: idx {IntegerVar} 
	^myTable intFetch: (myDsp inverseOfInt: idx)!
*/
}
public ScruTable subTable(XnRegion encl) {
	return new OffsetScruTable((myTable.subTable((myDsp.inverseOfAll(encl)))), myDsp);
/*
udanax-top.st:47563:OffsetImmuTable methodsFor: 'accessing'!
{ScruTable} subTable: encl {XnRegion}
	^OffsetScruTable create: (myTable subTable: (myDsp inverseOfAll: encl)) with: myDsp.!
*/
}
public ScruTable transformedBy(Dsp dsp) {
	if (myDsp.inverse().isEqual(dsp)) {
		return myTable;
	}
	else {
		return new OffsetScruTable(myTable, (dsp.compose(myDsp)));
	}
/*
udanax-top.st:47567:OffsetImmuTable methodsFor: 'accessing'!
{ScruTable} transformedBy: dsp {Dsp} 
	(myDsp inverse isEqual: dsp) 
		ifTrue: [^myTable]
		ifFalse: [^OffsetScruTable create: myTable with: (dsp compose: myDsp)]!
*/
}
public XnRegion runAt(Position key) {
	if (includesKey((myDsp.inverseOf(key)))) {
		return key.asRegion();
	}
	else {
		return myTable.coordinateSpace().emptyRegion();
	}
/*
udanax-top.st:47575:OffsetImmuTable methodsFor: 'runs'!
{XnRegion} runAt: key {Position} 
	(self includesKey: (myDsp inverseOf: key))
		ifTrue: [^ key asRegion]
		ifFalse: [^ myTable coordinateSpace emptyRegion]!
*/
}
public XnRegion runAtInt(int anIdx) {
	return myDsp.ofAll((myTable.runAtInt((myDsp.inverseOfInt(anIdx)))));
/*
udanax-top.st:47581:OffsetImmuTable methodsFor: 'runs'!
{XnRegion} runAtInt: anIdx {IntegerVar} 
	^myDsp ofAll: (myTable runAtInt: (myDsp inverseOfInt: anIdx))!
*/
}
public int actualHashForEqual() {
	return HashHelper.hashForEqual(this.getClass()) + myTable.hashForEqual() + myDsp.hashForEqual();
/*
udanax-top.st:47586:OffsetImmuTable methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^#cat.U.OffsetImmuTable hashForEqual + myTable hashForEqual + myDsp hashForEqual!
*/
}
public boolean includesIntKey(int aKey) {
	return myTable.includesIntKey((myDsp.inverseOfInt(aKey)));
/*
udanax-top.st:47589:OffsetImmuTable methodsFor: 'testing'!
{BooleanVar} includesIntKey: aKey {IntegerVar}
	^myTable includesIntKey: (myDsp inverseOfInt: aKey)!
*/
}
public boolean includesKey(Position aKey) {
	return myTable.includesKey((myDsp.inverseOf(aKey)));
/*
udanax-top.st:47592:OffsetImmuTable methodsFor: 'testing'!
{BooleanVar} includesKey: aKey {Position}
	
	^ myTable includesKey: (myDsp inverseOf: aKey)!
*/
}
public boolean isEmpty() {
	return myTable.isEmpty();
/*
udanax-top.st:47596:OffsetImmuTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	^myTable isEmpty!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print(getAboraClass().name());
	aStream.print("(");
	aStream.print(myDsp);
	aStream.print(", ");
	aStream.print(myTable);
	aStream.print(")");
/*
udanax-top.st:47601:OffsetImmuTable methodsFor: 'printing'!
{void} printOn: aStream {ostream reference} 
	aStream << self getCategory name << '(' << myDsp << ', ' << myTable << ')'!
*/
}
public OffsetImmuTable(ImmuTable table, Dsp dsp) {
	super();
	myTable = table;
	myDsp = dsp;
/*
udanax-top.st:47606:OffsetImmuTable methodsFor: 'creation'!
create: table {ImmuTable} with: dsp {Dsp}
	super create.
	myTable _ table.
	myDsp _ dsp!
*/
}
public ScruTable emptySize(int size) {
	return myTable.emptySize(size);
/*
udanax-top.st:47611:OffsetImmuTable methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar}
	^ myTable emptySize: size!
*/
}
public MuTable asMuTable() {
	MuTable newTab;
	TableStepper s;
	newTab = (myTable.emptySize(myTable.count())).asMuTable();
	Stepper stomper = (s = myTable.stepper());
	for (; stomper.hasValue(); stomper.step()) {
		Heaper e = (Heaper) stomper.fetch();
		if (e == null) {
			continue ;
		}
		newTab.store((myDsp.of(s.position())), e);
	}
	stomper.destroy();
	return newTab;
/*
udanax-top.st:47617:OffsetImmuTable methodsFor: 'conversion'!
{MuTable} asMuTable
	| newTab {MuTable} s {TableStepper} |
	newTab _ (myTable emptySize: myTable count) asMuTable.
	(s _ myTable stepper) forEach: [ :e {Heaper} |
		newTab at: (myDsp of: s position) store: e].
	^ newTab!
*/
}
public TableStepper stepper() {
	return new OffsetScruTableStepper((myTable.stepper()), myDsp);
/*
udanax-top.st:47626:OffsetImmuTable methodsFor: 'smalltalk: private:'!
{TableStepper} stepper
	^ OffsetScruTableStepper create.Stepper: (myTable stepper) with: myDsp!
*/
}
public TableStepper stepper(OrderSpec order) {
	return new OffsetScruTableStepper((myTable.stepper(order)), myDsp);
/*
udanax-top.st:47632:OffsetImmuTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL} 
	^OffsetScruTableStepper create.Stepper: (myTable stepper: order) with: myDsp!
*/
}
public Dsp innerDsp() {
	return myDsp;
/*
udanax-top.st:47638:OffsetImmuTable methodsFor: 'private: private'!
{Dsp} innerDsp
	^myDsp!
*/
}
public ScruTable innerTable() {
	return myTable;
/*
udanax-top.st:47641:OffsetImmuTable methodsFor: 'private: private'!
{ScruTable} innerTable
	^myTable!
*/
}
public ImmuTable combineWith(ImmuTable other) {
	MuTable newTable;
	TableStepper others;
	newTable = (MuTable) myTable.copy().asMuTable();
	others = other.stepper();
	while (others.hasValue()) {
		newTable.store((myDsp.inverseOf(others.position())), others.fetch());
		others.step();
	}
	others.destroy();
	return new OffsetImmuTable(newTable.asImmuTable(), myDsp);
/*
udanax-top.st:47646:OffsetImmuTable methodsFor: 'SEF manipulation'!
{ImmuTable} combineWith: other {ImmuTable}
	| newTable {MuTable} others {TableStepper} |
	newTable _ myTable copy asMuTable.
	others _ other stepper.
	[others hasValue] whileTrue:
		[newTable at: (myDsp inverseOf: others position) store: others fetch.
		others step].
	others destroy.
	^OffsetImmuTable create: newTable asImmuTable with: myDsp!
*/
}
public ImmuTable without(Position index) {
	return new OffsetImmuTable((myTable.without((myDsp.inverseOf(index)))), myDsp);
/*
udanax-top.st:47657:OffsetImmuTable methodsFor: 'SEF manipulation'!
{ImmuTable} without: index {Position} 
	^OffsetImmuTable create: (myTable without: (myDsp inverseOf: index)) with: myDsp!
*/
}
public OffsetImmuTable() {
/*

Generated during transformation
*/
}
public OffsetImmuTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
