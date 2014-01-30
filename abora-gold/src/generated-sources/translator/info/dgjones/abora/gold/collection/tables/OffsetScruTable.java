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

public class OffsetScruTable extends ScruTable {

	protected ScruTable myTable;
	protected Dsp myDsp;
/*
udanax-top.st:50530:
ScruTable subclass: #OffsetScruTable
	instanceVariableNames: '
		myTable {ScruTable}
		myDsp {Dsp}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:50536:
(OffsetScruTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OffsetScruTable.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public CoordinateSpace coordinateSpace() {
	return myTable.coordinateSpace();
/*
udanax-top.st:50541:OffsetScruTable methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^myTable coordinateSpace!
*/
}
public int count() {
	return myTable.count();
/*
udanax-top.st:50545:OffsetScruTable methodsFor: 'accessing'!
{IntegerVar} count
	^myTable count!
*/
}
public XnRegion domain() {
	return myDsp.ofAll(myTable.domain());
/*
udanax-top.st:50549:OffsetScruTable methodsFor: 'accessing'!
{XnRegion} domain
	^myDsp ofAll: myTable domain!
*/
}
public Heaper fetch(Position anIndex) {
	return myTable.intFetch((myDsp.inverseOfInt(((IntegerPos) anIndex).asIntegerVar())));
/*
udanax-top.st:50553:OffsetScruTable methodsFor: 'accessing'!
{Heaper} fetch: anIndex {Position} 
	^myTable intFetch: (myDsp inverseOfInt: (anIndex cast: IntegerPos) asIntegerVar)!
*/
}
public Heaper intFetch(int idx) {
	return myTable.intFetch((myDsp.inverseOfInt(idx)));
/*
udanax-top.st:50557:OffsetScruTable methodsFor: 'accessing'!
{Heaper} intFetch: idx {IntegerVar} 
	^myTable intFetch: (myDsp inverseOfInt: idx)!
*/
}
public ScruTable subTable(XnRegion encl) {
	return new OffsetScruTable((myTable.subTable((myDsp.inverseOfAll(encl)))), myDsp);
/*
udanax-top.st:50560:OffsetScruTable methodsFor: 'accessing'!
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
udanax-top.st:50564:OffsetScruTable methodsFor: 'accessing'!
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
udanax-top.st:50572:OffsetScruTable methodsFor: 'runs'!
{XnRegion} runAt: key {Position} 
	(self includesKey: (myDsp inverseOf: key))
		ifTrue: [^ key asRegion]
		ifFalse: [^ myTable coordinateSpace emptyRegion]!
*/
}
public XnRegion runAtInt(int anIdx) {
	return myDsp.ofAll((myTable.runAtInt((myDsp.inverseOfInt(anIdx)))));
/*
udanax-top.st:50578:OffsetScruTable methodsFor: 'runs'!
{XnRegion} runAtInt: anIdx {IntegerVar} 
	^myDsp ofAll: (myTable runAtInt: (myDsp inverseOfInt: anIdx))!
*/
}
public int actualHashForEqual() {
	return HashHelper.hashForEqual(this.getClass()) + myTable.hashForEqual() + myDsp.hashForEqual();
/*
udanax-top.st:50583:OffsetScruTable methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^#cat.U.OffsetScruTable hashForEqual + myTable hashForEqual + myDsp hashForEqual!
*/
}
public boolean includesIntKey(int aKey) {
	return myTable.includesIntKey((myDsp.inverseOfInt(aKey)));
/*
udanax-top.st:50586:OffsetScruTable methodsFor: 'testing'!
{BooleanVar} includesIntKey: aKey {IntegerVar}
	^myTable includesIntKey: (myDsp inverseOfInt: aKey)!
*/
}
public boolean includesKey(Position aKey) {
	return myTable.includesKey((myDsp.inverseOf(aKey)));
/*
udanax-top.st:50589:OffsetScruTable methodsFor: 'testing'!
{BooleanVar} includesKey: aKey {Position}
	
	^ myTable includesKey: (myDsp inverseOf: aKey)!
*/
}
public boolean isEmpty() {
	return myTable.isEmpty();
/*
udanax-top.st:50593:OffsetScruTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	^myTable isEmpty!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof OffsetScruTable) {
		OffsetScruTable ost = (OffsetScruTable) other;
		return (ost.innerTable().isEqual(myTable)) && (ost.innerTable().isEqual(myTable));
	}
	else {
		return false;
	}
/*
udanax-top.st:50596:OffsetScruTable methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper} 
	other 
		cast: OffsetScruTable into: [:ost |
			^(ost innerTable isEqual: myTable)
			 and: [ost innerTable isEqual: myTable]]
		others: [^false].
	^ false "compiler fodder"!
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
udanax-top.st:50607:OffsetScruTable methodsFor: 'printing'!
{void} printOn: aStream {ostream reference} 
	aStream << self getCategory name << '(' << myDsp << ', ' << myTable << ')'!
*/
}
public ScruTable copy() {
	return new OffsetScruTable(myTable.copy(), myDsp);
/*
udanax-top.st:50612:OffsetScruTable methodsFor: 'creation'!
{ScruTable} copy
	^ OffsetScruTable create: myTable copy with: myDsp!
*/
}
public OffsetScruTable(ScruTable table, Dsp dsp) {
	super();
	myTable = table;
	myDsp = dsp;
/*
udanax-top.st:50615:OffsetScruTable methodsFor: 'creation'!
create: table {ScruTable} with: dsp {Dsp}
	super create.
	myTable _ table.
	myDsp _ dsp!
*/
}
public ScruTable emptySize(int size) {
	return myTable.emptySize(size);
/*
udanax-top.st:50620:OffsetScruTable methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar}
	^ myTable emptySize: size!
*/
}
public ImmuTable asImmuTable() {
	return new OffsetImmuTable(myTable.asImmuTable(), myDsp);
/*
udanax-top.st:50626:OffsetScruTable methodsFor: 'conversion'!
{ImmuTable} asImmuTable
	^ OffsetImmuTable create: myTable asImmuTable with: myDsp!
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
udanax-top.st:50630:OffsetScruTable methodsFor: 'conversion'!
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
udanax-top.st:50639:OffsetScruTable methodsFor: 'smalltalk: private'!
{TableStepper} stepper
	^ OffsetScruTableStepper create.Stepper: (myTable stepper) with: myDsp!
*/
}
public TableStepper stepper(OrderSpec order) {
	return new OffsetScruTableStepper((myTable.stepper(order)), myDsp);
/*
udanax-top.st:50645:OffsetScruTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL} 
	^OffsetScruTableStepper create.Stepper: (myTable stepper: order) with: myDsp!
*/
}
public Dsp innerDsp() {
	return myDsp;
/*
udanax-top.st:50651:OffsetScruTable methodsFor: 'private:'!
{Dsp} innerDsp
	^myDsp!
*/
}
public ScruTable innerTable() {
	return myTable;
/*
udanax-top.st:50654:OffsetScruTable methodsFor: 'private:'!
{ScruTable} innerTable
	^myTable!
*/
}
public OffsetScruTable() {
/*

Generated during transformation
*/
}
public OffsetScruTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
