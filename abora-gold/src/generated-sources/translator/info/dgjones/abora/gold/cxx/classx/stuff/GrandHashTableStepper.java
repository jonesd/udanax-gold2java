/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cxx.classx.stuff;

import info.dgjones.abora.gold.collection.grand.GrandHashTable;
import info.dgjones.abora.gold.collection.grand.GrandTableEntry;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandHashTableStepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandNodeStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class GrandHashTableStepper extends TableStepper {

	protected GrandHashTable table;
	protected GrandNodeStepper nodeStepper;
	protected int nodeIndex;
/*
udanax-top.st:55659:
TableStepper subclass: #GrandHashTableStepper
	instanceVariableNames: '
		table {GrandHashTable}
		nodeStepper {GrandNodeStepper | NULL}
		nodeIndex {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-class-stuff'!
*/
/*
udanax-top.st:55666:
(GrandHashTableStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandHashTableStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void verifyEntry() {
	while (nodeIndex < table.nodeCount() && ((table.nodeAt(nodeIndex)).isEmpty())) {
		nodeIndex = nodeIndex + 1;
	}
	if (nodeIndex < table.nodeCount()) {
		nodeStepper = new GrandNodeStepper((table.nodeAt(nodeIndex)));
	}
/*
udanax-top.st:55671:GrandHashTableStepper methodsFor: 'private: private'!
{void} verifyEntry
	[nodeIndex < table nodeCount and: [(table nodeAt: nodeIndex) isEmpty]]
		whileTrue:
			[nodeIndex _ nodeIndex + 1 ].
	nodeIndex < table nodeCount ifTrue:
		[nodeStepper _ GrandNodeStepper create: (table nodeAt: nodeIndex)]!
*/
}
public Heaper fetch() {
	if (nodeStepper != null && (nodeStepper.hasValue())) {
		return nodeStepper.entry().value();
	}
	else {
		return null;
	}
/*
udanax-top.st:55680:GrandHashTableStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	(nodeStepper ~~ NULL and: [nodeStepper hasValue])
		ifTrue: [^ nodeStepper entry value]
		ifFalse: [^NULL]!
*/
}
public boolean hasValue() {
	return nodeStepper != null;
/*
udanax-top.st:55685:GrandHashTableStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^ nodeStepper ~~ NULL!
*/
}
public void step() {
	nodeStepper.step();
	if ( ! (nodeStepper.hasValue())) {
		nodeStepper.destroy();
		nodeStepper = null;
		nodeIndex = nodeIndex + 1;
		verifyEntry();
	}
/*
udanax-top.st:55688:GrandHashTableStepper methodsFor: 'operations'!
{void} step
	nodeStepper step.
	nodeStepper hasValue ifFalse:
		[nodeStepper destroy.
		nodeStepper _ NULL.
		nodeIndex _ nodeIndex + 1.
		self verifyEntry]!
*/
}
public Position position() {
	return ((GrandTableEntry) nodeStepper.entry()).position();
/*
udanax-top.st:55698:GrandHashTableStepper methodsFor: 'special'!
{Position} position
	^ (nodeStepper entry cast: GrandTableEntry) position!
*/
}
public Stepper copy() {
	return new GrandHashTableStepper(table, nodeStepper, nodeIndex);
/*
udanax-top.st:55703:GrandHashTableStepper methodsFor: 'create'!
{Stepper} copy
	^ GrandHashTableStepper create: table with: nodeStepper with: nodeIndex!
*/
}
public GrandHashTableStepper(GrandHashTable aTable) {
	super();
	table = aTable;
	table.moreSteppers();
	nodeIndex = 0;
	nodeStepper = null;
	verifyEntry();
/*
udanax-top.st:55706:GrandHashTableStepper methodsFor: 'create'!
create: aTable {GrandHashTable}
	super create.
	table _ aTable.
	table moreSteppers.
	nodeIndex _ IntegerVar0.
	nodeStepper _ NULL.
	self verifyEntry!
*/
}
public GrandHashTableStepper(GrandHashTable aTable, GrandNodeStepper aNodeStepper, int aNodeIndex) {
	super();
	table = aTable;
	table.moreSteppers();
	nodeStepper = aNodeStepper;
	nodeIndex = aNodeIndex;
/*
udanax-top.st:55716:GrandHashTableStepper methodsFor: 'protected: creation'!
create: aTable {GrandHashTable} with: aNodeStepper {GrandNodeStepper} with: aNodeIndex {IntegerVar}
	super create.
	table _ aTable.
	table moreSteppers.
	nodeStepper _ aNodeStepper.
	nodeIndex _ aNodeIndex.!
*/
}
public void destruct() {
	if (nodeStepper != null) {
		nodeStepper.destroy();
	}
	table.fewerSteppers();
	super.destruct();
/*
udanax-top.st:55723:GrandHashTableStepper methodsFor: 'protected: creation'!
{void} destruct
	nodeStepper ~~ NULL ifTrue: [ nodeStepper destroy ].
	table fewerSteppers.
	super destruct.!
*/
}
public GrandHashTableStepper() {
/*

Generated during transformation
*/
}
public GrandHashTableStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
