/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cxx.classx.stuff;

import info.dgjones.abora.gold.collection.grand.GrandHashSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandHashSetStepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandNodeStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class GrandHashSetStepper extends Stepper {

	protected GrandHashSet set;
	protected GrandNodeStepper nodeStepper;
	protected int nodeIndex;
/*
udanax-top.st:53926:
Stepper subclass: #GrandHashSetStepper
	instanceVariableNames: '
		set {GrandHashSet}
		nodeStepper {GrandNodeStepper | NULL}
		nodeIndex {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-class-stuff'!
*/
/*
udanax-top.st:53933:
(GrandHashSetStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandHashSetStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void verifyEntry() {
	while (nodeIndex < set.nodeCount() && ((set.nodeAt(nodeIndex)).isEmpty())) {
		nodeIndex = nodeIndex + 1;
	}
	if (nodeIndex < set.nodeCount()) {
		nodeStepper = new GrandNodeStepper((set.nodeAt(nodeIndex)));
	}
/*
udanax-top.st:53938:GrandHashSetStepper methodsFor: 'private: private'!
{void} verifyEntry
	[nodeIndex < set nodeCount and: [(set nodeAt: nodeIndex) isEmpty]]
		whileTrue:
			[nodeIndex _ nodeIndex + 1 ].
	nodeIndex < set nodeCount ifTrue:
		[nodeStepper _ GrandNodeStepper create: (set nodeAt: nodeIndex)]!
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
udanax-top.st:53947:GrandHashSetStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	(nodeStepper ~~ NULL and: [nodeStepper hasValue])
		ifTrue: [^ nodeStepper entry value]
		ifFalse: [^ NULL]!
*/
}
public boolean hasValue() {
	return (nodeIndex < set.nodeCount()) && (nodeStepper.hasValue());
/*
udanax-top.st:53952:GrandHashSetStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^ (nodeIndex < set nodeCount) and: [nodeStepper hasValue]!
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
udanax-top.st:53955:GrandHashSetStepper methodsFor: 'operations'!
{void} step
	nodeStepper step.
	nodeStepper hasValue ifFalse:
		[nodeStepper destroy.
		nodeStepper _ NULL.
		nodeIndex _ nodeIndex + 1.
		self verifyEntry]!
*/
}
public GrandHashSetStepper(GrandHashSet aSet, GrandNodeStepper aNodeStepper, int aNodeIndex) {
	super();
	set = aSet;
	set.moreSteppers();
	nodeStepper = aNodeStepper;
	nodeIndex = aNodeIndex;
/*
udanax-top.st:53965:GrandHashSetStepper methodsFor: 'protected: create'!
create: aSet {GrandHashSet} with: aNodeStepper {GrandNodeStepper} with: aNodeIndex {IntegerVar}
	super create.
	set _ aSet.
	set moreSteppers.
	nodeStepper _ aNodeStepper.
	nodeIndex _ aNodeIndex.!
*/
}
public void destruct() {
	if (nodeStepper != null) {
		nodeStepper.destroy();
	}
	set.fewerSteppers();
	super.destruct();
/*
udanax-top.st:53972:GrandHashSetStepper methodsFor: 'protected: create'!
{void} destruct
	nodeStepper ~~ NULL ifTrue: [ nodeStepper destroy ].
	set fewerSteppers.
	super destruct.!
*/
}
public Stepper copy() {
	return new GrandHashSetStepper(set, nodeStepper, nodeIndex);
/*
udanax-top.st:53979:GrandHashSetStepper methodsFor: 'create'!
{Stepper} copy
	^ GrandHashSetStepper create: set with: nodeStepper with: nodeIndex!
*/
}
public GrandHashSetStepper(GrandHashSet aSet) {
	super();
	set = aSet;
	set.moreSteppers();
	nodeIndex = 0;
	nodeStepper = null;
	verifyEntry();
/*
udanax-top.st:53982:GrandHashSetStepper methodsFor: 'create'!
create: aSet {GrandHashSet}
	super create.
	set _ aSet.
	set moreSteppers.
	nodeIndex _ IntegerVar0.
	nodeStepper _ NULL.
	self verifyEntry!
*/
}
public GrandHashSetStepper() {
/*

Generated during transformation
*/
}
public GrandHashSetStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
