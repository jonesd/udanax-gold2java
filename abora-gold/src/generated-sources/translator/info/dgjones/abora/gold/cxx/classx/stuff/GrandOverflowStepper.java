/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cxx.classx.stuff;

import info.dgjones.abora.gold.collection.grand.GrandEntry;
import info.dgjones.abora.gold.collection.grand.GrandOverflow;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandOverflowStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class GrandOverflowStepper extends Stepper {

	protected GrandOverflow overflow;
	protected int entryIndex;
	protected GrandOverflowStepper childStepper;
	protected int childIndex;
/*
udanax-top.st:54075:
Stepper subclass: #GrandOverflowStepper
	instanceVariableNames: '
		overflow {GrandOverflow}
		entryIndex {IntegerVar}
		childStepper {GrandOverflowStepper}
		childIndex {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-class-stuff'!
*/
/*
udanax-top.st:54083:
(GrandOverflowStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandOverflowStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void verifyEntry() {
	if (entryIndex < overflow.entryCount()) {
		while (entryIndex < overflow.entryCount() && ((overflow.entryAt(entryIndex)) == null)) {
			entryIndex = entryIndex + 1;
		}
	}
	if (entryIndex < overflow.entryCount()) {
		return ;
	}
	if (childIndex < overflow.childCount()) {
		while (childIndex < overflow.childCount() && ((overflow.childAt(childIndex)) == null)) {
			childIndex = childIndex + 1;
		}
		if (childIndex < overflow.childCount()) {
			childStepper = new GrandOverflowStepper((overflow.childAt(childIndex)));
		}
	}
/*
udanax-top.st:54088:GrandOverflowStepper methodsFor: 'private:'!
{void} verifyEntry
	entryIndex < overflow entryCount
		ifTrue:
			[[entryIndex < overflow entryCount and: [(overflow entryAt: entryIndex) == NULL]]
				whileTrue: [entryIndex _ entryIndex + 1]].
	entryIndex < overflow entryCount ifTrue: [ ^ VOID ].
	
	childIndex < overflow childCount
		ifTrue:
			[[childIndex < overflow childCount and: [(overflow childAt: childIndex) == NULL]]
				whileTrue: [childIndex _ childIndex + 1].
				childIndex < overflow childCount
					ifTrue: [ childStepper _ GrandOverflowStepper create: (overflow childAt: childIndex) ]]!
*/
}
public GrandEntry entry() {
	if (childStepper == null) {
		return overflow.entryAt(entryIndex);
	}
	else {
		return childStepper.entry();
	}
/*
udanax-top.st:54105:GrandOverflowStepper methodsFor: 'operations'!
{GrandEntry} entry
	childStepper == NULL
		ifTrue: [ ^ overflow entryAt: entryIndex ]
		ifFalse: [ ^ childStepper entry ]!
*/
}
public Heaper fetch() {
	shouldNotImplement();
	return null;
/*
udanax-top.st:54110:GrandOverflowStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	self shouldNotImplement.
	^ NULL!
*/
}
public boolean hasValue() {
	if (childStepper != null) {
		return childStepper.hasValue();
	}
	else {
		return entryIndex < overflow.entryCount() && (childIndex < overflow.childCount());
	}
/*
udanax-top.st:54114:GrandOverflowStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	childStepper ~~ NULL
		ifTrue: [ ^ childStepper hasValue ]
		ifFalse: [ ^ entryIndex < overflow entryCount and: [childIndex < overflow childCount]]!
*/
}
public void step() {
	if (childStepper != null) {
		childStepper.step();
		if (childStepper.hasValue()) {
			return ;
		}
		else {
			childStepper.destroy();
			childStepper = null;
			childIndex = childIndex + 1;
		}
	}
	else {
		entryIndex = entryIndex + 1;
	}
	verifyEntry();
/*
udanax-top.st:54119:GrandOverflowStepper methodsFor: 'operations'!
{void} step
	childStepper ~~ NULL
		ifTrue:
			[childStepper step.
			childStepper hasValue
				ifTrue: [ ^VOID ]
				ifFalse:
					[childStepper destroy.
					childStepper _ NULL.
					childIndex _ childIndex + 1]]
		ifFalse:
			[entryIndex _ entryIndex + 1].
	self verifyEntry!
*/
}
public Stepper copy() {
	return new GrandOverflowStepper(overflow, entryIndex, childStepper, childIndex);
/*
udanax-top.st:54135:GrandOverflowStepper methodsFor: 'create'!
{Stepper} copy
	^ GrandOverflowStepper create: overflow with: entryIndex with: childStepper with: childIndex!
*/
}
public GrandOverflowStepper(GrandOverflow aPage) {
	super();
	overflow = aPage;
	entryIndex = childIndex = 0;
	childStepper = null;
	verifyEntry();
/*
udanax-top.st:54138:GrandOverflowStepper methodsFor: 'create'!
create: aPage {GrandOverflow}
	super create.
	overflow _ aPage.
	entryIndex _ childIndex _ IntegerVar0.
	childStepper _ NULL.
	self verifyEntry.!
*/
}
public GrandOverflowStepper(GrandOverflow anOverflow, int entryIdx, GrandOverflowStepper child, int childIdx) {
	super();
	overflow = anOverflow;
	entryIndex = entryIdx;
	childStepper = child;
	childIndex = childIdx;
/*
udanax-top.st:54147:GrandOverflowStepper methodsFor: 'protected: creation'!
create: anOverflow {GrandOverflow} with: entryIdx {IntegerVar} with: child {GrandOverflowStepper} with: childIdx {IntegerVar}
	super create.
	overflow _ anOverflow.
	entryIndex _ entryIdx.
	childStepper _ child.
	childIndex _ childIdx.!
*/
}
public void destruct() {
	if (childStepper != null) {
		childStepper.destroy();
	}
	super.destruct();
/*
udanax-top.st:54154:GrandOverflowStepper methodsFor: 'protected: creation'!
{void} destruct
	childStepper ~~ NULL ifTrue: [ childStepper destroy ].
	super destruct.!
*/
}
public GrandOverflowStepper() {
/*

Generated during transformation
*/
}
public GrandOverflowStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
