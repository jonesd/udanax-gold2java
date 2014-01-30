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
import info.dgjones.abora.gold.collection.grand.GrandNode;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandDataPageStepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandNodeStepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandOverflowStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class GrandNodeStepper extends Stepper {

	protected GrandNode node;
	protected GrandDataPageStepper pageStepper;
	protected int pageIndex;
	protected GrandOverflowStepper overflowStepper;
/*
udanax-top.st:53990:
Stepper subclass: #GrandNodeStepper
	instanceVariableNames: '
		node {GrandNode}
		pageStepper {GrandDataPageStepper}
		pageIndex {IntegerVar}
		overflowStepper {GrandOverflowStepper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-class-stuff'!
*/
/*
udanax-top.st:53998:
(GrandNodeStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandNodeStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public GrandNodeStepper(GrandNode aNode, GrandDataPageStepper curPageStepper, int curPageIndex, GrandOverflowStepper oflowStepper) {
	super();
	node = aNode;
	pageStepper = curPageStepper;
	pageIndex = curPageIndex;
	overflowStepper = oflowStepper;
/*
udanax-top.st:54003:GrandNodeStepper methodsFor: 'protected: creation'!
create: aNode {GrandNode} with: curPageStepper {GrandDataPageStepper} with: curPageIndex {IntegerVar} with: oflowStepper {GrandOverflowStepper}
	super create.
	node _ aNode.
	pageStepper _ curPageStepper.
	pageIndex _ curPageIndex.
	overflowStepper _ oflowStepper.!
*/
}
public void destruct() {
	if (pageStepper != null) {
		pageStepper.destroy();
	}
	if (overflowStepper != null) {
		overflowStepper.destroy();
	}
	super.destruct();
/*
udanax-top.st:54010:GrandNodeStepper methodsFor: 'protected: creation'!
{void} destruct
	pageStepper ~~ NULL ifTrue: [ pageStepper destroy ].
	overflowStepper ~~ NULL ifTrue: [ overflowStepper destroy ].
	super destruct.!
*/
}
public void verifyEntry() {
	while (pageIndex < node.pageCount() && ((node.pageAt(pageIndex)).isEmpty())) {
		pageIndex = pageIndex + 1;
	}
	if (pageIndex < node.pageCount()) {
		pageStepper = new GrandDataPageStepper((node.pageAt(pageIndex)));
	}
	else {
		if (overflowStepper == null && (node.fetchOverflow() != null)) {
			overflowStepper = new GrandOverflowStepper(node.fetchOverflow());
		}
		else {
			if (overflowStepper != null) {
				overflowStepper.destroy();
			}
			overflowStepper = null;
			if (node.fetchOldOverflow() != null) {
				overflowStepper = new GrandOverflowStepper(node.fetchOldOverflow());
			}
		}
	}
/*
udanax-top.st:54017:GrandNodeStepper methodsFor: 'private:'!
{void} verifyEntry
	[pageIndex < node pageCount and: [(node pageAt: pageIndex) isEmpty]]
		whileTrue:
			[pageIndex _ pageIndex + 1].
	pageIndex < node pageCount
		ifTrue:		
			[pageStepper _ GrandDataPageStepper create: (node pageAt: pageIndex)]
		ifFalse:
			[(overflowStepper == NULL and: [node fetchOverflow ~~ NULL])
				ifTrue:
					[overflowStepper _ GrandOverflowStepper create: node fetchOverflow]
				ifFalse:
					[overflowStepper ~~ NULL ifTrue:
						[overflowStepper destroy].
					overflowStepper _ NULL.
					node fetchOldOverflow ~~ NULL ifTrue:
						[overflowStepper _ GrandOverflowStepper create: node fetchOldOverflow]]]!
*/
}
public GrandEntry entry() {
	if (overflowStepper != null) {
		return overflowStepper.entry();
	}
	else {
		return pageStepper.entry();
	}
/*
udanax-top.st:54037:GrandNodeStepper methodsFor: 'operations'!
{GrandEntry} entry
	overflowStepper ~~ NULL 
		ifTrue: [ ^ overflowStepper entry ]
		ifFalse: [ ^ pageStepper entry ]!
*/
}
public Heaper fetch() {
	shouldNotImplement();
	return null;
/*
udanax-top.st:54042:GrandNodeStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	self shouldNotImplement.
	^ NULL!
*/
}
public boolean hasValue() {
	if (overflowStepper != null) {
		return overflowStepper.hasValue();
	}
	else {
		return pageStepper != null && (pageStepper.hasValue());
	}
/*
udanax-top.st:54046:GrandNodeStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	overflowStepper ~~ NULL
		ifTrue: [ ^ overflowStepper hasValue ]
		ifFalse: [ ^ pageStepper ~~ NULL and: [pageStepper hasValue] ]!
*/
}
public void step() {
	if (overflowStepper != null) {
		overflowStepper.step();
	}
	else {
		pageStepper.step();
		if ( ! (pageStepper.hasValue())) {
			pageStepper.destroy();
			pageStepper = null;
			pageIndex = pageIndex + 1;
			verifyEntry();
		}
	}
/*
udanax-top.st:54051:GrandNodeStepper methodsFor: 'operations'!
{void} step
	overflowStepper ~~ NULL
		ifTrue: [ overflowStepper step ]
		ifFalse:
			[pageStepper step.
			pageStepper hasValue ifFalse:
				[pageStepper destroy.
				pageStepper _ NULL.
				pageIndex _ pageIndex + 1.
				self verifyEntry]]!
*/
}
public Stepper copy() {
	return new GrandNodeStepper(node, pageStepper, pageIndex, overflowStepper);
/*
udanax-top.st:54064:GrandNodeStepper methodsFor: 'create'!
{Stepper} copy
	^ GrandNodeStepper create: node with: pageStepper with: pageIndex with: overflowStepper!
*/
}
public GrandNodeStepper(GrandNode aNode) {
	super();
	node = aNode;
	pageIndex = 0;
	pageStepper = null;
	overflowStepper = null;
	verifyEntry();
/*
udanax-top.st:54067:GrandNodeStepper methodsFor: 'create'!
create: aNode {GrandNode}
	super create.
	node _ aNode.
	pageIndex _ IntegerVar0.
	pageStepper _ NULL.
	overflowStepper _ NULL.
	self verifyEntry.!
*/
}
public GrandNodeStepper() {
/*

Generated during transformation
*/
}
public GrandNodeStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
