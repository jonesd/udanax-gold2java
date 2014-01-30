/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.edgeregion;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.edge.EdgeManager;
import info.dgjones.abora.gold.edgeregion.EdgeSimpleRegionStepper;
import info.dgjones.abora.gold.edgeregion.EdgeStepper;
import info.dgjones.abora.gold.edgeregion.TransitionEdge;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Consider this a "protected" class.  See class comment in EdgeAccumulator
 */
public class EdgeSimpleRegionStepper extends Stepper {

	protected EdgeManager myManager;
	protected EdgeStepper myEdges;
	protected XnRegion mySimple;
/*
udanax-top.st:53520:
Stepper subclass: #EdgeSimpleRegionStepper
	instanceVariableNames: '
		myManager {EdgeManager}
		myEdges {EdgeStepper}
		mySimple {XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-EdgeRegion'!
*/
/*
udanax-top.st:53527:
EdgeSimpleRegionStepper comment:
'Consider this a "protected" class.  See class comment in EdgeAccumulator'!
*/
/*
udanax-top.st:53529:
(EdgeSimpleRegionStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:53611:
EdgeSimpleRegionStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:53614:
(EdgeSimpleRegionStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EdgeSimpleRegionStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	return mySimple;
/*
udanax-top.st:53534:EdgeSimpleRegionStepper methodsFor: 'accessing'!
{Heaper wimpy} fetch
	^mySimple!
*/
}
public boolean hasValue() {
	return mySimple != null;
/*
udanax-top.st:53537:EdgeSimpleRegionStepper methodsFor: 'accessing'!
{BooleanVar} hasValue
	^mySimple ~~ NULL!
*/
}
public void step() {
	boolean startsInside;
	TransitionEdge one;
	TransitionEdge two;
	/* if there are no more edges
		then the stepper is empty
		
	remember whether we're entering or leaving the region
	fetch the first edge
	if there is no first edge
		then remember the edges are gone
		if we were already in the region
			then there is the full region
			else we're empty
	else there is a first edge, so
		if we start outside and there is another edge
			then get it and make a two-sided region
			else make a one-side region */
	if (myEdges == null) {
		mySimple = null;
		return ;
	}
	startsInside = ! myEdges.isEntering();
	one = myEdges.fetchEdge();
	if (one == null) {
		myEdges = null;
		if (startsInside && (mySimple == null)) {
			mySimple = myManager.makeNew(true, PtrArray.empty());
		}
		else {
			mySimple = null;
		}
	}
	else {
		myEdges.step();
		if ( ! startsInside && (myEdges.hasValue())) {
			two = myEdges.fetchEdge();
			myEdges.step();
			mySimple = myManager.makeNew(startsInside, ((PtrArray) (PrimSpec.pointer().arrayWithTwo(one, two))));
		}
		else {
			mySimple = myManager.makeNew(startsInside, ((PtrArray) (PrimSpec.pointer().arrayWith(one))));
		}
	}
/*
udanax-top.st:53540:EdgeSimpleRegionStepper methodsFor: 'accessing'!
{void} step
	| startsInside {BooleanVar} one {TransitionEdge} two {TransitionEdge} |
	"if there are no more edges
		then the stepper is empty
		
	remember whether we're entering or leaving the region
	fetch the first edge
	if there is no first edge
		then remember the edges are gone
		if we were already in the region
			then there is the full region
			else we're empty
	else there is a first edge, so
		if we start outside and there is another edge
			then get it and make a two-sided region
			else make a one-side region"
	
	myEdges == NULL ifTrue:
		[mySimple := NULL.
		^VOID].
	
	startsInside := myEdges isEntering not.
	one := myEdges fetchEdge.
	one == NULL ifTrue:
		[myEdges := NULL.
		(startsInside and: [mySimple == NULL]) ifTrue:
			[mySimple := myManager makeNew: true with: PtrArray empty]
		ifFalse:
			[mySimple := NULL]]
	ifFalse:
		[myEdges step.
		(startsInside not and: [myEdges hasValue]) ifTrue:
			[two := myEdges fetchEdge.
			myEdges step.
			mySimple := myManager makeNew: startsInside
				with: ((PrimSpec pointer arrayWithTwo: one with: two) cast: PtrArray)]
		ifFalse:
			[mySimple := myManager makeNew: startsInside
				with: ((PrimSpec pointer arrayWith: one) cast: PtrArray)]].!
*/
}
public Stepper copy() {
	EdgeStepper step;
	/* can't to ?: with SPTRs */
	step = myEdges;
	if (step != null) {
		step = (EdgeStepper) myEdges.copy();
	}
	return new EdgeSimpleRegionStepper(myManager, step, mySimple);
/*
udanax-top.st:53583:EdgeSimpleRegionStepper methodsFor: 'create'!
{Stepper} copy
	| step {EdgeStepper} |
	"can't to ?: with SPTRs"
	step := myEdges.
	step ~~ NULL ifTrue:
		[step := myEdges copy cast: EdgeStepper].
	^ EdgeSimpleRegionStepper create: myManager with: step with: mySimple!
*/
}
public EdgeSimpleRegionStepper(EdgeManager manager, EdgeStepper edges) {
	super();
	myManager = manager;
	myEdges = edges;
	mySimple = null;
	step();
/*
udanax-top.st:53593:EdgeSimpleRegionStepper methodsFor: 'protected: create'!
create: manager {EdgeManager} with: edges {EdgeStepper}
	super create.
	myManager := manager.
	myEdges := edges.
	mySimple := NULL.
	self step.!
*/
}
public EdgeSimpleRegionStepper(EdgeManager manager, EdgeStepper edges, XnRegion simple) {
	super();
	myManager = manager;
	myEdges = edges;
	mySimple = simple;
/*
udanax-top.st:53601:EdgeSimpleRegionStepper methodsFor: 'protected: create'!
create: manager {EdgeManager}
	with: edges {EdgeStepper | NULL}
	with: simple {XnRegion | NULL}
	
	super create.
	myManager := manager.
	myEdges := edges.
	mySimple := simple!
*/
}
public static EdgeSimpleRegionStepper make(EdgeManager manager, EdgeStepper edges) {
	return new EdgeSimpleRegionStepper(manager, edges);
/*
udanax-top.st:53619:EdgeSimpleRegionStepper class methodsFor: 'create'!
make: manager {EdgeManager} with: edges {EdgeStepper}
	^ self create: manager with: edges!
*/
}
public EdgeSimpleRegionStepper() {
/*

Generated during transformation
*/
}
public EdgeSimpleRegionStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
