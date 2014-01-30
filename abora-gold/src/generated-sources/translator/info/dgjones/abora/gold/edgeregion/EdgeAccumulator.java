/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.edgeregion;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Accumulator;
import info.dgjones.abora.gold.edge.EdgeManager;
import info.dgjones.abora.gold.edgeregion.EdgeAccumulator;
import info.dgjones.abora.gold.edgeregion.EdgeStepper;
import info.dgjones.abora.gold.edgeregion.TransitionEdge;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class EdgeAccumulator extends Accumulator {

	protected EdgeManager myManager;
	protected boolean myStartsInside;
	protected PtrArray myEdges;
	protected int myIndex;
	protected TransitionEdge myPending;
	protected boolean myResultGiven;
	protected static InstanceCache SomeAccumulators;
/*
udanax-top.st:11960:
Accumulator subclass: #EdgeAccumulator
	instanceVariableNames: '
		myManager {EdgeManager}
		myStartsInside {BooleanVar}
		myEdges {PtrArray of: TransitionEdge}
		myIndex {Int32}
		myPending {TransitionEdge}
		myResultGiven {BooleanVar NOCOPY}'
	classVariableNames: 'SomeAccumulators {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-EdgeRegion'!
*/
/*
udanax-top.st:11970:
(EdgeAccumulator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:12109:
EdgeAccumulator class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:12112:
(EdgeAccumulator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EdgeAccumulator.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public EdgeAccumulator(EdgeManager manager, boolean startsInside) {
	super();
	myManager = manager;
	myStartsInside = startsInside;
	myEdges = PtrArray.nulls(4);
	myIndex = -1;
	myPending = null;
	myResultGiven = false;
/*
udanax-top.st:11975:EdgeAccumulator methodsFor: 'protected: create'!
create: manager {EdgeManager} with: startsInside {BooleanVar}
	
	super create.
	myManager := manager.
	myStartsInside := startsInside.
	myEdges := PtrArray nulls: 4.
	myIndex := -1.
	myPending := NULL.
	myResultGiven := false!
*/
}
public EdgeAccumulator(EdgeManager manager, boolean startsInside, PtrArray edges, int index, TransitionEdge pending) {
	super();
	myManager = manager;
	myStartsInside = startsInside;
	myEdges = edges;
	myIndex = index;
	myPending = pending;
	myResultGiven = false;
/*
udanax-top.st:11985:EdgeAccumulator methodsFor: 'protected: create'!
create: manager {EdgeManager}
	with: startsInside {BooleanVar}
	with: edges {PtrArray of: TransitionEdge}
	with: index {Int32}
	with: pending {TransitionEdge}
	
	super create.
	myManager := manager.
	myStartsInside _ startsInside.
	myEdges _ edges.
	myIndex := index.
	myPending _ pending.
	myResultGiven := false!
*/
}
public Accumulator copy() {
	Heaper result;
	result = SomeAccumulators.fetch();
	myResultGiven = true;
	if (result == null) {
		return new EdgeAccumulator(myManager, myStartsInside, myEdges, myIndex, myPending);
	}
	else {
		return 
		/* TODO newBecome */
		new EdgeAccumulator(myManager, myStartsInside, myEdges, myIndex, myPending);
	}
/*
udanax-top.st:12001:EdgeAccumulator methodsFor: 'creation'!
{Accumulator} copy
	| result {Heaper} |
	result := SomeAccumulators fetch.
	myResultGiven := true.
	result == NULL
		ifTrue: [
			^EdgeAccumulator create: myManager
				with: myStartsInside
				with: myEdges
				with: myIndex
				with: myPending]
		ifFalse: [
			^(EdgeAccumulator new.Become: result) create: myManager
				with: myStartsInside
				with: myEdges
				with: myIndex
				with: myPending]!
*/
}
public void destroy() {
	if ( ! (SomeAccumulators.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:12019:EdgeAccumulator methodsFor: 'creation'!
{void} destroy
	(SomeAccumulators store: self) ifFalse: [super destroy]!
*/
}
public void step(Heaper someObj) {
	edge(((TransitionEdge) someObj));
/*
udanax-top.st:12024:EdgeAccumulator methodsFor: 'operations'!
{void} step: someObj {Heaper}
	self edge: (someObj cast: TransitionEdge)!
*/
}
public Heaper value() {
	return region();
/*
udanax-top.st:12027:EdgeAccumulator methodsFor: 'operations'!
{Heaper} value
	^self region!
*/
}
/**
 * add a transition at the given position. doing it again cancels it
 */
public void edge(TransitionEdge x) {
	if (myPending == null) {
		myPending = x;
	}
	else {
		if (myPending.isEqual(x)) {
			myPending = null;
		}
		else {
			storeStep(myPending);
			myPending = x;
		}
	}
/*
udanax-top.st:12032:EdgeAccumulator methodsFor: 'edge operations'!
{void} edge: x {TransitionEdge}
	"add a transition at the given position. doing it again cancels it"
	myPending == NULL ifTrue:
		[myPending := x]
	ifFalse:
		[(myPending isEqual: x) ifTrue:
			[myPending := NULL]
		ifFalse:
			[self storeStep: myPending.
			myPending := x]].!
*/
}
/**
 * add a whole bunch of edges at once, assuming that they are sorted and there are no
 * duplicates
 */
public void edges(EdgeStepper stepper) {
	/* do the first step manually in case it is the same as the current edge
	then do all the rest without checking for repeats */
	if (stepper.hasValue()) {
		TransitionEdge edge;
		edge(stepper.fetchEdge());
		stepper.step();
		while ((edge = (TransitionEdge) stepper.fetch()) != null) {
			if (myPending != null) {
				storeStep(myPending);
			}
			myPending = edge;
			stepper.step();
		}
	}
/*
udanax-top.st:12043:EdgeAccumulator methodsFor: 'edge operations'!
{void} edges: stepper {EdgeStepper}
	"add a whole bunch of edges at once, assuming that they are sorted and there are no duplicates"
	
	"do the first step manually in case it is the same as the current edge
	then do all the rest without checking for repeats"
		
	stepper hasValue ifTrue:
		[|edge {TransitionEdge} |
		self edge: stepper fetchEdge.
		stepper step.
		[(edge := stepper fetch cast: TransitionEdge) ~~ NULL] whileTrue: [
			myPending ~~ NULL ifTrue:
				[self storeStep: myPending].
			myPending := edge.
			stepper step]]!
*/
}
/**
 * make a region out of the accumulated edges
 */
public XnRegion region() {
	if (myPending != null) {
		storeStep(myPending);
		myPending = null;
	}
	myResultGiven = true;
	return myManager.makeNew(myStartsInside, myEdges, myIndex + 1);
/*
udanax-top.st:12059:EdgeAccumulator methodsFor: 'edge operations'!
{XnRegion} region
	"make a region out of the accumulated edges"
	myPending ~~ NULL ifTrue:
		[self storeStep: myPending.
		myPending := NULL].
	myResultGiven := true.
	^myManager makeNew: myStartsInside with: myEdges with: myIndex + 1!
*/
}
/**
 * Just store an edge into the array and increment the count
 */
public void storeStep(TransitionEdge edge) {
	myIndex = myIndex + 1;
	if (myIndex == myEdges.count()) {
		myEdges = (PtrArray) (myEdges.copyGrow(myEdges.count()));
		myResultGiven = false;
	}
	else {
		if (myResultGiven) {
			myEdges = (PtrArray) myEdges.copy();
			myResultGiven = false;
		}
	}
	myEdges.store(myIndex, edge);
/*
udanax-top.st:12070:EdgeAccumulator methodsFor: 'private:'!
{void} storeStep: edge {TransitionEdge}
	"Just store an edge into the array and increment the count"
	
	myIndex := myIndex + 1.
	myIndex = myEdges count
		ifTrue: [
			myEdges := (myEdges copyGrow: myEdges count) cast: PtrArray.
			myResultGiven := false]
		ifFalse: [
			myResultGiven ifTrue: [
				myEdges := myEdges copy cast: PtrArray.
				myResultGiven := false]].
	myEdges at: myIndex store: edge.!
*/
}
public void restartEdgeAccumulator(Rcvr rcvr) {
	myResultGiven = false;
/*
udanax-top.st:12086:EdgeAccumulator methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartEdgeAccumulator: rcvr {Rcvr unused}
	myResultGiven := false!
*/
}
public EdgeAccumulator(Rcvr receiver) {
	super(receiver);
	myManager = (EdgeManager) receiver.receiveHeaper();
	myStartsInside = receiver.receiveBooleanVar();
	myEdges = (PtrArray) receiver.receiveHeaper();
	myIndex = receiver.receiveInt32();
	myPending = (TransitionEdge) receiver.receiveHeaper();
	restartEdgeAccumulator(receiver);
/*
udanax-top.st:12091:EdgeAccumulator methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myManager _ receiver receiveHeaper.
	myStartsInside _ receiver receiveBooleanVar.
	myEdges _ receiver receiveHeaper.
	myIndex _ receiver receiveInt32.
	myPending _ receiver receiveHeaper.
	self restartEdgeAccumulator: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myManager);
	xmtr.sendBooleanVar(myStartsInside);
	xmtr.sendHeaper(myEdges);
	xmtr.sendInt32(myIndex);
	xmtr.sendHeaper(myPending);
/*
udanax-top.st:12100:EdgeAccumulator methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myManager.
	xmtr sendBooleanVar: myStartsInside.
	xmtr sendHeaper: myEdges.
	xmtr sendInt32: myIndex.
	xmtr sendHeaper: myPending.!
*/
}
public static void initTimeNonInherited() {
	SomeAccumulators = InstanceCache.make(8);
/*
udanax-top.st:12117:EdgeAccumulator class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeAccumulators := InstanceCache make: 8!
*/
}
public static void linkTimeNonInherited() {
	SomeAccumulators = null;
/*
udanax-top.st:12120:EdgeAccumulator class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeAccumulators := NULL!
*/
}
public static EdgeAccumulator make(EdgeManager manager, boolean startsInside) {
	Heaper result;
	result = SomeAccumulators.fetch();
	if (result == null) {
		return new EdgeAccumulator(manager, startsInside);
	}
	else {
		return 
		/* TODO newBecome */
		new EdgeAccumulator(manager, startsInside);
	}
/*
udanax-top.st:12125:EdgeAccumulator class methodsFor: 'create'!
make: manager {EdgeManager} with: startsInside {BooleanVar}
	| result {Heaper} |
	result := SomeAccumulators fetch.
	result == NULL
		ifTrue: [^ self create: manager with: startsInside]
		ifFalse: [^ (self new.Become: result) create: manager with: startsInside]!
*/
}
public EdgeAccumulator() {
/*

Generated during transformation
*/
}
}
