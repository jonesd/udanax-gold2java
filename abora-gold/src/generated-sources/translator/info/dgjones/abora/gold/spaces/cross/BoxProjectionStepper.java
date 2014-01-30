/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.cross;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.BoxProjectionStepper;
import info.dgjones.abora.gold.spaces.cross.GenericCrossRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Steps over all projections of some boxes. was not.a.type but this prevented compilation
 */
public class BoxProjectionStepper extends Stepper {

	protected GenericCrossRegion myRegion;
	protected int myBoxIndex;
	protected int myBoxLimit;
	protected int myDimension;
	protected static InstanceCache SomeSteppers;
/*
udanax-top.st:53030:
Stepper subclass: #BoxProjectionStepper
	instanceVariableNames: '
		myRegion {GenericCrossRegion}
		myBoxIndex {Int32}
		myBoxLimit {Int32}
		myDimension {Int32}'
	classVariableNames: 'SomeSteppers {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Cross'!
*/
/*
udanax-top.st:53038:
BoxProjectionStepper comment:
'Steps over all projections of some boxes. was not.a.type but this prevented compilation'!
*/
/*
udanax-top.st:53040:
(BoxProjectionStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:53130:
BoxProjectionStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:53133:
(BoxProjectionStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BoxProjectionStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return a new stepper which steps independently of me, but whose current
 * value is the same as mine, and which must produce a future history of values
 * which satisfies the same obligation that my contract obligates me to produce
 * now. Typically, this will mean that he must produce the same future history
 * that I'm going to produce. However, let's say that I am enumerating the
 * elements of a partial order in some full order which is consistent with the
 * partial order. If a copy of me is made after I'm part way through, then me
 * and my copy may produce any future history compatable both with the partial
 * order and the elements I've already produced by the time of the copy. Of
 * course, a subclass or a Stepper creating message (like
 * IntegerRegion::stepper()) may specify the more stringent requirement (that a
 * copy must produce the same sequence).
 * To prevent aliasing, Steppers should typically be passed by copy. See class
 * comment.
 */
public Stepper copy() {
	Someone.shouldImplement();
	return null;
/*
udanax-top.st:53045:BoxProjectionStepper methodsFor: 'create'!
{Stepper} copy
	"Return a new stepper which steps independently of me, but whose current 
	value is the same as mine, and which must produce a future history of values 
	which satisfies the same obligation that my contract obligates me to produce 
	now. Typically, this will mean that he must produce the same future history 
	that I'm going to produce. However, let's say that I am enumerating the 
	elements of a partial order in some full order which is consistent with the 
	partial order. If a copy of me is made after I'm part way through, then me 
	and my copy may produce any future history compatable both with the partial 
	order and the elements I've already produced by the time of the copy. Of 
	course, a subclass or a Stepper creating message (like 
	IntegerRegion::stepper()) may specify the more stringent requirement (that a 
	copy must produce the same sequence). 
	
	To prevent aliasing, Steppers should typically be passed by copy. See class 
	comment."
	Someone shouldImplement.
	^NULL "fodder"!
*/
}
public void destroy() {
	if ( ! (SomeSteppers.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:53065:BoxProjectionStepper methodsFor: 'create'!
{void} destroy
	(SomeSteppers store: self) ifFalse: [super destroy]!
*/
}
public BoxProjectionStepper(GenericCrossRegion region) {
	super();
	myRegion = region;
	myBoxIndex = 0;
	myBoxLimit = region.boxCount();
	myDimension = 0;
/*
udanax-top.st:53070:BoxProjectionStepper methodsFor: 'protected: create'!
create: region {GenericCrossRegion}
	
	super create.
	myRegion := region.
	myBoxIndex := Int32Zero.
	myBoxLimit := region boxCount.
	myDimension := Int32Zero.!
*/
}
public BoxProjectionStepper(GenericCrossRegion region, int boxIndex, int boxLimit) {
	super();
	myRegion = region;
	myBoxIndex = boxIndex;
	myBoxLimit = boxLimit;
	myDimension = 0;
/*
udanax-top.st:53078:BoxProjectionStepper methodsFor: 'protected: create'!
create: region {GenericCrossRegion}
	with: boxIndex {Int32}
	with: boxLimit {Int32}
	
	super create.
	myRegion := region.
	myBoxIndex := boxIndex.
	myBoxLimit := boxLimit.
	myDimension := Int32Zero.!
*/
}
public BoxProjectionStepper(GenericCrossRegion region, int boxIndex, int boxLimit, int dimension) {
	super();
	myRegion = region;
	myBoxIndex = boxIndex;
	myBoxLimit = boxLimit;
	myDimension = dimension;
/*
udanax-top.st:53088:BoxProjectionStepper methodsFor: 'protected: create'!
create: region {GenericCrossRegion}
	with: boxIndex {Int32}
	with: boxLimit {Int32}
	with: dimension {Int32}
	
	super create.
	myRegion := region.
	myBoxIndex := boxIndex.
	myBoxLimit := boxLimit.
	myDimension := dimension.!
*/
}
public Heaper fetch() {
	if ( ! (myBoxIndex < myBoxLimit)) {
		return null;
	}
	return projection();
/*
udanax-top.st:53101:BoxProjectionStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	myBoxIndex < myBoxLimit ifFalse:
		[^NULL].
	^self projection!
*/
}
public boolean hasValue() {
	return myBoxIndex < myBoxLimit;
/*
udanax-top.st:53107:BoxProjectionStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^myBoxIndex < myBoxLimit!
*/
}
public void step() {
	if (myBoxIndex < myBoxLimit) {
		myDimension = myDimension + 1;
		if ( ! (myDimension < myRegion.crossSpace().axisCount())) {
			myBoxIndex = myBoxIndex + 1;
			myDimension = 0;
		}
	}
/*
udanax-top.st:53111:BoxProjectionStepper methodsFor: 'operations'!
{void} step
	myBoxIndex < myBoxLimit ifTrue:
		[myDimension := myDimension + 1.
		myDimension < myRegion crossSpace axisCount ifFalse:
			[myBoxIndex := myBoxIndex + 1.
			myDimension := Int32Zero]].!
*/
}
public int dimension() {
	return myDimension;
/*
udanax-top.st:53121:BoxProjectionStepper methodsFor: 'accessing'!
{Int32} dimension
	^myDimension!
*/
}
public XnRegion projection() {
	return myRegion.boxProjection(myBoxIndex, myDimension);
/*
udanax-top.st:53125:BoxProjectionStepper methodsFor: 'accessing'!
{XnRegion} projection
	^myRegion boxProjection: myBoxIndex with: myDimension!
*/
}
public static BoxProjectionStepper make(GenericCrossRegion region) {
	Heaper result;
	result = SomeSteppers.fetch();
	if (result == null) {
		return new BoxProjectionStepper(region);
	}
	else {
		return 
		/* TODO newBecome */
		new BoxProjectionStepper(region);
	}
/*
udanax-top.st:53138:BoxProjectionStepper class methodsFor: 'create'!
make: region {GenericCrossRegion}
	| result {Heaper} |
	result := SomeSteppers fetch.
	result == NULL
		ifTrue: [^ self create: region]
		ifFalse: [^ (self new.Become: result) create: region]!
*/
}
public static BoxProjectionStepper make(GenericCrossRegion region, int boxIndex, int boxLimit) {
	Heaper result;
	result = SomeSteppers.fetch();
	if (result == null) {
		return new BoxProjectionStepper(region, boxIndex, boxLimit);
	}
	else {
		return 
		/* TODO newBecome */
		new BoxProjectionStepper(region, boxIndex, boxLimit);
	}
/*
udanax-top.st:53145:BoxProjectionStepper class methodsFor: 'create'!
make: region {GenericCrossRegion}
	with: boxIndex {Int32}
	with: boxLimit {Int32}
	| result {Heaper} |
	result := SomeSteppers fetch.
	result == NULL
		ifTrue: [^ self create: region with: boxIndex with: boxLimit]
		ifFalse: [^ (self new.Become: result) create: region with: boxIndex with: boxLimit]!
*/
}
public static void initTimeNonInherited() {
	SomeSteppers = InstanceCache.make(8);
/*
udanax-top.st:53156:BoxProjectionStepper class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeSteppers := InstanceCache make: 8!
*/
}
public static void linkTimeNonInherited() {
	SomeSteppers = null;
/*
udanax-top.st:53159:BoxProjectionStepper class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeSteppers := NULL!
*/
}
public BoxProjectionStepper() {
/*

Generated during transformation
*/
}
public BoxProjectionStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
