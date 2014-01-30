/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cross;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.cross.GenericCrossSimpleRegionStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class GenericCrossSimpleRegionStepper extends Stepper {

	protected CrossSpace mySpace;
	protected Stepper myBoxes;
	protected PtrArray mySimples;
/*
udanax-top.st:53775:
Stepper subclass: #GenericCrossSimpleRegionStepper
	instanceVariableNames: '
		mySpace {CrossSpace}
		myBoxes {Stepper of: CrossRegion}
		mySimples {PtrArray of: (Stepper of: XnRegion)}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cross'!
*/
/*
udanax-top.st:53782:
(GenericCrossSimpleRegionStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:53863:
GenericCrossSimpleRegionStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:53866:
(GenericCrossSimpleRegionStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GenericCrossSimpleRegionStepper.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	PtrArray result;
	if ( ! (hasValue())) {
		return null;
	}
	result = PtrArray.nulls(mySpace.axisCount());
	for (int i = 0; i < mySpace.axisCount(); i ++ ) {
		result.store(i, ((Stepper) (mySimples.get(i))).get());
	}
	return mySpace.crossOfRegions(result);
/*
udanax-top.st:53787:GenericCrossSimpleRegionStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	| result {PtrArray} |
	self hasValue ifFalse:
		[^NULL].
	result := PtrArray nulls: mySpace axisCount.
	Int32Zero almostTo: mySpace axisCount do: [ :i {Int32} |
		result at: i store: ((mySimples get: i) cast: Stepper) get].
	^mySpace crossOfRegions: result!
*/
}
public boolean hasValue() {
	return myBoxes.hasValue();
/*
udanax-top.st:53797:GenericCrossSimpleRegionStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^myBoxes hasValue!
*/
}
public void step() {
	int index;
	if (myBoxes.hasValue()) {
		index = mySpace.axisCount() - 1;
		while (index >= 0) {
			Stepper sub;
			sub = (Stepper) (mySimples.get(index));
			sub.step();
			if (sub.hasValue()) {
				replenishSteppers(index + 1);
				return ;
			}
			index = index - 1;
		}
		myBoxes.step();
		if (myBoxes.hasValue()) {
			replenishSteppers(0);
		}
	}
/*
udanax-top.st:53801:GenericCrossSimpleRegionStepper methodsFor: 'operations'!
{void} step
	| index {Int32} |
	myBoxes hasValue ifTrue: [
		index := mySpace axisCount - 1.
		[index >= Int32Zero] whileTrue:
			[ | sub {Stepper} |
			sub := (mySimples get: index) cast: Stepper.
			sub step.
			sub hasValue ifTrue:
				[self replenishSteppers: index + 1.
				^VOID].
			index := index - 1].
		myBoxes step.
		myBoxes hasValue ifTrue:
			[self replenishSteppers: Int32Zero]]!
*/
}
/**
 * Replenish all steppers starting at index
 */
public void replenishSteppers(int index) {
	CrossRegion box;
	box = (CrossRegion) myBoxes.get();
	for (int i = index; i < mySpace.axisCount(); i ++ ) {
		mySimples.store(i, (box.projection(i)).simpleRegions());
	}
/*
udanax-top.st:53820:GenericCrossSimpleRegionStepper methodsFor: 'private:'!
{void} replenishSteppers: index {Int32}
	"Replenish all steppers starting at index"
	
	| box {CrossRegion} |
	box := myBoxes get cast: CrossRegion.
	index almostTo: mySpace axisCount do: [ :i {Int32} |
		mySimples at: i store: (box projection: i) simpleRegions]!
*/
}
public Stepper copy() {
	PtrArray simples;
	simples = PtrArray.nulls(mySimples.count());
	for (int i = 0; i < simples.count(); i ++ ) {
		simples.store(i, ((Stepper) (mySimples.get(i))).copy());
	}
	return new GenericCrossSimpleRegionStepper(mySpace, myBoxes.copy(), simples);
/*
udanax-top.st:53830:GenericCrossSimpleRegionStepper methodsFor: 'create'!
{Stepper} copy
	| simples {PtrArray} |
	simples := PtrArray nulls: mySimples count.
	Int32Zero almostTo: simples count do: [ :i {Int32} |
		simples at: i store: ((mySimples get: i) cast: Stepper) copy].
	^GenericCrossSimpleRegionStepper create: mySpace with: myBoxes copy with: simples!
*/
}
public GenericCrossSimpleRegionStepper(CrossSpace space, Stepper boxes) {
	super();
	mySpace = space;
	myBoxes = boxes;
	if (boxes.hasValue()) {
		mySimples = PtrArray.nulls(space.axisCount());
		replenishSteppers(0);
	}
/*
udanax-top.st:53840:GenericCrossSimpleRegionStepper methodsFor: 'protected: create'!
create: space {CrossSpace} with: boxes {Stepper}
	super create.
	mySpace := space.
	myBoxes := boxes.
	boxes hasValue ifTrue:
		[mySimples := PtrArray nulls: space axisCount.
		self replenishSteppers: Int32Zero]!
*/
}
public GenericCrossSimpleRegionStepper(CrossSpace space, Stepper boxes, PtrArray simples) {
	super();
	mySpace = space;
	myBoxes = boxes;
	mySimples = simples;
/*
udanax-top.st:53849:GenericCrossSimpleRegionStepper methodsFor: 'protected: create'!
create: space {CrossSpace} with: boxes {Stepper} with: simples {PtrArray}
	super create.
	mySpace := space.
	myBoxes := boxes.
	mySimples := simples.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:53858:GenericCrossSimpleRegionStepper methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:53860:GenericCrossSimpleRegionStepper methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static Stepper make(CrossSpace space, Stepper boxes) {
	return new GenericCrossSimpleRegionStepper(space, boxes);
/*
udanax-top.st:53871:GenericCrossSimpleRegionStepper class methodsFor: 'create'!
{Stepper} make: space {CrossSpace} with: boxes {Stepper}
	^self create: space with: boxes!
*/
}
public GenericCrossSimpleRegionStepper() {
/*

Generated during transformation
*/
}
public GenericCrossSimpleRegionStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
