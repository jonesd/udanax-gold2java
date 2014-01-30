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
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.cross.BoxAccumulator;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.ActualTuple;
import info.dgjones.abora.gold.spaces.cross.BoxProjectionStepper;
import info.dgjones.abora.gold.spaces.cross.BoxStepper;
import info.dgjones.abora.gold.spaces.cross.GenericCrossRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Steps over all boxes. was NOT.A.TYPE but this prevented compilation
 */
public class BoxStepper extends Stepper {

	protected GenericCrossRegion myRegion;
	protected int myIndex;
	protected XnRegion myValue;
	protected static InstanceCache SomeSteppers;
/*
udanax-top.st:53162:
Stepper subclass: #BoxStepper
	instanceVariableNames: '
		myRegion {GenericCrossRegion}
		myIndex {Int32}
		myValue {XnRegion | NULL}'
	classVariableNames: 'SomeSteppers {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Cross'!
*/
/*
udanax-top.st:53169:
BoxStepper comment:
'Steps over all boxes. was NOT.A.TYPE but this prevented compilation'!
*/
/*
udanax-top.st:53171:
(BoxStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:53389:
BoxStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:53392:
(BoxStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BoxStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	if (myIndex >= myRegion.boxCount()) {
		return null;
	}
	if (myValue == null) {
		myValue = GenericCrossRegion.make(myRegion.crossSpace(), 1, ((PtrArray) (myRegion.secretRegions().copy(myRegion.crossSpace().axisCount(), myIndex * myRegion.crossSpace().axisCount()))));
	}
	return myValue;
/*
udanax-top.st:53176:BoxStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	myIndex >= myRegion boxCount
		ifTrue: [^NULL].
	myValue == NULL ifTrue:
		[myValue := GenericCrossRegion
						make: myRegion crossSpace
						with: 1
						with: ((myRegion secretRegions
											copy: myRegion crossSpace axisCount
											with: myIndex * myRegion crossSpace axisCount) cast: PtrArray)].
	^myValue!
*/
}
public boolean hasValue() {
	return myIndex < myRegion.boxCount();
/*
udanax-top.st:53189:BoxStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^myIndex < myRegion boxCount!
*/
}
public void step() {
	if (myIndex < myRegion.boxCount()) {
		myIndex = myIndex + 1;
		myValue = null;
	}
/*
udanax-top.st:53193:BoxStepper methodsFor: 'operations'!
{void} step
	myIndex < myRegion boxCount ifTrue:
		[myIndex := myIndex + 1.
		myValue := NULL]!
*/
}
public Stepper copy() {
	Heaper result;
	result = SomeSteppers.fetch();
	if (result == null) {
		return new BoxStepper(myRegion, myIndex, myValue);
	}
	else {
		return 
		/* TODO newBecome */
		new BoxStepper(myRegion, myIndex, myValue);
	}
/*
udanax-top.st:53201:BoxStepper methodsFor: 'create'!
{Stepper} copy
	| result {Heaper} |
	result := SomeSteppers fetch.
	result == NULL
		ifTrue: [^BoxStepper create: myRegion with: myIndex with: myValue]
		ifFalse: [^(BoxStepper new.Become: result) create: myRegion with: myIndex with: myValue]!
*/
}
public void destroy() {
	if ( ! (SomeSteppers.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:53208:BoxStepper methodsFor: 'create'!
{void} destroy
	(SomeSteppers store: self) ifFalse: [super destroy]!
*/
}
public BoxStepper(GenericCrossRegion region) {
	super();
	myRegion = region;
	myIndex = 0;
	myValue = null;
/*
udanax-top.st:53213:BoxStepper methodsFor: 'protected: create'!
create: region {GenericCrossRegion}
	
	super create.
	myRegion := region.
	myIndex := Int32Zero.
	myValue := NULL.!
*/
}
public BoxStepper(GenericCrossRegion region, int index, XnRegion value) {
	super();
	myRegion = region;
	myIndex = index;
	myValue = value;
/*
udanax-top.st:53220:BoxStepper methodsFor: 'protected: create'!
create: region {GenericCrossRegion} with: index {Int32} with: value {XnRegion | NULL}
	
	super create.
	myRegion := region.
	myIndex := index.
	myValue := value.!
*/
}
/**
 * The complement of this box
 */
public GenericCrossRegion boxComplement() {
	BoxAccumulator result;
	PtrArray extrusion;
	result = BoxAccumulator.make(myRegion.crossSpace(), myRegion.crossSpace().axisCount());
	for (int dimension = 0; dimension < myRegion.crossSpace().axisCount(); dimension ++ ) {
		XnRegion special;
		extrusion = PtrArray.nulls(myRegion.crossSpace().axisCount());
		for (int i = 0; i < dimension; i ++ ) {
			extrusion.store(i, (projection(i)));
		}
		special = (projection(dimension)).complement();
		if ( ! (special.isEmpty())) {
			extrusion.store(dimension, special);
			for (int i1 = dimension + 1; i1 < myRegion.crossSpace().axisCount(); i1 ++ ) {
				extrusion.store(i1, (myRegion.crossSpace().axis(i1)).fullRegion());
			}
			result.addProjections(extrusion, 0);
		}
	}
	return (GenericCrossRegion) result.region();
/*
udanax-top.st:53229:BoxStepper methodsFor: 'accessing'!
{GenericCrossRegion} boxComplement
	"The complement of this box"
	
	| result {BoxAccumulator} extrusion {PtrArray of: XnRegion} |
	result := BoxAccumulator make: myRegion crossSpace
		with: myRegion crossSpace axisCount.
	Int32Zero almostTo: myRegion crossSpace axisCount do:
		[ :dimension {Int32} | | special {XnRegion} |
		extrusion := PtrArray nulls: myRegion crossSpace axisCount.
		Int32Zero almostTo: dimension do: [ :i {Int32} |
			extrusion at: i store: (self projection: i)].
		special := (self projection: dimension) complement.
		special isEmpty ifFalse:
			[extrusion at: dimension store: special.
			dimension + 1 almostTo: myRegion crossSpace axisCount do: [ :i {Int32} |
				extrusion at: i store: (myRegion crossSpace axis: i) fullRegion].
			result addProjections: extrusion with: Int32Zero]].
	^result region cast: GenericCrossRegion!
*/
}
/**
 * The complement of this box
 */
public BoxAccumulator boxComplementAccumulator() {
	BoxAccumulator result;
	PtrArray extrusion;
	result = BoxAccumulator.make(myRegion.crossSpace(), myRegion.crossSpace().axisCount());
	for (int dimension = 0; dimension < myRegion.crossSpace().axisCount(); dimension ++ ) {
		extrusion = PtrArray.nulls(myRegion.crossSpace().axisCount());
		for (int i = 0; i < dimension; i ++ ) {
			extrusion.store(i, (projection(i)));
		}
		extrusion.store(dimension, (projection(dimension)).complement());
		for (int i1 = dimension + 1; i1 < myRegion.crossSpace().axisCount(); i1 ++ ) {
			extrusion.store(i1, (myRegion.crossSpace().axis(i1)).fullRegion());
		}
		result.addProjections(extrusion, 0);
	}
	return result;
/*
udanax-top.st:53248:BoxStepper methodsFor: 'accessing'!
{BoxAccumulator} boxComplementAccumulator
	"The complement of this box"
	
	| result {BoxAccumulator} extrusion {PtrArray of: XnRegion} |
	result := BoxAccumulator make: myRegion crossSpace
		with: myRegion crossSpace axisCount.
	Int32Zero almostTo: myRegion crossSpace axisCount do: [ :dimension {Int32} |
		extrusion := PtrArray nulls: myRegion crossSpace axisCount.
		Int32Zero almostTo: dimension do: [ :i {Int32} |
			extrusion at: i store: (self projection: i)].
		extrusion at: dimension store: (self projection: dimension) complement.
		dimension + 1 almostTo: myRegion crossSpace axisCount do: [ :i {Int32} |
			extrusion at: i store: (myRegion crossSpace axis: i) fullRegion].
		result addProjections: extrusion with: Int32Zero].
	^result!
*/
}
public int boxHash() {
	int result;
	result = 0;
	Stepper stomper = projectionStepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion sub = (XnRegion) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		result = result ^ sub.hashForEqual();
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:53264:BoxStepper methodsFor: 'accessing'!
{UInt32} boxHash
	| result {UInt32} |
	result := UInt32Zero.
	self projectionStepper forEach: [ :sub {XnRegion} |
		result := result bitXor: sub hashForEqual].
	^result!
*/
}
/**
 * Whether my current box contains a position
 */
public boolean boxHasMember(ActualTuple tuple) {
	BoxProjectionStepper mine;
	mine = projectionStepper();
	while (mine.hasValue()) {
		if ( ! (mine.projection().hasMember((tuple.positionAt(mine.dimension()))))) {
			return false;
		}
		mine.step();
	}
	mine.destroy();
	return true;
/*
udanax-top.st:53272:BoxStepper methodsFor: 'accessing'!
{BooleanVar} boxHasMember: tuple {ActualTuple}
	"Whether my current box contains a position"
	
	| mine {BoxProjectionStepper} |
	mine := self projectionStepper.
	[mine hasValue] whileTrue:
		[(mine projection hasMember: (tuple positionAt: mine dimension)) ifFalse:
			[^false].
		mine step].
	mine destroy.
	^true!
*/
}
public int boxIndex() {
	return myIndex;
/*
udanax-top.st:53284:BoxStepper methodsFor: 'accessing'!
{Int32} boxIndex
	^myIndex!
*/
}
/**
 * Whether my current box intersects others current box
 */
public boolean boxIntersects(BoxStepper other) {
	BoxProjectionStepper mine;
	BoxProjectionStepper others;
	mine = projectionStepper();
	others = other.projectionStepper();
	while (mine.hasValue()) {
		if ( ! (mine.projection().intersects(others.projection()))) {
			return false;
		}
		mine.step();
		others.step();
	}
	mine.destroy();
	others.destroy();
	return true;
/*
udanax-top.st:53288:BoxStepper methodsFor: 'accessing'!
{BooleanVar} boxIntersects: other {BoxStepper}
	"Whether my current box intersects others current box"
	
	| mine {BoxProjectionStepper} others {BoxProjectionStepper} |
	mine := self projectionStepper.
	others := other projectionStepper.
	[mine hasValue] whileTrue:
		[(mine projection intersects: others projection) ifFalse:
			[^false].
		mine step.
		others step].
	mine destroy.
	others destroy.
	^true!
*/
}
/**
 * Whether my current box isEqual others current box
 */
public boolean boxIsEqual(BoxStepper other) {
	BoxProjectionStepper mine;
	BoxProjectionStepper others;
	mine = projectionStepper();
	others = other.projectionStepper();
	while (mine.hasValue()) {
		if ( ! (mine.projection().isEqual(others.projection()))) {
			return false;
		}
		mine.step();
		others.step();
	}
	mine.destroy();
	others.destroy();
	return true;
/*
udanax-top.st:53303:BoxStepper methodsFor: 'accessing'!
{BooleanVar} boxIsEqual: other {BoxStepper}
	"Whether my current box isEqual others current box"
	
	| mine {BoxProjectionStepper} others {BoxProjectionStepper} |
	mine := self projectionStepper.
	others := other projectionStepper.
	[mine hasValue] whileTrue:
		[(mine projection isEqual: others projection) ifFalse:
			[^false].
		mine step.
		others step].
	mine destroy.
	others destroy.
	^true!
*/
}
/**
 * Whether my current box isSubsetOf others current box
 */
public boolean boxIsSubsetOf(BoxStepper other) {
	BoxProjectionStepper mine;
	BoxProjectionStepper others;
	mine = projectionStepper();
	others = other.projectionStepper();
	while (mine.hasValue()) {
		if ( ! (mine.projection().isSubsetOf(others.projection()))) {
			return false;
		}
		mine.step();
		others.step();
	}
	mine.destroy();
	others.destroy();
	return true;
/*
udanax-top.st:53318:BoxStepper methodsFor: 'accessing'!
{BooleanVar} boxIsSubsetOf: other {BoxStepper}
	"Whether my current box isSubsetOf others current box"
	
	| mine {BoxProjectionStepper} others {BoxProjectionStepper} |
	mine := self projectionStepper.
	others := other projectionStepper.
	[mine hasValue] whileTrue:
		[(mine projection isSubsetOf: others projection) ifFalse:
			[^false].
		mine step.
		others step].
	mine destroy.
	others destroy.
	^true!
*/
}
/**
 * Intersect each projection in the box into the array. Return false if the result is empty,
 * stopping at the first dimension for which the intersection is empty.
 */
public boolean intersectBoxInto(PtrArray result, int boxIndex) {
	BoxProjectionStepper mine;
	XnRegion proj;
	int base;
	base = myRegion.crossSpace().axisCount() * boxIndex;
	mine = projectionStepper();
	while (mine.hasValue()) {
		result.store(base + mine.dimension(), (proj = ((XnRegion) (result.fetch(base + mine.dimension()))).intersect(mine.projection())));
		if (proj.isEmpty()) {
			return false;
		}
		mine.step();
	}
	mine.destroy();
	return true;
/*
udanax-top.st:53333:BoxStepper methodsFor: 'accessing'!
{BooleanVar} intersectBoxInto: result {PtrArray of: XnRegion} with: boxIndex {Int32}
	"Intersect each projection in the box into the array. Return false if the result is empty, stopping at the first dimension for which the intersection is empty."
	
	| mine {BoxProjectionStepper} proj {XnRegion} base {Int32} |
	base := myRegion crossSpace axisCount * boxIndex.
	mine := self projectionStepper.
	[mine hasValue] whileTrue:
		[result at: base + mine dimension
			store: (proj := ((result fetch: base + mine dimension) cast: XnRegion)
				intersect: mine projection).
		proj isEmpty ifTrue: [^false].
		mine step].
	mine destroy.
	^true!
*/
}
/**
 * Whether my box is also a box in the other region
 */
public boolean isBoxOf(GenericCrossRegion other) {
	BoxStepper others;
	others = other.boxStepper();
	while (others.hasValue()) {
		if (boxIsEqual(others)) {
			return true;
		}
		others.step();
	}
	return false;
/*
udanax-top.st:53348:BoxStepper methodsFor: 'accessing'!
{BooleanVar} isBoxOf: other {GenericCrossRegion}
	"Whether my box is also a box in the other region"
	
	| others {BoxStepper} |
	others := other boxStepper.
	[others hasValue] whileTrue:
		[(self boxIsEqual: others)
			ifTrue: [^true].
		others step].
	^false!
*/
}
/**
 * The projection of my current box into one dimension
 */
public XnRegion projection(int dimension) {
	return myRegion.boxProjection(myIndex, dimension);
/*
udanax-top.st:53359:BoxStepper methodsFor: 'accessing'!
{XnRegion} projection: dimension {Int32}
	"The projection of my current box into one dimension"
	
	^myRegion boxProjection: myIndex with: dimension!
*/
}
/**
 * A stepper over all the projections in the current box
 */
public BoxProjectionStepper projectionStepper() {
	return BoxProjectionStepper.make(myRegion, myIndex, myIndex + 1);
/*
udanax-top.st:53364:BoxStepper methodsFor: 'accessing'!
{BoxProjectionStepper} projectionStepper
	"A stepper over all the projections in the current box"
	
	^BoxProjectionStepper make: myRegion
		with: myIndex
		with: myIndex + 1!
*/
}
public GenericCrossRegion region() {
	return myRegion;
/*
udanax-top.st:53371:BoxStepper methodsFor: 'accessing'!
{GenericCrossRegion} region
	^myRegion!
*/
}
/**
 * Union each projection in the box into the array
 */
public void unionBoxInto(PtrArray result, int boxIndex) {
	BoxProjectionStepper mine;
	int base;
	base = myRegion.crossSpace().axisCount() * boxIndex;
	mine = projectionStepper();
	while (mine.hasValue()) {
		result.store(base + mine.dimension(), (((XnRegion) (result.fetch(base + mine.dimension()))).unionWith(mine.projection())));
		mine.step();
	}
	mine.destroy();
/*
udanax-top.st:53375:BoxStepper methodsFor: 'accessing'!
{void} unionBoxInto: result {PtrArray of: XnRegion} with: boxIndex {Int32}
	"Union each projection in the box into the array"
	
	| mine {BoxProjectionStepper} base {Int32} |
	base := myRegion crossSpace axisCount * boxIndex.
	mine := self projectionStepper.
	[mine hasValue] whileTrue:
		[result at: base + mine dimension
			store: (((result fetch: base + mine dimension) cast: XnRegion)
				unionWith: mine projection).
		mine step].
	mine destroy.!
*/
}
public static void initTimeNonInherited() {
	SomeSteppers = InstanceCache.make(8);
/*
udanax-top.st:53397:BoxStepper class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeSteppers := InstanceCache make: 8!
*/
}
public static void linkTimeNonInherited() {
	SomeSteppers = null;
/*
udanax-top.st:53400:BoxStepper class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeSteppers := NULL!
*/
}
public static BoxStepper make(GenericCrossRegion region) {
	Heaper result;
	result = SomeSteppers.fetch();
	if (result == null) {
		return new BoxStepper(region);
	}
	else {
		return 
		/* TODO newBecome */
		new BoxStepper(region);
	}
/*
udanax-top.st:53405:BoxStepper class methodsFor: 'create'!
make: region {GenericCrossRegion}
	| result {Heaper} |
	result := SomeSteppers fetch.
	result == NULL
		ifTrue: [^ self create: region]
		ifFalse: [^ (self new.Become: result) create: region]!
*/
}
public BoxStepper() {
/*

Generated during transformation
*/
}
public BoxStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
