/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cross;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Accumulator;
import info.dgjones.abora.gold.cross.BoxAccumulator;
import info.dgjones.abora.gold.cross.GenericCrossDsp;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.BoxStepper;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.cross.GenericCrossRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * was NOT.A.TYPE but this prevented compilation
 */
public class BoxAccumulator extends Accumulator {

	protected CrossSpace mySpace;
	protected PtrArray myRegions;
	protected int myIndex;
	protected static InstanceCache SomeAccumulators;
/*
udanax-top.st:11626:
Accumulator subclass: #BoxAccumulator
	instanceVariableNames: '
		mySpace {CrossSpace}
		myRegions {PtrArray of: XnRegion}
		myIndex {Int32}'
	classVariableNames: 'SomeAccumulators {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-cross'!
*/
/*
udanax-top.st:11633:
BoxAccumulator comment:
'was NOT.A.TYPE but this prevented compilation '!
*/
/*
udanax-top.st:11635:
(BoxAccumulator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:11930:
BoxAccumulator class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:11933:
(BoxAccumulator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BoxAccumulator.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Accumulator copy() {
	Heaper result;
	result = SomeAccumulators.fetch();
	if (result == null) {
		return new BoxAccumulator(mySpace, ((PtrArray) myRegions.copy()), myIndex);
	}
	else {
		return 
		/* TODO newBecome */
		new BoxAccumulator(mySpace, ((PtrArray) myRegions.copy()), myIndex);
	}
/*
udanax-top.st:11640:BoxAccumulator methodsFor: 'creation'!
{Accumulator} copy
	| result {Heaper} |
	result := SomeAccumulators fetch.
	result == NULL
		ifTrue: [^BoxAccumulator create: mySpace with: (myRegions copy cast: PtrArray) with: myIndex]
		ifFalse: [^(BoxAccumulator new.Become: result) create: mySpace with: (myRegions copy cast: PtrArray) with: myIndex]!
*/
}
public BoxAccumulator(GenericCrossRegion region) {
	super();
	mySpace = region.crossSpace();
	myRegions = (PtrArray) region.secretRegions().copy();
	myIndex = region.boxCount();
/*
udanax-top.st:11649:BoxAccumulator methodsFor: 'protected: creation'!
create: region {GenericCrossRegion}
	super create.
	mySpace := region crossSpace.
	myRegions := region secretRegions copy cast: PtrArray.
	myIndex := region boxCount.!
*/
}
public BoxAccumulator(CrossSpace space, int expectedBoxCount) {
	super();
	mySpace = space;
	myRegions = PtrArray.nulls(space.axisCount() * expectedBoxCount);
	myIndex = 0;
/*
udanax-top.st:11656:BoxAccumulator methodsFor: 'protected: creation'!
create: space {CrossSpace} with: expectedBoxCount {Int32}
	super create.
	mySpace := space.
	myRegions := PtrArray nulls: space axisCount * expectedBoxCount.
	myIndex := Int32Zero.!
*/
}
public BoxAccumulator(CrossSpace space, PtrArray regions, int expectedBoxCount) {
	super();
	mySpace = space;
	myRegions = PtrArray.nulls(space.axisCount() * expectedBoxCount);
	myIndex = 0;
	Ravi.shouldImplement();
	/* shouldn't we be doing something with the 'regios' argument? */
/*
udanax-top.st:11663:BoxAccumulator methodsFor: 'protected: creation'!
create: space {CrossSpace}
	with: regions {PtrArray unused of: XnRegion}
	with: expectedBoxCount {Int32}
	super create.
	mySpace := space.
	myRegions := PtrArray nulls: space axisCount * expectedBoxCount.
	myIndex := Int32Zero.
	Ravi shouldImplement. "shouldn't we be doing something with the 'regios' argument?"!
*/
}
/**
 * Make sure there is room to add a box
 */
public void aboutToAdd() {
	if ( ! (myIndex * mySpace.axisCount() < myRegions.count())) {
		myRegions = (PtrArray) (myRegions.copyGrow((myIndex + 1) * mySpace.axisCount()));
	}
/*
udanax-top.st:11675:BoxAccumulator methodsFor: 'private:'!
{void} aboutToAdd
	"Make sure there is room to add a box"
	
	myIndex * mySpace axisCount < myRegions count ifFalse:
		[myRegions := (myRegions copyGrow: (myIndex + 1) * mySpace axisCount) cast: PtrArray].!
*/
}
/**
 * Add a new box which is just like a current one except for the projection on one dimension.
 * Return its index
 */
public int addSubstitutedBox(int current, int dimension, XnRegion newRegion) {
	aboutToAdd();
	myRegions.storeMany(myIndex * mySpace.axisCount(), myRegions, mySpace.axisCount(), current * mySpace.axisCount());
	myRegions.store(myIndex * mySpace.axisCount() + dimension, newRegion);
	myIndex = myIndex + 1;
	return myIndex - 1;
/*
udanax-top.st:11681:BoxAccumulator methodsFor: 'private:'!
{Int32} addSubstitutedBox: current {Int32} with: dimension {Int32}
	with: newRegion {XnRegion}
	"Add a new box which is just like a current one except for the projection on one dimension. Return its index"
	
	self aboutToAdd.
	myRegions at: myIndex * mySpace axisCount
		storeMany: myRegions
		with: mySpace axisCount
		with: current * mySpace axisCount.
	myRegions at: myIndex * mySpace axisCount + dimension
		store: newRegion.
	myIndex := myIndex + 1.
	^myIndex - 1!
*/
}
public int boxCount() {
	Someone.knownBug();
	/* includes deleted boxes */
	return myIndex;
/*
udanax-top.st:11695:BoxAccumulator methodsFor: 'private:'!
{Int32} boxCount
	self knownBug. "includes deleted boxes"
	^myIndex!
*/
}
/**
 * Change a projection of a box
 */
public XnRegion boxProjection(int box, int dimension) {
	return (XnRegion) (myRegions.fetch(box * mySpace.axisCount() + dimension));
/*
udanax-top.st:11700:BoxAccumulator methodsFor: 'private:'!
{XnRegion} boxProjection: box {Int32} with: dimension {Int32}
	"Change a projection of a box"
	
	^(myRegions fetch: box * mySpace axisCount + dimension) cast: XnRegion!
*/
}
/**
 * Mark a box as deleted
 */
public void deleteBox(int box) {
	myRegions.store(box * mySpace.axisCount(), null);
/*
udanax-top.st:11705:BoxAccumulator methodsFor: 'private:'!
{void} deleteBox: box {Int32}
	"Mark a box as deleted"
	
	myRegions at: box * mySpace axisCount store: NULL!
*/
}
/**
 * Take my box at added
 * and distribute it over my existing boxes from start to stop - 1
 * meanwhile taking pieces out of my box at remainder
 * and delete it if it becomes empty
 * Return true if there is still something left in the remainder
 */
public boolean distributeUnion(int added, int start, int stop) {
	for (int index = start; index < stop; index ++ ) {
		if ( ! (splitUnion(added, index, stop))) {
			return false;
		}
	}
	return true;
/*
udanax-top.st:11710:BoxAccumulator methodsFor: 'private:'!
{BooleanVar} distributeUnion: added {Int32}
	with: start {Int32}
	with: stop {Int32}
	
	"Take my box at added
		and distribute it over my existing boxes from start to stop - 1
		meanwhile taking pieces out of my box at remainder
			and delete it if it becomes empty
	Return true if there is still something left in the remainder"
	
	start almostTo: stop do: [ :index {Int32} |
		(self splitUnion: added with: index with: stop)
			ifFalse: [^false]].
	^true!
*/
}
public int index() {
	return myIndex;
/*
udanax-top.st:11725:BoxAccumulator methodsFor: 'private:'!
{Int32} index
	^myIndex!
*/
}
/**
 * Whether the box has been deleted
 */
public boolean isDeleted(int box) {
	return (myRegions.fetch(box * mySpace.axisCount())) == null;
/*
udanax-top.st:11729:BoxAccumulator methodsFor: 'private:'!
{BooleanVar} isDeleted: box {Int32}
	"Whether the box has been deleted"
	
	^(myRegions fetch: box * mySpace axisCount) == NULL!
*/
}
public PtrArray secretRegions() {
	return myRegions;
/*
udanax-top.st:11734:BoxAccumulator methodsFor: 'private:'!
{PtrArray of: XnRegion} secretRegions
	^myRegions!
*/
}
/**
 * Take my box at added
 * and union it with my box at current
 * delete it if it becomes empty
 * Return true if there is still something left in the added box
 */
public boolean splitUnion(int added, int current, int stop) {
	int dimension;
	XnRegion addedRegion;
	XnRegion currentRegion;
	XnRegion common;
	int newAdded;
	XnRegion extraCurrent;
	XnRegion extraAdded;
	if (isDeleted(current)) {
		return true;
	}
	dimension = 0;
	while (dimension + 1 < mySpace.axisCount()) {
		/* see if the added intersects the current in this dimension */
		addedRegion = boxProjection(added, dimension);
		currentRegion = boxProjection(current, dimension);
		Someone.thingToDo();
		/* Add protocol for tri-delta: gives triple (a-b, a&b, b-a) */
		common = addedRegion.intersect(currentRegion);
		if (common.isEmpty()) {
			return true;
		}
		/* split out the part of current that doesn't intersect */
		extraCurrent = currentRegion.minus(common);
		if ( ! (extraCurrent.isEmpty())) {
			addSubstitutedBox(current, dimension, extraCurrent);
			storeBoxProjection(current, dimension, common);
		}
		/* split out the part of the added that doesn't intersect */
		extraAdded = addedRegion.minus(common);
		if ( ! (extraAdded.isEmpty())) {
			newAdded = addSubstitutedBox(added, dimension, extraAdded);
			distributeUnion(newAdded, current + 1, stop);
			storeBoxProjection(added, dimension, common);
		}
		dimension = dimension + 1;
	}
	/* union the added into the last dimension of the current box */
	addedRegion = boxProjection(added, dimension);
	currentRegion = boxProjection(current, dimension);
	storeBoxProjection(current, dimension, (currentRegion.unionWith(addedRegion)));
	deleteBox(added);
	return false;
/*
udanax-top.st:11738:BoxAccumulator methodsFor: 'private:'!
{BooleanVar} splitUnion: added {Int32}
	with: current {Int32}
	with: stop {Int32}
	
	"Take my box at added
		and union it with my box at current
		delete it if it becomes empty
	Return true if there is still something left in the added box"
	
	| dimension {Int32} addedRegion {XnRegion} currentRegion {XnRegion} common {XnRegion}
	  newAdded {Int32} extraCurrent {XnRegion} extraAdded {XnRegion} |
	
	(self isDeleted: current) ifTrue: [^true].
	dimension := Int32Zero.
	[dimension + 1 < mySpace axisCount] whileTrue:
		["see if the added intersects the current in this dimension"
		addedRegion := self boxProjection: added with: dimension.
		currentRegion := self boxProjection: current with: dimension.
		self thingToDo. "Add protocol for tri-delta: gives triple (a-b, a&b, b-a)"
		common := addedRegion intersect: currentRegion.
		common isEmpty ifTrue:
			[^true].
		
		"split out the part of current that doesn't intersect"
		extraCurrent := currentRegion minus: common.
		extraCurrent isEmpty ifFalse:
			[self addSubstitutedBox: current
				with: dimension with: extraCurrent.
			self storeBoxProjection: current with: dimension with: common].
		
		"split out the part of the added that doesn't intersect"
		extraAdded := addedRegion minus: common.
		extraAdded isEmpty ifFalse:
			[newAdded := self addSubstitutedBox: added
				with: dimension with: extraAdded.
			self distributeUnion: newAdded with: current + 1 with: stop.
			self storeBoxProjection: added with: dimension with: common].
		
		dimension := dimension + 1].
	
	"union the added into the last dimension of the current box"
	addedRegion := self boxProjection: added with: dimension.
	currentRegion := self boxProjection: current with: dimension.
	self storeBoxProjection: current with: dimension with: (currentRegion unionWith: addedRegion).
	self deleteBox: added.
	^false!
*/
}
/**
 * Change a projection of a box
 */
public void storeBoxProjection(int box, int dimension, XnRegion region) {
	myRegions.store(box * mySpace.axisCount() + dimension, region);
/*
udanax-top.st:11785:BoxAccumulator methodsFor: 'private:'!
{void} storeBoxProjection: box {Int32} with: dimension {Int32} with: region {XnRegion}
	"Change a projection of a box"
	
	myRegions at: box * mySpace axisCount + dimension store: region!
*/
}
/**
 * If two boxes differ by only one projection, union the second into the first and delete the
 * second
 */
public void tryMergeBoxes(int i, int j) {
	int unequal;
	unequal = -1;
	for (int dim = 0; dim < mySpace.axisCount(); dim ++ ) {
		if ( ! ((boxProjection(i, dim)).isEqual((boxProjection(j, dim))))) {
			if (unequal >= 0) {
				return ;
			}
			unequal = dim;
		}
	}
	storeBoxProjection(i, unequal, ((boxProjection(i, unequal)).unionWith((boxProjection(j, unequal)))));
	deleteBox(j);
/*
udanax-top.st:11790:BoxAccumulator methodsFor: 'private:'!
{void} tryMergeBoxes: i {Int32} with: j {Int32}
	"If two boxes differ by only one projection, union the second into the first and delete the second"
	
	| unequal {Int32} |
	unequal := -1.
	Int32Zero almostTo: mySpace axisCount do: [ :dim {Int32} |
		((self boxProjection: i with: dim) isEqual: (self boxProjection: j with: dim))
			ifFalse:
				[unequal >= Int32Zero
					ifTrue: [^VOID].
				unequal := dim]].
	self storeBoxProjection: i with: unequal
		with: ((self boxProjection: i with: unequal)
			unionWith: (self boxProjection: j with: unequal)).
	self deleteBox: j.!
*/
}
/**
 * Add in all the boxes in another accumulator
 */
public void addAccumulatedBoxes(BoxAccumulator other) {
	for (int box = 0; box < other.index(); box ++ ) {
		if ( ! (other.isDeleted(box))) {
			aboutToAdd();
			myRegions.storeMany(myIndex * mySpace.axisCount(), other.secretRegions(), mySpace.axisCount(), box * mySpace.axisCount());
			myIndex = myIndex + 1;
		}
	}
/*
udanax-top.st:11808:BoxAccumulator methodsFor: 'operations'!
{void} addAccumulatedBoxes: other {BoxAccumulator}
	"Add in all the boxes in another accumulator"
	
	Int32Zero almostTo: other index do: [ :box {Int32} |
		(other isDeleted: box) ifFalse:
			[self aboutToAdd.
			myRegions at: myIndex * mySpace axisCount
				storeMany: other secretRegions
				with: mySpace axisCount
				with: box * mySpace axisCount.
			myIndex := myIndex + 1]]!
*/
}
/**
 * Add the current box to the end of the array
 */
public int addBox(BoxStepper box) {
	return addProjections(box.region().secretRegions(), box.boxIndex());
/*
udanax-top.st:11820:BoxAccumulator methodsFor: 'operations'!
{Int32} addBox: box {BoxStepper}
	"Add the current box to the end of the array"
	
	^self addProjections: box region secretRegions
		with: box boxIndex!
*/
}
/**
 * Add the current box, transformed by the inverse of the dsp
 */
public void addInverseTransformedBox(BoxStepper box, GenericCrossDsp dsp) {
	int base;
	aboutToAdd();
	base = mySpace.axisCount() * myIndex;
	for (int dimension = 0; dimension < mySpace.axisCount(); dimension ++ ) {
		myRegions.store(base + dimension, ((dsp.subMapping(dimension)).inverseOfAll((box.projection(dimension)))));
	}
	myIndex = myIndex + 1;
/*
udanax-top.st:11826:BoxAccumulator methodsFor: 'operations'!
{void} addInverseTransformedBox: box {BoxStepper} with: dsp {GenericCrossDsp}
	"Add the current box, transformed by the inverse of the dsp"
	
	| base {Int32} |
	self aboutToAdd.
	base := mySpace axisCount * myIndex.
	Int32Zero almostTo: mySpace axisCount do: [ :dimension {Int32} |
		myRegions at: base + dimension
			store: ((dsp subMapping: dimension) inverseOfAll: (box projection: dimension))].
	myIndex := myIndex + 1.!
*/
}
/**
 * Add a box to the end of the array
 */
public int addProjections(PtrArray projections, int boxIndex) {
	aboutToAdd();
	myRegions.storeMany(myIndex * mySpace.axisCount(), projections, mySpace.axisCount(), boxIndex * mySpace.axisCount());
	myIndex = myIndex + 1;
	return myIndex - 1;
/*
udanax-top.st:11837:BoxAccumulator methodsFor: 'operations'!
{Int32} addProjections: projections {PtrArray of: XnRegion}
	with: boxIndex {Int32}
	"Add a box to the end of the array"
	
	self aboutToAdd.
	myRegions at: myIndex * mySpace axisCount
		storeMany: projections
		with: mySpace axisCount
		with: boxIndex * mySpace axisCount.
	myIndex := myIndex + 1.
	^myIndex - 1!
*/
}
/**
 * Add the current box, transformed by the dsp
 */
public void addTransformedBox(BoxStepper box, GenericCrossDsp dsp) {
	int base;
	aboutToAdd();
	base = myIndex * mySpace.axisCount();
	for (int dimension = 0; dimension < mySpace.axisCount(); dimension ++ ) {
		myRegions.store(base + dimension, ((dsp.subMapping(dimension)).ofAll((box.projection(dimension)))));
	}
	myIndex = myIndex + 1;
/*
udanax-top.st:11849:BoxAccumulator methodsFor: 'operations'!
{void} addTransformedBox: box {BoxStepper} with: dsp {GenericCrossDsp}
	"Add the current box, transformed by the dsp"
	
	| base {Int32} |
	self aboutToAdd.
	base := myIndex * mySpace axisCount.
	Int32Zero almostTo: mySpace axisCount do: [ :dimension {Int32} |
		myRegions at: base + dimension
			store: ((dsp subMapping: dimension) ofAll: (box projection: dimension))].
	myIndex := myIndex + 1.!
*/
}
/**
 * Intersect the current region with a box. May leave the result uncanonicalized
 */
public void intersectWithBox(BoxStepper box) {
	for (int i = 0; i < myIndex; i ++ ) {
		if ( ! (box.intersectBoxInto(myRegions, i))) {
			deleteBox(i);
		}
	}
/*
udanax-top.st:11860:BoxAccumulator methodsFor: 'operations'!
{void} intersectWithBox: box {BoxStepper}
	"Intersect the current region with a box. May leave the result uncanonicalized"
	
	Int32Zero almostTo: myIndex do: [ :i {Int32} |
		(box intersectBoxInto: myRegions with: i) ifFalse:
			[self deleteBox: i]].!
*/
}
/**
 * merge boxes which differ in only one projection
 */
public void mergeBoxes() {
	Ravi.thingToDo();
	for (int i = 
	/* hash lookup */
	0; i < myIndex; i ++ ) {
		if ( ! (isDeleted(i))) {
			for (int j = 0; j < myIndex; j ++ ) {
				if ( ! (i == j || (isDeleted(j)))) {
					tryMergeBoxes(i, j);
				}
			}
		}
	}
/*
udanax-top.st:11867:BoxAccumulator methodsFor: 'operations'!
{void} mergeBoxes
	"merge boxes which differ in only one projection"
	
	Ravi thingToDo. "hash lookup"
	Int32Zero almostTo: myIndex do: [ :i {Int32} |
		(self isDeleted: i) ifFalse:
			[Int32Zero almostTo: myIndex do: [ :j {Int32} |
				(i == j or: [self isDeleted: j]) ifFalse:
					[self tryMergeBoxes: i with: j]]]]!
*/
}
/**
 * The current region in the accumulator. CLIENT MUST KNOW THAT IT IS CANONICAL
 */
public XnRegion region() {
	return GenericCrossRegion.make(mySpace, myIndex, ((PtrArray) (myRegions.copy(myIndex * mySpace.axisCount()))));
/*
udanax-top.st:11877:BoxAccumulator methodsFor: 'operations'!
{XnRegion} region
	"The current region in the accumulator. CLIENT MUST KNOW THAT IT IS CANONICAL"
	
	^GenericCrossRegion make: mySpace
		with: myIndex
		with: ((myRegions copy: myIndex * mySpace axisCount) cast: PtrArray)!
*/
}
/**
 * Remove boxes which have been deleted
 */
public void removeDeleted() {
	int to;
	int from;
	from = to = 0;
	while (from < myIndex) {
		if ( ! (isDeleted(from))) {
			if (from > to) {
				myRegions.storeMany(to * mySpace.axisCount(), myRegions, mySpace.axisCount(), from * mySpace.axisCount());
			}
			to = to + 1;
		}
		from = from + 1;
	}
	myIndex = to;
/*
udanax-top.st:11884:BoxAccumulator methodsFor: 'operations'!
{void} removeDeleted
	"Remove boxes which have been deleted"
	
	| to {Int32} from {Int32} |
	from := to := Int32Zero.
	[from < myIndex] whileTrue:
		[(self isDeleted: from)  ifFalse:
			[from > to ifTrue:
				[myRegions at: to * mySpace axisCount
					storeMany: myRegions
					with: mySpace axisCount
					with: from * mySpace axisCount].
			to := to + 1].
		from := from + 1].
	myIndex := to!
*/
}
public void step(Heaper someObj) {
	unionWithBoxes(((GenericCrossRegion) someObj).boxStepper());
/*
udanax-top.st:11900:BoxAccumulator methodsFor: 'operations'!
{void} step: someObj {Heaper}
	self unionWithBoxes: (someObj cast: GenericCrossRegion) boxStepper!
*/
}
/**
 * Add the current box to the accumulator
 */
public void unionWithBox(BoxStepper box) {
	int initialIndex;
	int addedIndex;
	initialIndex = myIndex;
	addedIndex = addBox(box);
	distributeUnion(addedIndex, 0, initialIndex);
/*
udanax-top.st:11904:BoxAccumulator methodsFor: 'operations'!
{void} unionWithBox: box {BoxStepper}
	"Add the current box to the accumulator"
	
	| initialIndex {Int32} addedIndex {Int32} |
	initialIndex := myIndex.
	addedIndex := self addBox: box.
	self distributeUnion: addedIndex with: Int32Zero with: initialIndex.!
*/
}
/**
 * Add a sequence of disjoint boxes to the accumulator
 */
public void unionWithBoxes(BoxStepper boxes) {
	if (myIndex == 0) {
		while (boxes.hasValue()) {
			addBox(boxes);
			boxes.step();
		}
	}
	else {
		while (boxes.hasValue()) {
			unionWithBox(boxes);
			boxes.step();
		}
	}
/*
udanax-top.st:11912:BoxAccumulator methodsFor: 'operations'!
{void} unionWithBoxes: boxes {BoxStepper}
	"Add a sequence of disjoint boxes to the accumulator"
	
	myIndex = Int32Zero ifTrue:
		[[boxes hasValue] whileTrue:
			[self addBox: boxes.
			boxes step]]
	ifFalse:
		[[boxes hasValue]
			whileTrue:
				[self unionWithBox: boxes.
				boxes step]]!
*/
}
public Heaper value() {
	return region();
/*
udanax-top.st:11925:BoxAccumulator methodsFor: 'operations'!
{Heaper} value
	^self region!
*/
}
public static void initTimeNonInherited() {
	SomeAccumulators = InstanceCache.make(8);
/*
udanax-top.st:11938:BoxAccumulator class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeAccumulators := InstanceCache make: 8!
*/
}
public static void linkTimeNonInherited() {
	SomeAccumulators = null;
/*
udanax-top.st:11941:BoxAccumulator class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeAccumulators := NULL!
*/
}
public static BoxAccumulator make(GenericCrossRegion region) {
	Heaper result;
	result = SomeAccumulators.fetch();
	if (result == null) {
		return new BoxAccumulator(region);
	}
	else {
		return 
		/* TODO newBecome */
		new BoxAccumulator(region);
	}
/*
udanax-top.st:11946:BoxAccumulator class methodsFor: 'creation'!
make: region {GenericCrossRegion}
	| result {Heaper} |
	result := SomeAccumulators fetch.
	result == NULL
		ifTrue: [^ self create: region]
		ifFalse: [^ (self new.Become: result) create: region]!
*/
}
public static BoxAccumulator make(CrossSpace space, int expectedBoxCount) {
	Heaper result;
	result = SomeAccumulators.fetch();
	if (result == null) {
		return new BoxAccumulator(space, expectedBoxCount);
	}
	else {
		return 
		/* TODO newBecome */
		new BoxAccumulator(space, expectedBoxCount);
	}
/*
udanax-top.st:11953:BoxAccumulator class methodsFor: 'creation'!
make: space {CrossSpace} with: expectedBoxCount {Int32}
	| result {Heaper} |
	result := SomeAccumulators fetch.
	result == NULL
		ifTrue: [^ self create: space with: expectedBoxCount]
		ifFalse: [^ (self new.Become: result) create: space with: expectedBoxCount]!
*/
}
public BoxAccumulator() {
/*

Generated during transformation
*/
}
public BoxAccumulator(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
