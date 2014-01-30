/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.cross;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.collection.steppers.Accumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.cross.BoxAccumulator;
import info.dgjones.abora.gold.cross.GenericCrossSimpleRegionStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.ActualTuple;
import info.dgjones.abora.gold.spaces.cross.BoxProjectionStepper;
import info.dgjones.abora.gold.spaces.cross.BoxStepper;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.cross.GenericCrossRegion;
import info.dgjones.abora.gold.spaces.cross.GenericCrossSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Represents a region as a two-dimensional array of crosses of subregions.
 * Was NOT.A.TYPE but that obstructed compilation.
 * I think this might work better if the array is lexically sorted, but I am not sure there
 * is any meaningful way to do so. Thus there is no sorting assumed in the algorithms,
 * although the protocol may occasionally suggest that there might be.
 * Eventually this implementation may save space by using NULL to represent repetitions of a
 * sub region such that
 * fetchBoxProjection (box, dim) == NULL
 * only if
 * box > 0
 * && boxProjection (box, dim)->isEqual (boxProjection (box - 1, dim))
 * && (dim == 0
 * || fetchBoxProjection (box, dim - 1) == NULL)
 */
public class GenericCrossRegion extends CrossRegion {

	protected CrossSpace mySpace;
	protected int myCount;
	protected PtrArray myRegions;
/*
udanax-top.st:65635:
CrossRegion subclass: #GenericCrossRegion
	instanceVariableNames: '
		mySpace {CrossSpace}
		myCount {Int32}
		myRegions {PtrArray of: XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Cross'!
*/
/*
udanax-top.st:65642:
GenericCrossRegion comment:
'Represents a region as a two-dimensional array of crosses of subregions.
 Was NOT.A.TYPE but that obstructed compilation.
I think this might work better if the array is lexically sorted, but I am not sure there is any meaningful way to do so. Thus there is no sorting assumed in the algorithms, although the protocol may occasionally suggest that there might be.
Eventually this implementation may save space by using NULL to represent repetitions of a sub region such that
	fetchBoxProjection (box, dim) == NULL
only if
	box > 0
	&& boxProjection (box, dim)->isEqual (boxProjection (box - 1, dim))
	&& (dim == 0
		|| fetchBoxProjection (box, dim - 1) == NULL)'!
*/
/*
udanax-top.st:65654:
(GenericCrossRegion getOrMakeCxxClassDescription)
	friends:
'friend class BoxAccumulator;
friend class BoxProjectionStepper;
friend class BoxStepper;
friend class GenericCrossDsp;
friend class GenericCrossSpace;
friend class CrossOrderSpec;
';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:66033:
GenericCrossRegion class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:66036:
(GenericCrossRegion getOrMakeCxxClassDescription)
	friends:
'friend class BoxAccumulator;
friend class BoxProjectionStepper;
friend class BoxStepper;
friend class GenericCrossDsp;
friend class GenericCrossSpace;
friend class CrossOrderSpec;
';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GenericCrossRegion.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public CoordinateSpace coordinateSpace() {
	return mySpace;
/*
udanax-top.st:65667:GenericCrossRegion methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^mySpace!
*/
}
public int count() {
	int result;
	BoxStepper boxes;
	result = 0;
	boxes = boxStepper();
	while (boxes.hasValue()) {
		int sub;
		sub = 1;
		Stepper stomper = boxes.projectionStepper();
		for (; stomper.hasValue(); stomper.step()) {
			XnRegion proj = (XnRegion) stomper.fetch();
			if (proj == null) {
				continue ;
			}
			sub = sub * proj.count();
		}
		stomper.destroy();
		result = result + sub;
		boxes.step();
	}
	boxes.destroy();
	return result;
/*
udanax-top.st:65671:GenericCrossRegion methodsFor: 'accessing'!
{IntegerVar} count
	| result {IntegerVar} boxes {BoxStepper} |
	result := IntegerVarZero.
	boxes := self boxStepper.
	[boxes hasValue] whileTrue:
		[ | sub {IntegerVar} |
		sub := 1.
		boxes projectionStepper forEach: [ :proj {XnRegion} |
			sub := sub * proj count].
		result := result + sub.
		boxes step].
	boxes destroy.
	^result!
*/
}
public XnRegion projection(int index) {
	XnRegion result;
	BoxStepper boxes;
	if (myCount == 1) {
		return boxProjection(0, index);
	}
	result = (mySpace.axis(index)).emptyRegion();
	boxes = boxStepper();
	while (boxes.hasValue()) {
		result = result.unionWith((boxes.projection(index)));
		boxes.step();
	}
	boxes.destroy();
	return result;
/*
udanax-top.st:65686:GenericCrossRegion methodsFor: 'accessing'!
{XnRegion} projection: index {Int32}
	| result {XnRegion} boxes {BoxStepper} |
	myCount = 1 ifTrue: [^self boxProjection: Int32Zero with: index].
	result := (mySpace axis: index) emptyRegion.
	boxes := self boxStepper.
	[boxes hasValue] whileTrue:
		[result := result unionWith: (boxes projection: index).
		boxes step].
	boxes destroy.
	^result!
*/
}
public PtrArray projections() {
	PtrArray result;
	BoxStepper boxes;
	result = PtrArray.nulls(mySpace.axisCount());
	for (int i = 0; i < result.count(); i ++ ) {
		result.store(i, (mySpace.axis(i)).emptyRegion());
	}
	boxes = boxStepper();
	while (boxes.hasValue()) {
		boxes.unionBoxInto(result, 0);
		boxes.step();
	}
	boxes.destroy();
	return result;
/*
udanax-top.st:65698:GenericCrossRegion methodsFor: 'accessing'!
{PtrArray of: XnRegion} projections
	| result {PtrArray of: XnRegion} boxes {BoxStepper} |
	result := PtrArray nulls: mySpace axisCount.
	UInt32Zero almostTo: result count do: [ :i {UInt32} |
		result at: i store: (mySpace axis: i) emptyRegion].
	boxes := self boxStepper.
	[boxes hasValue] whileTrue:
		[boxes unionBoxInto: result with: Int32Zero.
		boxes step].
	boxes destroy.
	^result!
*/
}
public Position theOne() {
	PtrArray result;
	if ( ! (myCount == 1)) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_SINGLE_ELEMENT);
	}
	result = PtrArray.nulls(mySpace.axisCount());
	for (int i = 0; i < result.count(); i ++ ) {
		result.store(i, (boxProjection(0, i)).theOne());
	}
	return mySpace.crossOfPositions(result);
/*
udanax-top.st:65711:GenericCrossRegion methodsFor: 'accessing'!
{Position} theOne
	| result {PtrArray of: Position} |
	myCount = 1 ifFalse: [Heaper BLAST: #MustHaveSingleElement].
	result := PtrArray nulls: mySpace axisCount.
	Int32Zero almostTo: result count do: [ :i {Int32} |
		result at: i store: (self boxProjection: Int32Zero with: i) theOne].
	^mySpace crossOfPositions: result!
*/
}
public CrossSpace crossSpace() {
	return mySpace;
/*
udanax-top.st:65722:GenericCrossRegion methodsFor: 'protected:'!
{CrossSpace} crossSpace
	^mySpace!
*/
}
public int boxCount() {
	return myCount;
/*
udanax-top.st:65728:GenericCrossRegion methodsFor: 'private:'!
{Int32} boxCount
	^myCount!
*/
}
/**
 * A region is at a given 2D place in the array
 */
public XnRegion boxProjection(int box, int dimension) {
	return (XnRegion) (myRegions.fetch(box * crossSpace().axisCount() + dimension));
/*
udanax-top.st:65732:GenericCrossRegion methodsFor: 'private:'!
{XnRegion} boxProjection: box {Int32} with: dimension {Int32}
	"A region is at a given 2D place in the array"
	
	^(myRegions fetch: box * self crossSpace axisCount + dimension) cast: XnRegion!
*/
}
/**
 * A stepper over all projections of all boxes in the region
 */
public BoxProjectionStepper boxProjectionStepper() {
	return BoxProjectionStepper.make(this);
/*
udanax-top.st:65737:GenericCrossRegion methodsFor: 'private:'!
{BoxProjectionStepper} boxProjectionStepper
	"A stepper over all projections of all boxes in the region"
	
	^BoxProjectionStepper make: self!
*/
}
/**
 * A stepper over all boxes
 */
public BoxStepper boxStepper() {
	return BoxStepper.make(this);
/*
udanax-top.st:65742:GenericCrossRegion methodsFor: 'private:'!
{BoxStepper} boxStepper
	"A stepper over all boxes"
	
	^BoxStepper make: self!
*/
}
/**
 * Whether a region is at a given 2D place in the array. Searches forward and backward
 * through adjacent boxes which have the same hash value
 */
public boolean hasBoxProjection(XnRegion other, int box, int dimension) {
	int index;
	int hash;
	XnRegion sub;
	index = box;
	hash = other.hashForEqual();
	while (index >= 0 && ((sub = boxProjection(index, dimension)).hashForEqual() == hash)) {
		if (sub.isEqual(other)) {
			return true;
		}
		index = index - 1;
	}
	index = box + 1;
	while (index < myCount && ((sub = boxProjection(index, dimension)).hashForEqual() == hash)) {
		if (sub.isEqual(other)) {
			return true;
		}
		index = index + 1;
	}
	return false;
/*
udanax-top.st:65747:GenericCrossRegion methodsFor: 'private:'!
{BooleanVar} hasBoxProjection: other {XnRegion}
	with: box {Int32} with: dimension {Int32}
	"Whether a region is at a given 2D place in the array. Searches forward and backward through adjacent boxes which have the same hash value"
	
	| index {Int32} hash {UInt32} sub {XnRegion} |
	index := box.
	hash := other hashForEqual.
	[index >= Int32Zero and: [(sub := self boxProjection: index with: dimension) hashForEqual = hash]] whileTrue:
		[(sub isEqual: other) ifTrue:
			[^true].
		index := index - 1].
	index := box + 1.
	[index < myCount and: [(sub := self boxProjection: index with: dimension) hashForEqual = hash]] whileTrue:
		[(sub isEqual: other) ifTrue:
			[^true].
		index := index + 1].
	^false!
*/
}
/**
 * The array holding the regions. DO NOT MODIFY
 */
public PtrArray secretRegions() {
	return myRegions;
/*
udanax-top.st:65765:GenericCrossRegion methodsFor: 'private:'!
{PtrArray of: XnRegion} secretRegions
	"The array holding the regions. DO NOT MODIFY"
	
	^myRegions!
*/
}
public int actualHashForEqual() {
	int result;
	BoxStepper boxes;
	result = getCategory().hashForEqual();
	boxes = boxStepper();
	while (boxes.hasValue()) {
		result = result ^ boxes.boxHash();
		boxes.step();
	}
	boxes.destroy();
	return result;
/*
udanax-top.st:65772:GenericCrossRegion methodsFor: 'testing'!
{UInt32} actualHashForEqual
	| result {UInt32} boxes {BoxStepper} |
	result := self getCategory hashForEqual.
	boxes := self boxStepper.
	[boxes hasValue] whileTrue:
		[result := result bitXor: boxes boxHash.
		boxes step].
	boxes destroy.
	^result!
*/
}
public boolean hasMember(Position position) {
	BoxStepper boxes;
	boxes = boxStepper();
	while (boxes.hasValue()) {
		if (boxes.boxHasMember(((ActualTuple) position))) {
			return true;
		}
		boxes.step();
	}
	return false;
/*
udanax-top.st:65783:GenericCrossRegion methodsFor: 'testing'!
{BooleanVar} hasMember: position {Position}
	| boxes {BoxStepper} |
	boxes := self boxStepper.
	[boxes hasValue] whileTrue:
		[(boxes boxHasMember: (position cast: ActualTuple)) ifTrue:
			[^true].
		boxes step].
	^false!
*/
}
public boolean intersects(XnRegion other) {
	BoxStepper mine;
	/* TODO variable may not be initialized before being used */
	BoxStepper others = null;
	mine = boxStepper();
	while (mine.hasValue()) {
		others = ((GenericCrossRegion) other).boxStepper();
		while (others.hasValue()) {
			if (mine.boxIntersects(others)) {
				return true;
			}
			others.step();
		}
		mine.step();
	}
	mine.destroy();
	if (others != null ) {
		others.destroy();
	}
	return false;
/*
udanax-top.st:65793:GenericCrossRegion methodsFor: 'testing'!
{BooleanVar} intersects: other {XnRegion}
	| mine {BoxStepper} others {BoxStepper} |
	mine := self boxStepper.
	[mine hasValue] whileTrue:
		[others := (other cast: GenericCrossRegion) boxStepper.
		[others hasValue] whileTrue:
			[(mine boxIntersects: others) ifTrue:
				[^true].
			others step].
		mine step].
	mine destroy.
	others destroy.
	^false!
*/
}
public boolean isDistinction() {
	if (myCount > 1) {
		return false;
	}
	if (myCount == 0) {
		return true;
	}
	Stepper stomper = boxProjectionStepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion proj = (XnRegion) stomper.fetch();
		if (proj == null) {
			continue ;
		}
		if ( ! (proj.isDistinction())) {
			return false;
		}
	}
	stomper.destroy();
	return true;
/*
udanax-top.st:65808:GenericCrossRegion methodsFor: 'testing'!
{BooleanVar} isDistinction
	myCount > 1 ifTrue: [^false].
	myCount == Int32Zero ifTrue: [^true].
	self boxProjectionStepper forEach: [ :proj {XnRegion} |
		proj isDistinction ifFalse:
			[^false]].
	^true!
*/
}
public boolean isEmpty() {
	return myCount == 0;
/*
udanax-top.st:65817:GenericCrossRegion methodsFor: 'testing'!
{BooleanVar} isEmpty
	^myCount == Int32Zero!
*/
}
public boolean isEnumerable(OrderSpec order) {
	Someone.shouldImplement();
	return false;
/*
udanax-top.st:65821:GenericCrossRegion methodsFor: 'testing'!
{BooleanVar} isEnumerable: order {OrderSpec unused default: NULL}
	
	Someone shouldImplement.
	^false "fodder"!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof GenericCrossRegion) {
		GenericCrossRegion cross = (GenericCrossRegion) other;
		BoxStepper boxes;
		if ( ! (cross.boxCount() == myCount && (cross.crossSpace().isEqual(crossSpace())))) {
			return false;
		}
		boxes = boxStepper();
		while (boxes.hasValue()) {
			if ( ! (boxes.isBoxOf(cross))) {
				return false;
			}
			boxes.step();
		}
		boxes.destroy();
		return true;
	}
	else {
		return false;
	}
/*
udanax-top.st:65826:GenericCrossRegion methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other cast: GenericCrossRegion into: [ :cross | | boxes {BoxStepper} |
		(cross boxCount = myCount and: [cross crossSpace isEqual: self crossSpace])
			ifFalse: [^false].
		boxes := self boxStepper.
		[boxes hasValue] whileTrue:
			[(boxes isBoxOf: cross) ifFalse:
				[^false].
			boxes step].
		boxes destroy.
		^true]
	others:
		[^false].
	^ false "compiler fodder"!
*/
}
public boolean isFinite() {
	Stepper stomper = boxProjectionStepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion sub = (XnRegion) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		if ( ! (sub.isFinite())) {
			return false;
		}
	}
	stomper.destroy();
	return true;
/*
udanax-top.st:65842:GenericCrossRegion methodsFor: 'testing'!
{BooleanVar} isFinite
	self boxProjectionStepper forEach: [ :sub {XnRegion} |
		sub isFinite ifFalse:
			[^false]].
	^true!
*/
}
public boolean isFull() {
	if ( ! (myCount == 1)) {
		return false;
	}
	Stepper stomper = boxProjectionStepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion sub = (XnRegion) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		if ( ! (sub.isFull())) {
			return false;
		}
	}
	stomper.destroy();
	return true;
/*
udanax-top.st:65849:GenericCrossRegion methodsFor: 'testing'!
{BooleanVar} isFull
	myCount = 1 ifFalse: [^false].
	self boxProjectionStepper forEach: [ :sub {XnRegion} |
		sub isFull ifFalse: [^false]].
	^true!
*/
}
public boolean isSimple() {
	if (myCount > 1) {
		return false;
	}
	if (myCount == 0) {
		return true;
	}
	Stepper stomper = boxProjectionStepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion proj = (XnRegion) stomper.fetch();
		if (proj == null) {
			continue ;
		}
		if ( ! (proj.isSimple())) {
			return false;
		}
	}
	stomper.destroy();
	return true;
/*
udanax-top.st:65856:GenericCrossRegion methodsFor: 'testing'!
{BooleanVar} isSimple
	myCount > 1 ifTrue: [^false].
	myCount == Int32Zero ifTrue: [^true].
	self boxProjectionStepper forEach: [ :proj {XnRegion} |
		proj isSimple ifFalse: [^false]].
	^true!
*/
}
public boolean isSubsetOf(XnRegion other) {
	Ravi.thingToDo();
	/* figure out a more efficient algorithm - the one commented out below doesn't work */
	return super.isSubsetOf(other
	/* | others {BoxStepper} mine {BoxStepper} |
	others := other boxStepper.
	[others hasValue] whileTrue:
		[mine := self boxStepper.
		[mine hasValue] whileTrue:
			[(others boxIsSubsetOf: mine) ifFalse:
				[^false].
			mine step].
		others step].
	^true */
	);
/*
udanax-top.st:65864:GenericCrossRegion methodsFor: 'testing'!
{BooleanVar} isSubsetOf: other {XnRegion}
	Ravi thingToDo. "figure out a more efficient algorithm - the one commented out below doesn't work"
	^super isSubsetOf: other
	"| others {BoxStepper} mine {BoxStepper} |
	others := other boxStepper.
	[others hasValue] whileTrue:
		[mine := self boxStepper.
		[mine hasValue] whileTrue:
			[(others boxIsSubsetOf: mine) ifFalse:
				[^false].
			mine step].
		others step].
	^true"!
*/
}
public XnRegion asSimpleRegion() {
	PtrArray result;
	BoxProjectionStepper projections;
	if (isEmpty()) {
		return this;
	}
	result = PtrArray.nulls(mySpace.axisCount());
	projections = boxProjectionStepper();
	while (projections.hasValue()) {
		if ((result.fetch(projections.dimension())) == null) {
			result.store(projections.dimension(), projections.projection().asSimpleRegion());
		}
		else {
			result.store(projections.dimension(), (((XnRegion) (result.fetch(projections.dimension()))).simpleUnion(projections.projection())));
		}
		projections.step();
	}
	projections.destroy();
	return mySpace.crossOfRegions(result);
/*
udanax-top.st:65881:GenericCrossRegion methodsFor: 'operations'!
{XnRegion} asSimpleRegion
	| result {PtrArray} projections {BoxProjectionStepper} |
	self isEmpty ifTrue: [^self].
	result := PtrArray nulls: mySpace axisCount.
	projections := self boxProjectionStepper.
	[projections hasValue] whileTrue:
		[(result fetch: projections dimension) == NULL ifTrue:
			[result at: projections dimension store: projections projection asSimpleRegion]
		ifFalse:
			[result at: projections dimension
				store: (((result fetch: projections dimension) cast: XnRegion) simpleUnion: projections projection)].
		projections step].
	projections destroy.
	^mySpace crossOfRegions: result!
*/
}
public XnRegion complement() {
	XnRegion result;
	BoxStepper boxes;
	if (isEmpty()) {
		return mySpace.fullRegion();
	}
	boxes = boxStepper();
	result = boxes.boxComplement();
	boxes.step();
	while (boxes.hasValue()) {
		result = result.intersect(boxes.boxComplement());
		boxes.step();
	}
	boxes.destroy();
	return result;
/*
udanax-top.st:65897:GenericCrossRegion methodsFor: 'operations'!
{XnRegion} complement
	| result {XnRegion} boxes {BoxStepper} |
	self isEmpty ifTrue:
		[^mySpace fullRegion].
	boxes := self boxStepper.
	result := boxes boxComplement.
	boxes step.
	[boxes hasValue] whileTrue:
		[result := result intersect: boxes boxComplement.
		boxes step].
	boxes destroy.
	^result!
*/
}
public XnRegion intersect(XnRegion region) {
	if (region instanceof GenericCrossRegion) {
		GenericCrossRegion other = (GenericCrossRegion) region;
		BoxAccumulator result;
		GenericCrossRegion smaller;
		GenericCrossRegion larger;
		BoxStepper bits;
		BoxAccumulator piece;
		if (boxCount() < other.boxCount()) {
			smaller = this;
			larger = other;
		}
		else {
			smaller = other;
			larger = this;
		}
		if (smaller.isEmpty()) {
			return smaller;
		}
		bits = smaller.boxStepper();
		result = null;
		piece = BoxAccumulator.make(larger);
		while (bits.hasValue()) {
			piece.intersectWithBox(bits);
			if (result == null) {
				result = piece;
			}
			else {
				result.addAccumulatedBoxes(piece);
			}
			bits.step();
			if (bits.hasValue()) {
				piece = BoxAccumulator.make(larger);
			}
		}
		bits.destroy();
		result.mergeBoxes();
		result.removeDeleted();
		return result.region();
	}
	return null;
/*
udanax-top.st:65911:GenericCrossRegion methodsFor: 'operations'!
{XnRegion} intersect: region {XnRegion}
	region cast: GenericCrossRegion into: [ :other |
		| result {BoxAccumulator} smaller {GenericCrossRegion} larger {GenericCrossRegion}
		  bits {BoxStepper} piece {BoxAccumulator} |
		self boxCount < other boxCount
			ifTrue: [smaller := self. larger := other]
			ifFalse: [smaller := other. larger := self].
		smaller isEmpty
			ifTrue: [^smaller].
		bits := smaller boxStepper.
		result := NULL.
		piece := BoxAccumulator make: larger.
		[bits hasValue] whileTrue:
			[piece intersectWithBox: bits.
			result == NULL
				ifTrue: [result := piece]
				ifFalse: [result addAccumulatedBoxes: piece].
			bits step.
			bits hasValue
				ifTrue: [piece := BoxAccumulator make: larger]].
		bits destroy.
		result mergeBoxes.
		result removeDeleted.
		^result region].
	^ NULL "compiler fodder"!
*/
}
public XnRegion unionWith(XnRegion region) {
	BoxAccumulator result;
	if (region instanceof GenericCrossRegion) {
		GenericCrossRegion other = (GenericCrossRegion) region;
		BoxStepper stepper;
		result = BoxAccumulator.make(this);
		stepper = other.boxStepper();
		result.unionWithBoxes(stepper);
		stepper.destroy();
		result.mergeBoxes();
		result.removeDeleted();
		return result.region();
	}
	return null;
/*
udanax-top.st:65938:GenericCrossRegion methodsFor: 'operations'!
{XnRegion} unionWith: region {XnRegion}
	| result {BoxAccumulator} |
	region cast: GenericCrossRegion into: [ :other |
		| stepper {BoxStepper} |
		result := BoxAccumulator make: self.
		stepper := other boxStepper.
		result unionWithBoxes: stepper.
		stepper destroy.
		result mergeBoxes.
		result removeDeleted.
		^result region].
	^ NULL "compiler fodder"!
*/
}
public void printOn(PrintWriter oo) {
	BoxStepper boxes;
	String between;
	oo.print("{");
	boxes = boxStepper();
	while (boxes.hasValue()) {
		between = "";
		Stepper stomper = boxes.projectionStepper();
		for (; stomper.hasValue(); stomper.step()) {
			XnRegion proj = (XnRegion) stomper.fetch();
			if (proj == null) {
				continue ;
			}
			oo.print(between);
			if (proj.isFull()) {
				oo.print("*");
			}
			else {
				oo.print(proj);
			}
			between = " x ";
		}
		stomper.destroy();
		boxes.step();
		if (boxes.hasValue()) {
			oo.print(", ");
		}
	}
	boxes.destroy();
	oo.print("}");
/*
udanax-top.st:65954:GenericCrossRegion methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	| boxes {BoxStepper} between {char star} |
	oo << '{'.
	boxes := self boxStepper.
	[boxes hasValue] whileTrue:
		[between := ''.
		boxes projectionStepper forEach: [ :proj {XnRegion} |
			oo << between.
			proj isFull
				ifTrue: [oo << '*']
				ifFalse: [oo << proj].
			between := ' x '].
		boxes step.
		boxes hasValue ifTrue:
			[oo << ', ']].
	boxes destroy.
	oo << '}'!
*/
}
public Stepper boxes() {
	return boxStepper();
/*
udanax-top.st:65975:GenericCrossRegion methodsFor: 'enumerating'!
{Stepper of: CrossRegion} boxes
	^self boxStepper!
*/
}
public ScruSet distinctions() {
	Accumulator result;
	BoxProjectionStepper ps;
	if ( ! (isSimple())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_SIMPLE);
	}
	result = SetAccumulator.make();
	ps = boxProjectionStepper();
	Stepper stomper = ps;
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion sub = (XnRegion) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		Stepper stomper2 = sub.distinctions().stepper();
		for (; stomper2.hasValue(); stomper2.step()) {
			XnRegion dist = (XnRegion) stomper2.fetch();
			if (dist == null) {
				continue ;
			}
			result.step((mySpace.extrusion(ps.dimension(), dist)));
		}
		stomper2.destroy();
	}
	stomper.destroy();
	return (ScruSet) result.value();
/*
udanax-top.st:65979:GenericCrossRegion methodsFor: 'enumerating'!
{ScruSet of: XnRegion} distinctions
	| result {Accumulator} ps {BoxProjectionStepper} |
	self isSimple ifFalse:
		[Heaper BLAST: #MustBeSimple].
	result := SetAccumulator make.
	ps := self boxProjectionStepper.
	ps forEach: [ :sub {XnRegion} |
		sub distinctions stepper forEach: [ :dist {XnRegion} |
			result step: (mySpace extrusion: ps dimension with: dist)]].
	^result value cast: ScruSet!
*/
}
public boolean isBox() {
	return isSimple();
/*
udanax-top.st:65991:GenericCrossRegion methodsFor: 'enumerating'!
{BooleanVar} isBox
	^self isSimple!
*/
}
public Stepper simpleRegions(OrderSpec order) {
	if (order != null) {
		throw new UnimplementedException();
	}
	return GenericCrossSimpleRegionStepper.make(mySpace, boxStepper());
/*
udanax-top.st:65995:GenericCrossRegion methodsFor: 'enumerating'!
{Stepper} simpleRegions: order {OrderSpec default: NULL} 
	order ~~ NULL ifTrue: [self unimplemented].
	^GenericCrossSimpleRegionStepper make: mySpace with: self boxStepper!
*/
}
public Stepper actualStepper(OrderSpec order) {
	if (isEmpty()) {
		return Stepper.emptyStepper();
	}
	Ravi.thingToDo();
	/* do a real stepper */
	Someone.hack();
	return Stepper.itemStepper(theOne());
/*
udanax-top.st:66002:GenericCrossRegion methodsFor: 'protected: enumerating'!
{Stepper of: Position} actualStepper: order {OrderSpec unused} 
	self isEmpty ifTrue: [^Stepper emptyStepper].
	Ravi thingToDo. "do a real stepper"
	self hack.
	^Stepper itemStepper: self theOne!
*/
}
public GenericCrossRegion(CrossSpace space, int count, PtrArray regions) {
	super();
	mySpace = space;
	myCount = count;
	myRegions = regions;
/*
udanax-top.st:66011:GenericCrossRegion methodsFor: 'protected: create'!
create: space {CrossSpace} with: count {Int32} with: regions {PtrArray of: XnRegion}
	super create.
	mySpace := space.
	myCount := count.
	myRegions := regions.!
*/
}
public GenericCrossRegion(Rcvr receiver) {
	super(receiver);
	mySpace = (CrossSpace) receiver.receiveHeaper();
	myCount = receiver.receiveInt32();
	myRegions = (PtrArray) receiver.receiveHeaper();
/*
udanax-top.st:66020:GenericCrossRegion methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	mySpace _ receiver receiveHeaper.
	myCount _ receiver receiveInt32.
	myRegions _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(mySpace);
	xmtr.sendInt32(myCount);
	xmtr.sendHeaper(myRegions);
/*
udanax-top.st:66026:GenericCrossRegion methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: mySpace.
	xmtr sendInt32: myCount.
	xmtr sendHeaper: myRegions.!
*/
}
public static CrossRegion empty(GenericCrossSpace space) {
	return new GenericCrossRegion(space, 0, PtrArray.empty());
/*
udanax-top.st:66049:GenericCrossRegion class methodsFor: 'private: pseudo constructors'!
{CrossRegion} empty: space {GenericCrossSpace}
	^self create: space with: Int32Zero with: PtrArray empty!
*/
}
/**
 * Only used during construction; must pass the array in explicitly since the space isnt
 * initialized yet
 */
public static CrossRegion full(GenericCrossSpace space, PtrArray subSpaces) {
	PtrArray result;
	result = PtrArray.nulls(subSpaces.count());
	for (int dimension = 0; dimension < result.count(); dimension ++ ) {
		result.store(dimension, ((CoordinateSpace) (subSpaces.fetch(dimension))).fullRegion());
	}
	return new GenericCrossRegion(space, 1, result);
/*
udanax-top.st:66053:GenericCrossRegion class methodsFor: 'private: pseudo constructors'!
{CrossRegion} full: space {GenericCrossSpace}
	with: subSpaces {PtrArray of: CoordinateSpace}
	"Only used during construction; must pass the array in explicitly since the space isnt initialized yet"
	| result {PtrArray of: XnRegion} |
	result := PtrArray nulls: subSpaces count.
	Int32Zero almostTo: result count do: [ :dimension {Int32} |
		result at: dimension store: ((subSpaces fetch: dimension) cast: CoordinateSpace) fullRegion].
	^self create: space with: 1 with: result!
*/
}
public static GenericCrossRegion make(CrossSpace space, int count, PtrArray regions) {
	return new GenericCrossRegion(space, count, regions);
/*
udanax-top.st:66065:GenericCrossRegion class methodsFor: 'create'!
make: space {CrossSpace} with: count {Int32} with: regions {PtrArray of: XnRegion}
	^ self create: space with: count with: regions!
*/
}
public GenericCrossRegion() {
/*

Generated during transformation
*/
}
}
