/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.integers;

import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.AscendingIntegerStepper;
import info.dgjones.abora.gold.spaces.integers.DescendingIntegerStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerEdgeAccumulator;
import info.dgjones.abora.gold.spaces.integers.IntegerEdgeStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerMapping;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSimpleRegionStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * An IntegerRegion can be thought of as the disjoint union of intervals and inequalities.
 * The interesting cases to look at are:
 * The distinctions:
 * 1) The empty region
 * 2) The full region
 * 3) A "left" inequality -- e.g., everything less that 3.
 * 4) A "right" inequality -- e.g., everything greater than or equal to 7
 * The non-distinction simple regions:
 * 5) An interval -- e.g., from 3 inclusive to 7 exclusive
 * The non-simple regions:
 * 6) A disjoint union of (in order) an optional left inequality, a set of
 * intervals, and an optional right inequality.
 * If a non-empty region has a least element, then it "isBoundedLeft".  Otherwise it extends
 * leftwards indefinitely.  Similarly, if a non-empty region has a greatest element, then it
 * "isBoundedRight".  Otherwise it extends rightwards indefinitely.  (We may figuratively
 * speak of the region extending towards + or - infinity, but we have purposely avoided
 * introducing any value which represents an infinity.)
 * Looking at cases again:
 * 1) "isBoundedLeft" and "isBoundedRight" since it doesn''t extent
 * indenfinitely in either direction.  (A danger to watch out for is that
 * this still has niether a greatest nor a least element).
 * 2) niether.
 * 3) "isBoundedRight"
 * 4) "isBoundedLeft"
 * 5) "isBoundedLeft" and "isBoundedRight"
 * 6) "isBoundedLeft" iff doesn''t include a left inequality,
 * "isBoundedRight" iff doesn''t include a right inequality.
 * An efficiency note:  Currently many of the method which could be doing an O(log) binary
 * search (such as hasMember) are instead doing a linear search.  This will be fixed if it
 * turns out to be a problem in practice.
 * See OrderedRegion.
 */
public class IntegerRegion extends XnRegion {

	protected boolean myStartsInside;
	protected int myTransitionCount;
	protected IntegerVarArray myTransitions;
	protected static IntegerRegion AllIntegers;
	protected static IntegerRegion EmptyIntegerRegion;
	protected static IntegerRegion LastAfterRegion;
	protected static int LastAfterStart;
	protected static int LastBeforeEnd;
	protected static IntegerRegion LastBeforeRegion;
	protected static IntegerRegion LastInterval;
	protected static int LastLeft;
	protected static int LastRight;
	protected static int LastSingleton;
	protected static IntegerRegion LastSingletonRegion;
/*
udanax-top.st:68420:
XnRegion subclass: #IntegerRegion
	instanceVariableNames: '
		myStartsInside {BooleanVar}
		myTransitionCount {UInt32}
		myTransitions {IntegerVarArray}'
	classVariableNames: '
		AllIntegers {IntegerRegion} 
		EmptyIntegerRegion {IntegerRegion} 
		LastAfterRegion {IntegerRegion} 
		LastAfterStart {IntegerVar} 
		LastBeforeEnd {IntegerVar} 
		LastBeforeRegion {IntegerRegion} 
		LastInterval {IntegerRegion} 
		LastLeft {IntegerVar} 
		LastRight {IntegerVar} 
		LastSingleton {IntegerVar} 
		LastSingletonRegion {IntegerRegion} '
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Integers'!
*/
/*
udanax-top.st:68438:
IntegerRegion comment:
'An IntegerRegion can be thought of as the disjoint union of intervals and inequalities.  The interesting cases to look at are:
	
	The distinctions:
		1) The empty region
		2) The full region
		3) A "left" inequality -- e.g., everything less that 3.
		4) A "right" inequality -- e.g., everything greater than or equal to 7
		
	The non-distinction simple regions:
		5) An interval -- e.g., from 3 inclusive to 7 exclusive
		
	The non-simple regions:
		6) A disjoint union of (in order) an optional left inequality, a set of 
			intervals, and an optional right inequality.  
		
	If a non-empty region has a least element, then it "isBoundedLeft".  Otherwise it extends leftwards indefinitely.  Similarly, if a non-empty region has a greatest element, then it "isBoundedRight".  Otherwise it extends rightwards indefinitely.  (We may figuratively speak of the region extending towards + or - infinity, but we have purposely avoided introducing any value which represents an infinity.)
	
	Looking at cases again:
		1) "isBoundedLeft" and "isBoundedRight" since it doesn''t extent 
			indenfinitely in either direction.  (A danger to watch out for is that 
			this still has niether a greatest nor a least element).
		2) niether.
		3) "isBoundedRight"
		4) "isBoundedLeft"
		5) "isBoundedLeft" and "isBoundedRight"
		6) "isBoundedLeft" iff doesn''t include a left inequality,
			"isBoundedRight" iff doesn''t include a right inequality.
			
	An efficiency note:  Currently many of the method which could be doing an O(log) binary search (such as hasMember) are instead doing a linear search.  This will be fixed if it turns out to be a problem in practice.
	
	See OrderedRegion.'!
*/
/*
udanax-top.st:68470:
(IntegerRegion getOrMakeCxxClassDescription)
	friends:
'friend class ID;
friend class IntegerMapping;
friend class IntegerSpace;
friend class PointRegion;
friend class SlicePointRegion;
';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
/*
udanax-top.st:69102:
IntegerRegion class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:69105:
(IntegerRegion getOrMakeCxxClassDescription)
	friends:
'friend class ID;
friend class IntegerMapping;
friend class IntegerSpace;
friend class PointRegion;
friend class SlicePointRegion;
';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IntegerRegion.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Will always return the smallest simple region which contains all my positions
 */
public XnRegion asSimpleRegion() {
	if (isSimple()) {
		return this;
	}
	if (isBoundedBelow()) {
		if (isBoundedAbove()) {
			return IntegerRegion.make(start(), stop());
		}
		else {
			return IntegerRegion.after(start());
		}
	}
	else {
		if (isBoundedAbove()) {
			return IntegerRegion.before(stop());
		}
		else {
			return IntegerRegion.allIntegers();
		}
	}
/*
udanax-top.st:68482:IntegerRegion methodsFor: 'accessing'!
{XnRegion} asSimpleRegion
	"Will always return the smallest simple region which contains all my positions"
	self isSimple ifTrue: [^self].
	self isBoundedBelow
		ifTrue: [self isBoundedAbove
			ifTrue: [^IntegerRegion make: self start with: self stop]
			ifFalse: [^IntegerRegion after: self start]]
		ifFalse: [self isBoundedAbove
			ifTrue: [^IntegerRegion before: self stop]
			ifFalse: [^IntegerRegion allIntegers]]!
*/
}
/**
 * the region before the last element of the set.
 * What on earth is this for? (Yes, I've looked at senders)
 */
public XnRegion beforeLast() {
	if (myTransitionCount == 0) {
		return this;
	}
	if (isBoundedAbove()) {
		return IntegerRegion.before(stop());
	}
	else {
		return IntegerRegion.allIntegers();
	}
/*
udanax-top.st:68494:IntegerRegion methodsFor: 'accessing'!
{XnRegion} beforeLast
	"the region before the last element of the set.  
	What on earth is this for? (Yes, I've looked at senders)"
	myTransitionCount == Int32Zero ifTrue:
		[^self].
	self isBoundedAbove
		ifTrue: [^IntegerRegion before: self stop]
		ifFalse: [^IntegerRegion allIntegers]!
*/
}
/**
 * transform the region into a simple region with left bound 0
 * (or -inf if unbounded).
 * What on earth is this for? (Yes, I've looked at senders)
 */
public XnRegion compacted() {
	/* ((IntegerRegion make: 3 with: 7) unionWith: (IntegerRegion before: -10)) compacted */
	if (isBoundedBelow()) {
		if (isBoundedAbove()) {
			return IntegerRegion.make(0, count());
		}
		else {
			return IntegerRegion.after(0);
		}
	}
	else {
		if (isBoundedAbove()) {
			return IntegerRegion.before(((myTransitions.integerVarAt(0)) + (intersect((IntegerRegion.after((myTransitions.integerVarAt(0)))))).count()));
		}
		else {
			return IntegerRegion.allIntegers();
		}
	}
/*
udanax-top.st:68504:IntegerRegion methodsFor: 'accessing'!
{XnRegion} compacted
	"transform the region into a simple region with left bound 0 
	(or -inf if unbounded).
	What on earth is this for? (Yes, I've looked at senders)"
	"((IntegerRegion make: 3 with: 7) unionWith: (IntegerRegion before: -10)) compacted"
	self isBoundedBelow
		ifTrue: [self isBoundedAbove
			ifTrue: [^IntegerRegion make: IntegerVar0 with: self count]
			ifFalse: [^IntegerRegion after: IntegerVar0]]
		ifFalse: [self isBoundedAbove
			ifTrue: [^IntegerRegion before: ((myTransitions integerVarAt: Int32Zero)
				+ (self intersect: (IntegerRegion after: (myTransitions integerVarAt: Int32Zero))) count)]
			ifFalse: [^IntegerRegion allIntegers]]!
*/
}
/**
 * A mapping to transform the region into a simple region with left bound 0 (or -inf if
 * unbounded). The domain of the mapping is precisely this region.
 * This is primarily used in XuText Waldos, which only deal with contiguous zero-based
 * regions of data.
 */
public Mapping compactor() {
	/* ((IntegerRegion make: 3 with: 7) unionWith: (IntegerRegion after: 10)) compactor */
	Mapping result;
	int end;
	int index;
	IntegerRegion simple;
	Mapping sub;
	if (myTransitionCount == 0) {
		return IntegerMapping.make().restrict(this);
	}
	result = null;
	if (myStartsInside) {
		end = myTransitions.integerVarAt(0);
		index = 1;
	}
	else {
		end = 0;
		index = 0;
	}
	while (index < myTransitionCount) {
		simple = simpleRegionAtIndex(index);
		sub = (IntegerMapping.make(end - simple.start())).restrict(simple);
		if (result == null) {
			result = sub;
		}
		else {
			result = result.combine(sub);
		}
		if (simple.isBoundedAbove()) {
			end = end + simple.count();
		}
		index = index + 2;
	}
	if (result != null) {
		return result.restrict(this);
	}
	else {
		return IntegerMapping.make().restrict(this);
	}
/*
udanax-top.st:68519:IntegerRegion methodsFor: 'accessing'!
{Mapping} compactor
	"A mapping to transform the region into a simple region with left bound 0 (or -inf if unbounded). The domain of the mapping is precisely this region.
	This is primarily used in XuText Waldos, which only deal with contiguous zero-based regions of data."
	"((IntegerRegion make: 3 with: 7) unionWith: (IntegerRegion after: 10)) compactor"
	| result {Mapping} end {IntegerVar} index {UInt32} simple {IntegerRegion} sub {Mapping} |
	myTransitionCount == Int32Zero ifTrue: [^IntegerMapping make restrict: self].
	result _ NULL.
	myStartsInside ifTrue:
		[end _ myTransitions integerVarAt: Int32Zero.
		index _ 1]
	ifFalse:
		[end _ IntegerVar0.
		index _ Int32Zero].
	[index < myTransitionCount] whileTrue:
		[simple _ self simpleRegionAtIndex: index.
		sub _ (IntegerMapping make: end - simple start) restrict: simple.
		result == NULL
			ifTrue: [result _ sub]
			ifFalse: [result _ result combine: sub].
		simple isBoundedAbove ifTrue:
			[end _ end + simple count].
		index _ index + 2].
	result ~~ NULL
		ifTrue: [^result restrict: self]
		ifFalse: [^IntegerMapping make restrict: self]!
*/
}
public CoordinateSpace coordinateSpace() {
	return IntegerSpace.make();
/*
udanax-top.st:68546:IntegerRegion methodsFor: 'accessing'!
{CoordinateSpace INLINE} coordinateSpace
	^IntegerSpace make!
*/
}
/**
 * True if this is either empty or a simple region with lower bound of either 0 or -infinity.
 * Equivalent to
 * this->compacted()->isEqual (this)
 */
public boolean isCompacted() {
	if (myStartsInside) {
		return myTransitionCount == 1;
	}
	else {
		return myTransitionCount == 0 || (myTransitionCount == 2 && ((myTransitions.integerVarAt(0)) == 0));
	}
/*
udanax-top.st:68549:IntegerRegion methodsFor: 'accessing'!
{BooleanVar} isCompacted
	"True if this is either empty or a simple region with lower bound of either 0 or -infinity. Equivalent to
		this->compacted()->isEqual (this)"
	
	myStartsInside
		ifTrue: [^myTransitionCount = 1]
		ifFalse: [^myTransitionCount = Int32Zero
			or: [myTransitionCount = 2
				and: [(myTransitions integerVarAt: Int32Zero) = IntegerVar0]]]!
*/
}
/**
 * This is a hack for finding the smallest available index to allocate that is not in a
 * particular region (a table domain, for example).
 */
public int nearestIntHole(int index) {
	IntegerEdgeStepper edges;
	boolean test;
	edges = edgeStepper();
	while (edges.hasValue()) {
		if (index < edges.edge()) {
			if (edges.isEntering()) {
				edges.destroy();
				return index;
			}
			else {
				int result;
				result = edges.edge();
				edges.destroy();
				return result;
			}
		}
		edges.step();
	}
	test = edges.isEntering();
	edges.destroy();
	if (test) {
		return index;
	}
	else {
		throw new AboraRuntimeException(AboraRuntimeException.NO_HOLE);
	}
/*
udanax-top.st:68559:IntegerRegion methodsFor: 'accessing'!
{IntegerVar} nearestIntHole: index {IntegerVar}
	"This is a hack for finding the smallest available index to allocate that is not in a particular region (a table domain, for example)."
	
	| edges {IntegerEdgeStepper} test {BooleanVar} |
	edges _ self edgeStepper.
	[edges hasValue] whileTrue:
		[index < edges edge ifTrue:
			[edges isEntering
				ifTrue: [edges destroy. ^index]
				ifFalse:
					[| result {IntegerVar}|
					result := edges edge.
					edges destroy.
					^ result]].
		edges step].
	test := edges isEntering.
	edges destroy.
	test
		ifTrue: [^index]
		ifFalse: [Heaper BLAST: #NoHole].
	^IntegerVarZero "fodder"!
*/
}
/**
 * The region starting from pos (inclusive) and going until the next transition. If I contain
 * pos, then I return the longest contiguous region starting at pos of positions I contain.
 * If I don't contain pos, then I return the longest contiguous region starting at pos of
 * positions I do not contain.
 */
public IntegerRegion runAt(int pos) {
	for (int i = 0; i < myTransitionCount; i ++ ) {
		if ((myTransitions.integerVarAt(i)) > pos) {
			return IntegerRegion.make(pos, (myTransitions.integerVarAt(i)));
		}
	}
	return IntegerRegion.after(pos);
/*
udanax-top.st:68581:IntegerRegion methodsFor: 'accessing'!
{IntegerRegion} runAt: pos {IntegerVar} 
	"The region starting from pos (inclusive) and going until the next transition. If I contain pos, then I return the longest contiguous region starting at pos of positions I contain. If I don't contain pos, then I return the longest contiguous region starting at pos of positions I do not contain."
	Int32Zero almostTo: myTransitionCount do: [:i {UInt32} | (myTransitions integerVarAt: i)
			> pos ifTrue: [^IntegerRegion make: pos with: (myTransitions integerVarAt: i)]].
	^IntegerRegion after: pos!
*/
}
/**
 * I have a start only if I'm not empty and I am isBoundedBelow. I report as my start the
 * smallest position I *do* contain, which is one greater than the largest position I do not
 * contain. The lower bound of the interval from 3 inclusive to 7 exclusive is 3.
 * See 'stop', you may be surprised.
 */
public int start() {
	if (myStartsInside || (myTransitionCount == 0)) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	return myTransitions.integerVarAt(0);
/*
udanax-top.st:68588:IntegerRegion methodsFor: 'accessing'!
{IntegerVar CLIENT} start
	"I have a start only if I'm not empty and I am isBoundedBelow. I report as my start the smallest position I *do* contain, which is one greater than the largest position I do not contain. The lower bound of the interval from 3 inclusive to 7 exclusive is 3. 
	See 'stop', you may be surprised."
	(myStartsInside or: [myTransitionCount == Int32Zero])
		ifTrue: [Heaper BLAST: #InvalidRequest].
	^myTransitions integerVarAt: Int32Zero!
*/
}
/**
 * I have a stop only if I'm not empty and I am isBoundedAbove. I report as my stop the
 * smallest position I *do not* contain, which is one greater than the largest position I do
 * contain.  The ustop of the interval from 3 inclusive to 7 exclusive is 7.
 * See 'start', you may be surprised.
 */
public int stop() {
	if ( ! isBoundedAbove() || (myTransitionCount == 0)) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	return myTransitions.integerVarAt(myTransitionCount - 1);
/*
udanax-top.st:68596:IntegerRegion methodsFor: 'accessing'!
{IntegerVar CLIENT} stop
	"I have a stop only if I'm not empty and I am isBoundedAbove. I report as my stop the smallest position I *do not* contain, which is one greater than the largest position I do contain.  The ustop of the interval from 3 inclusive to 7 exclusive is 7.
	See 'start', you may be surprised."
	(self isBoundedAbove not or: [myTransitionCount == Int32Zero])
		ifTrue: [Heaper BLAST: #InvalidRequest]. 
	^myTransitions integerVarAt: myTransitionCount - 1!
*/
}
public IntegerRegion(boolean startsInside, int count, IntegerVarArray transitions) {
	super();
	myStartsInside = startsInside;
	myTransitionCount = count;
	myTransitions = transitions;
/*
udanax-top.st:68606:IntegerRegion methodsFor: 'unprotected creation'!
create: startsInside {BooleanVar} with: count {UInt32} with: transitions {IntegerVarArray}
	super create.
	myStartsInside _ startsInside.
	myTransitionCount _ count.
	myTransitions _ transitions.!
*/
}
public void destroy() {
/*
udanax-top.st:68614:IntegerRegion methodsFor: 'destroy'!
{void} destroy!
*/
}
public void printOn(PrintWriter oo) {
	if (isEmpty()) {
		oo.print("{}");
	}
	else {
		IntegerEdgeStepper edges;
		String between;
		edges = edgeStepper();
		if ( ! (isSimple())) {
			oo.print("{");
		}
		if ( ! (edges.isEntering())) {
			oo.print("(-inf");
		}
		between = "[";
		while (edges.hasValue()) {
			if (edges.isEntering()) {
				oo.print(between);
			}
			else {
				oo.print(", ");
			}
			oo.print(edges.edge());
			between = "), [";
			edges.step();
		}
		if (edges.isEntering()) {
			oo.print(")");
		}
		else {
			oo.print(", +inf)");
		}
		if ( ! (isSimple())) {
			oo.print("}");
		}
		edges.destroy();
	}
/*
udanax-top.st:68618:IntegerRegion methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	self isEmpty ifTrue:
		[oo << '{}']
	ifFalse:
		[ | edges {IntegerEdgeStepper} between {char star} |
		edges _ self edgeStepper.
		self isSimple ifFalse: [oo << '{'].
		edges isEntering ifFalse: [oo << '(-inf'].
		between _ '['.
		[edges hasValue] whileTrue:
			[edges isEntering ifTrue: [oo << between] ifFalse: [oo << ', '].
			oo << edges edge.
			between _ '), ['.
			edges step].
		edges isEntering ifTrue: [oo << ')'] ifFalse: [oo << ', +inf)'].
		self isSimple ifFalse: [oo << '}'].
		edges destroy]!
*/
}
public int actualHashForEqual() {
	return (myTransitions.elementsHash(myTransitionCount)) ^ (((myStartsInside) ? 9617 : 518293) ^ myTransitionCount * 17);
/*
udanax-top.st:68638:IntegerRegion methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(myTransitions elementsHash: myTransitionCount)
		bitXor: ((myStartsInside ifTrue: [9617] ifFalse: [518293])
		bitXor: myTransitionCount * 17)!
*/
}
/**
 * Unboxed version.  See class comment for XuInteger
 */
public boolean hasIntMember(int key) {
	IntegerEdgeStepper edges;
	boolean result;
	edges = edgeStepper();
	while (edges.hasValue()) {
		if (key < edges.edge()) {
			result = ! edges.isEntering();
			edges.destroy();
			return result;
		}
		edges.step();
	}
	result = ! edges.isEntering();
	edges.destroy();
	return result;
/*
udanax-top.st:68643:IntegerRegion methodsFor: 'testing'!
{BooleanVar} hasIntMember: key {IntegerVar}
	"Unboxed version.  See class comment for XuInteger"
	
	| edges {IntegerEdgeStepper} result {BooleanVar} |
	edges _ self edgeStepper.
	[edges hasValue] whileTrue:
		[key < edges edge ifTrue:
			[result := edges isEntering not.
			edges destroy.
			^ result].
		edges step].
	result := edges isEntering not.
	edges destroy.
	^ result!
*/
}
public boolean hasMember(Position pos) {
	return hasIntMember(((IntegerPos) pos).asIntegerVar());
/*
udanax-top.st:68658:IntegerRegion methodsFor: 'testing'!
{BooleanVar} hasMember: pos {Position}
	^self hasIntMember: (pos cast: IntegerPos) asIntegerVar!
*/
}
public boolean intersects(XnRegion region) {
	IntegerRegion other;
	other = (IntegerRegion) region;
	if (0 == myTransitionCount) {
		if (myStartsInside) {
			return ! other.isEmpty();
		}
		else {
			return ! isEmpty();
		}
	}
	else {
		if (0 == other.transitionCount()) {
			if (other.isBoundedBelow()) {
				return ! other.isEmpty();
			}
			else {
				return ! isEmpty();
			}
		}
		else {
			IntegerEdgeStepper mine;
			IntegerEdgeStepper others;
			/* TODO variable may not be initialized before being used */
			int pending = 0;
			boolean havePending;
			int index;
			boolean startsInside;
			mine = edgeStepper();
			others = other.edgeStepper();
			havePending = false;
			index = 0;
			while (mine.hasValue() && (others.hasValue())) {
				int me;
				int it;
				me = mine.edge();
				it = others.edge();
				if (me < it) {
					if ( ! others.isEntering()) {
						if (havePending) {
							if (pending == me) {
								havePending = false;
							}
							else {
								mine.destroy();
								others.destroy();
								return true;
							}
						}
						else {
							havePending = true;
							pending = me;
							index = 1;
						}
					}
					mine.step();
				}
				else {
					if ( ! mine.isEntering()) {
						if (havePending) {
							if (pending == it) {
								havePending = false;
							}
							else {
								mine.destroy();
								others.destroy();
								return true;
							}
						}
						else {
							havePending = true;
							pending = it;
							index = 1;
						}
					}
					others.step();
				}
			}
			startsInside = myStartsInside && ( ! other.isBoundedBelow());
			if (mine.hasValue() && ( ! others.isEntering())) {
				if (havePending) {
					if (pending == mine.edge()) {
						havePending = false;
					}
					else {
						mine.destroy();
						others.destroy();
						return true;
					}
				}
				else {
					havePending = true;
					index = 1;
				}
				if (havePending) {
					mine.destroy();
					others.destroy();
					return true;
				}
				mine.step();
				if (mine.hasValue()) {
					mine.destroy();
					others.destroy();
					return (index == 0 && (startsInside)) || (index != 0);
				}
			}
			if (others.hasValue() && ( ! mine.isEntering())) {
				if (havePending) {
					if (pending == others.edge()) {
						havePending = false;
					}
					else {
						mine.destroy();
						others.destroy();
						return true;
					}
				}
				else {
					havePending = true;
					index = 1;
				}
				if (havePending) {
					mine.destroy();
					others.destroy();
					return true;
				}
				others.step();
				if (others.hasValue()) {
					mine.destroy();
					others.destroy();
					return (index == 0 && (startsInside)) || (index != 0);
				}
			}
			mine.destroy();
			others.destroy();
			return havePending;
		}
	}
/*
udanax-top.st:68661:IntegerRegion methodsFor: 'testing'!
{BooleanVar} intersects: region {XnRegion}
	| other {IntegerRegion wimpy} |
	other _ region cast: IntegerRegion.
	Int32Zero == myTransitionCount
		ifTrue: [myStartsInside ifTrue: [^other isEmpty not] ifFalse: [^self isEmpty not]]
		ifFalse:
			[Int32Zero == other transitionCount
				ifTrue: [other isBoundedBelow ifTrue: [^other isEmpty not] ifFalse: [^self isEmpty not]]
				ifFalse:
					[| mine {IntegerEdgeStepper} others {IntegerEdgeStepper} pending {IntegerVar} havePending {BooleanVar} index {Int32}
					    startsInside {BooleanVar} |
					mine _ self edgeStepper.
					others _ other edgeStepper.
					havePending _ false.
					index _ Int32Zero.
					[mine hasValue and: [others hasValue]] whileTrue:
						[ | me {IntegerVar} it {IntegerVar} |
						me _ mine edge.
						it _ others edge.
						me < it
							ifTrue:
								[others isEntering not ifTrue:
									[havePending ifTrue:
										[pending = me ifTrue: [havePending _ false]
													    ifFalse: [
													    		mine destroy.
													    		others destroy.
													    		^ true]]
									ifFalse:
										[havePending _ true.
										pending _ me.
										index _ 1]].
								mine step]
							ifFalse:
								[mine isEntering not ifTrue:
									[havePending ifTrue:
										[pending = it ifTrue: [havePending _ false]
													  ifFalse: [
													  	mine destroy.
													  	others destroy.
													  	^ true]]
									ifFalse:
										[havePending _ true.
										pending _ it.
										index _ 1]].
								others step]].
					startsInside _ myStartsInside and: [other isBoundedBelow not].
					(mine hasValue and: [others isEntering not]) ifTrue:
						[havePending ifTrue:
							[pending = mine edge ifTrue: [havePending _ false]
												   ifFalse: [
												   	mine destroy.
												   	others destroy.
												   	^ true]]
						ifFalse:
							[havePending _ true.
							index _ 1].
						havePending ifTrue: [
							mine destroy.
							others destroy.
							^ true].
						mine step.
						mine hasValue ifTrue:
							[mine destroy.
							others destroy.
							^ (index = Int32Zero and: [startsInside]) or: [index ~= Int32Zero]]].
					(others hasValue and: [mine isEntering not]) ifTrue:
						[havePending ifTrue:
							[pending = others edge ifTrue: [havePending _ false]
												     ifFalse: [
												     	mine destroy.
												     	others destroy.
												      ^ true]]
						ifFalse:
							[havePending _ true.
							index _ 1].
						havePending ifTrue: [
							mine destroy.
							others destroy.
							^ true].
						others step.
						others hasValue ifTrue:
							[mine destroy.
							others destroy.
							^ (index = Int32Zero and: [startsInside]) or: [index ~= Int32Zero]]].
					mine destroy.
					others destroy.
					^havePending]]!
*/
}
/**
 * Either I extend indefinitely to plus infinity, or I am bounded above, not both.
 * The empty region is bounded above despite the fact that it has no upper edge.
 */
public boolean isBoundedAbove() {
	return ((myTransitionCount & 1) == 0) != myStartsInside;
/*
udanax-top.st:68753:IntegerRegion methodsFor: 'testing'!
{BooleanVar CLIENT} isBoundedAbove
	"Either I extend indefinitely to plus infinity, or I am bounded above, not both. 
	The empty region is bounded above despite the fact that it has no upper edge."
	^((myTransitionCount bitAnd: 1) == Int32Zero) ~~ myStartsInside!
*/
}
/**
 * Either I extend indefinitely to minus infinity, or I am bounded below, not both.
 * The empty region is bounded below despite the fact that it has no lower bound.
 */
public boolean isBoundedBelow() {
	return ! myStartsInside;
/*
udanax-top.st:68759:IntegerRegion methodsFor: 'testing'!
{BooleanVar CLIENT INLINE} isBoundedBelow
	"Either I extend indefinitely to minus infinity, or I am bounded below, not both. 
	The empty region is bounded below despite the fact that it has no lower bound."
	^myStartsInside not!
*/
}
public boolean isEmpty() {
	return ! myStartsInside && (myTransitionCount == 0);
/*
udanax-top.st:68765:IntegerRegion methodsFor: 'testing'!
{BooleanVar} isEmpty
	^myStartsInside not and: [myTransitionCount == Int32Zero]!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof IntegerRegion) {
		IntegerRegion ir = (IntegerRegion) other;
		return ir.isBoundedBelow() != myStartsInside && (ir.transitionCount() == myTransitionCount && (ir.secretTransitions().elementsEqual(0, myTransitions, 0, myTransitionCount)));
	}
	else {
		return false;
	}
/*
udanax-top.st:68769:IntegerRegion methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: IntegerRegion into: [:ir |
			^ir isBoundedBelow ~~ myStartsInside
			 and: [ir transitionCount = myTransitionCount
			 and: [ir secretTransitions elementsEqual: Int32Zero
			 	with: myTransitions
			 	with: Int32Zero
			 	with: myTransitionCount]]]
		others: [^false].
	^ false "compiler fodder"!
*/
}
public boolean isFinite() {
	return isBoundedBelow() && (isBoundedAbove());
/*
udanax-top.st:68782:IntegerRegion methodsFor: 'testing'!
{BooleanVar} isFinite
	^self isBoundedBelow and: [self isBoundedAbove]!
*/
}
public boolean isFull() {
	return myStartsInside && (myTransitionCount == 0);
/*
udanax-top.st:68786:IntegerRegion methodsFor: 'testing'!
{BooleanVar} isFull	
	^myStartsInside and: [myTransitionCount == Int32Zero]!
*/
}
/**
 * Inequalities and intervals are both simple.  See class comment
 */
public boolean isSimple() {
	if (myStartsInside) {
		return myTransitionCount <= 1;
	}
	else {
		return myTransitionCount <= 2;
	}
/*
udanax-top.st:68790:IntegerRegion methodsFor: 'testing'!
{BooleanVar} isSimple
	"Inequalities and intervals are both simple.  See class comment"
	
	myStartsInside ifTrue:
		[^myTransitionCount <= 1]
	ifFalse:
		[^myTransitionCount <= 2]!
*/
}
public boolean isSubsetOf(XnRegion other) {
	if (other.isEmpty()) {
		return isEmpty();
	}
	else {
		IntegerEdgeStepper mine;
		IntegerEdgeStepper others;
		boolean result;
		mine = edgeStepper();
		others = ((IntegerRegion) other).edgeStepper();
		if ( ! (mine.hasValue() || (others.hasValue()))) {
			result = mine.isEntering() || ( ! others.isEntering());
			mine.destroy();
			others.destroy();
			return result;
		}
		if ( ! mine.isEntering() && (others.isEntering())) {
			mine.destroy();
			others.destroy();
			return false;
		}
		while (mine.hasValue() && (others.hasValue())) {
			if (others.edge() < mine.edge()) {
				if ( ! others.isEntering() && ( ! mine.isEntering())) {
					mine.destroy();
					others.destroy();
					return false;
				}
				others.step();
			}
			else {
				if (others.edge() > mine.edge()) {
					if (others.isEntering() && (mine.isEntering())) {
						mine.destroy();
						others.destroy();
						return false;
					}
					mine.step();
				}
				else {
					if (others.isEntering() != mine.isEntering()) {
						mine.destroy();
						others.destroy();
						return false;
					}
					others.step();
					mine.step();
				}
			}
		}
		result = ! ((mine.hasValue() && (others.isEntering())) || (others.hasValue() && ( ! mine.isEntering())));
		mine.destroy();
		others.destroy();
		return result;
	}
/*
udanax-top.st:68798:IntegerRegion methodsFor: 'testing'!
{BooleanVar} isSubsetOf: other {XnRegion} 
	other isEmpty
		ifTrue: [ ^ self isEmpty ]
		ifFalse:
			[| mine {IntegerEdgeStepper} others {IntegerEdgeStepper} result {BooleanVar} |
			mine _ self edgeStepper.
			others _ (other cast: IntegerRegion) edgeStepper.
			(mine hasValue or: [others hasValue])
				ifFalse: [
					result := mine isEntering or: [others isEntering not].
					mine destroy.
					others destroy.
					^ result].
			(mine isEntering not and: [others isEntering]) ifTrue: [mine destroy. others destroy. ^false].
			[mine hasValue and: [others hasValue]]
				whileTrue: 
					[others edge < mine edge
						ifTrue: 
							[(others isEntering not and: [mine isEntering not]) ifTrue: [mine destroy. others destroy. ^false].
							others step]
						ifFalse: 
							[others edge > mine edge
								ifTrue: 
									[(others isEntering and: [mine isEntering]) ifTrue: [mine destroy. others destroy. ^false].
									mine step]
								ifFalse: 
									[others isEntering ~~ mine isEntering ifTrue: [mine destroy. others destroy. ^false].
									others step.
									mine step]]].
			result := ((mine hasValue and: [others isEntering])
				or: [others hasValue and: [mine isEntering not]]) not.
			mine destroy.
			others destroy.
			^ result]!
*/
}
public XnRegion complement() {
	return new IntegerRegion( ! myStartsInside, myTransitionCount, myTransitions);
/*
udanax-top.st:68835:IntegerRegion methodsFor: 'operations'!
{XnRegion} complement
	^IntegerRegion create: myStartsInside not with: myTransitionCount with: myTransitions!
*/
}
public XnRegion intersect(XnRegion region) {
	IntegerRegion other;
	other = (IntegerRegion) region;
	if (0 == myTransitionCount) {
		if (myStartsInside) {
			return other;
		}
		else {
			return this;
		}
	}
	else {
		if (0 == other.transitionCount()) {
			if (other.isBoundedBelow()) {
				return other;
			}
			else {
				return this;
			}
		}
		else {
			IntegerEdgeStepper mine;
			IntegerEdgeStepper others;
			IntegerEdgeAccumulator result;
			XnRegion resultReg;
			mine = edgeStepper();
			others = other.edgeStepper();
			result = IntegerEdgeAccumulator.make((myStartsInside && ( ! other.isBoundedBelow())), (myTransitionCount + other.transitionCount()));
			while (mine.hasValue() && (others.hasValue())) {
				int me;
				int it;
				me = mine.edge();
				it = others.edge();
				if (me < it) {
					if ( ! others.isEntering()) {
						result.edge(me);
					}
					mine.step();
				}
				else {
					if ( ! mine.isEntering()) {
						result.edge(it);
					}
					others.step();
				}
			}
			if (mine.hasValue() && ( ! others.isEntering())) {
				result.edges(mine);
			}
			if (others.hasValue() && ( ! mine.isEntering())) {
				result.edges(others);
			}
			mine.destroy();
			others.destroy();
			resultReg = result.region();
			result.destroy();
			return resultReg;
		}
	}
/*
udanax-top.st:68839:IntegerRegion methodsFor: 'operations'!
{XnRegion} intersect: region {XnRegion}
	| other {IntegerRegion wimpy} |
	other _ region cast: IntegerRegion.
	Int32Zero == myTransitionCount
		ifTrue: [myStartsInside ifTrue: [^other] ifFalse: [^self]]
		ifFalse:
			[Int32Zero == other transitionCount
				ifTrue: [other isBoundedBelow ifTrue: [^other] ifFalse: [^self]]
				ifFalse:
					[ | mine {IntegerEdgeStepper} others {IntegerEdgeStepper} result {IntegerEdgeAccumulator} resultReg {XnRegion} |
					mine _ self edgeStepper.
					others _ other edgeStepper.
					result _ IntegerEdgeAccumulator
						make: (myStartsInside and: [other isBoundedBelow not])
						with: (myTransitionCount + other transitionCount).
					[mine hasValue and: [others hasValue]] whileTrue:
						[ | me {IntegerVar} it {IntegerVar} |
						me _ mine edge.
						it _ others edge.
						me < it
							ifTrue:
								[others isEntering not ifTrue: [result edge: me].
								mine step]
							ifFalse:
								[mine isEntering not ifTrue: [result edge: it].
								others step]].
					(mine hasValue and: [others isEntering not]) ifTrue: [result edges: mine].
					(others hasValue and: [mine isEntering not]) ifTrue: [result edges: others].
					mine destroy.
					others destroy.
					resultReg _ result region.
					result destroy.
					^resultReg]]!
*/
}
/**
 * The result is the smallest simple region which satisfies the spec in XuRegion::simpleUnion
 */
public XnRegion simpleUnion(XnRegion otherRegion) {
	IntegerRegion other;
	other = (IntegerRegion) otherRegion;
	if (isEmpty()) {
		return other.asSimpleRegion();
	}
	if (other.isEmpty()) {
		return asSimpleRegion();
	}
	if (isBoundedBelow() && (other.isBoundedBelow())) {
		if (isBoundedAbove() && (other.isBoundedAbove())) {
			return IntegerRegion.make((Math.min(start(), other.start())), (Math.max(stop(), other.stop())));
		}
		else {
			return IntegerRegion.after((Math.min(start(), other.start())));
		}
	}
	else {
		if (isBoundedAbove() && (other.isBoundedAbove())) {
			return IntegerRegion.before((Math.max(stop(), other.stop())));
		}
		else {
			return IntegerRegion.make().complement();
		}
	}
/*
udanax-top.st:68873:IntegerRegion methodsFor: 'operations'!
{XnRegion} simpleUnion: otherRegion {XnRegion}
	"The result is the smallest simple region which satisfies the spec in XuRegion::simpleUnion"
	
	| other {IntegerRegion wimpy} |
	other _ otherRegion cast: IntegerRegion.
	self isEmpty ifTrue: [^other asSimpleRegion].
	other isEmpty ifTrue: [^self asSimpleRegion].
	(self isBoundedBelow and: [other isBoundedBelow])
		ifTrue: [(self isBoundedAbove and: [other isBoundedAbove])
			ifTrue: [^IntegerRegion make: (self start min: other start)
				with: (self stop max: other stop)]
			ifFalse: [^IntegerRegion after: (self start min: other start)]]
		ifFalse: [(self isBoundedAbove and: [other isBoundedAbove])
			ifTrue: [^IntegerRegion before: (self stop max: other stop)]
			ifFalse: [^IntegerRegion make complement]]!
*/
}
public XnRegion unionWith(XnRegion region) {
	if (region.isEmpty()) {
		return this;
	}
	else {
		IntegerRegion other;
		IntegerEdgeStepper mine;
		IntegerEdgeStepper others;
		IntegerEdgeAccumulator result;
		XnRegion resultReg;
		other = (IntegerRegion) region;
		mine = edgeStepper();
		others = other.edgeStepper();
		result = IntegerEdgeAccumulator.make((myStartsInside || ( ! other.isBoundedBelow())), myTransitionCount + other.transitionCount());
		while (mine.hasValue() && (others.hasValue())) {
			int me;
			int him;
			me = mine.edge();
			him = others.edge();
			if (me < him) {
				if (others.isEntering()) {
					result.edge(me);
				}
				mine.step();
			}
			else {
				if (mine.isEntering()) {
					result.edge(him);
				}
				others.step();
			}
		}
		if (mine.hasValue() && (others.isEntering())) {
			result.edges(mine);
		}
		if (others.hasValue() && (mine.isEntering())) {
			result.edges(others);
		}
		mine.destroy();
		others.destroy();
		resultReg = result.region();
		result.destroy();
		return resultReg;
	}
/*
udanax-top.st:68889:IntegerRegion methodsFor: 'operations'!
{XnRegion} unionWith: region {XnRegion}
	region isEmpty
		ifTrue: [ ^ self ]
		ifFalse:
			[| other {IntegerRegion} 
			   mine {IntegerEdgeStepper} 
			   others {IntegerEdgeStepper} 
			   result {IntegerEdgeAccumulator}
			   resultReg {XnRegion} |
			   
			other _ region cast: IntegerRegion.
			mine _ self edgeStepper.
			others _ other edgeStepper.
			result _ IntegerEdgeAccumulator
				make: (myStartsInside or: [other isBoundedBelow not])
				with: myTransitionCount + other transitionCount.
			[mine hasValue and: [others hasValue]] whileTrue:
				[ | me {IntegerVar} him {IntegerVar} |
				me _ mine edge.
				him _ others edge.
				me < him
					ifTrue:
						[others isEntering ifTrue: [result edge: me].
						mine step]
					ifFalse:
						[mine isEntering ifTrue: [result edge: him].
						others step]].
			(mine hasValue and: [others isEntering]) ifTrue: [result edges: mine].
			(others hasValue and: [mine isEntering]) ifTrue: [result edges: others].
			mine destroy.
			others destroy.
			resultReg _ result region.
			result destroy.
			^ resultReg]!
*/
}
public XnRegion with(Position position) {
	return withInt(((IntegerPos) position).asIntegerVar());
/*
udanax-top.st:68925:IntegerRegion methodsFor: 'operations'!
{XnRegion} with: position {Position}
	^ self withInt: (position cast: IntegerPos) asIntegerVar!
*/
}
public XnRegion withInt(int pos) {
	IntegerEdgeStepper mine;
	/* TODO variable may not be initialized before being used */
	int me = 0;
	IntegerEdgeAccumulator result;
	XnRegion resultReg;
	if (isEmpty()) {
		return IntegerRegion.make(pos);
	}
	if (isBoundedAbove() && (pos == stop())) {
		IntegerVarArray newTransitions;
		newTransitions = (IntegerVarArray) myTransitions.copy();
		newTransitions.storeIntegerVar(myTransitionCount-1, pos + 1);
		return new IntegerRegion(myStartsInside, myTransitionCount, newTransitions);
	}
	mine = edgeStepper();
	result = IntegerEdgeAccumulator.make(myStartsInside, myTransitionCount + 2);
	while (mine.hasValue() && ((me = mine.edge()) < pos)) {
		result.edge(me);
		mine.step();
	}
	if (mine.isEntering()) {
		result.edge(pos);
		if (me == pos) {
			mine.step();
		}
		else {
			result.edge(pos + 1);
		}
	}
	else {
		if (me == pos) {
			mine.step();
			result.edge(pos + 1);
		}
	}
	if (mine.hasValue()) {
		result.edges(mine);
	}
	mine.destroy();
	resultReg = result.region();
	result.destroy();
	return resultReg;
/*
udanax-top.st:68929:IntegerRegion methodsFor: 'operations'!
{XnRegion} withInt: pos {IntegerVar}
	| mine {IntegerEdgeStepper} me {IntegerVar} result {IntegerEdgeAccumulator} resultReg {XnRegion} |
	self isEmpty ifTrue: [^IntegerRegion make: pos].
	(self isBoundedAbove and: [pos == self stop])
		ifTrue: [
			| newTransitions {IntegerVarArray} |
			newTransitions := myTransitions copy cast: IntegerVarArray.
			newTransitions at: myTransitionCount -1 storeIntegerVar: pos + 1.
			^ IntegerRegion create: myStartsInside with: myTransitionCount with: newTransitions].
	mine _ self edgeStepper.
	result _ IntegerEdgeAccumulator
		make: myStartsInside
		with: myTransitionCount + 2.
	[mine hasValue and: [(me _ mine edge) < pos]] whileTrue:
		[result edge: me.
		mine step].
	mine isEntering 
		ifTrue: 
			[result edge: pos.
			me == pos 
				ifTrue: [mine step]
				ifFalse: [result edge: pos+1]]
		ifFalse:
			[me == pos ifTrue: 
				[mine step.
				result edge: pos+1]].
	mine hasValue ifTrue: [result edges: mine].
	mine destroy.
	resultReg _ result region.
	result destroy.
	^ resultReg!
*/
}
public Stepper intervals() {
	return intervals(null);
/*
udanax-top.st:68964:IntegerRegion methodsFor: 'smalltalk: defaults'!
{Stepper CLIENT of: IntegerRegion} intervals
	^self intervals: NULL!
*/
}
public int count() {
	int result;
	if ( ! (isFinite())) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	result = 0;
	for (int i = 0; i < myTransitionCount; i += 2 ) {
		result = result + (myTransitions.integerVarAt(i + 1)) - (myTransitions.integerVarAt(i));
	}
	return result;
/*
udanax-top.st:68969:IntegerRegion methodsFor: 'enumerating'!
{IntegerVar} count
	| result {IntegerVar} |
	self isFinite ifFalse:
		[Heaper BLAST: #InvalidRequest].
	result _ IntegerVarZero.
	Int32Zero almostTo: myTransitionCount by: 2 do: [ :i {UInt32} |
		result _ result + (myTransitions integerVarAt: i + 1) - (myTransitions integerVarAt: i)].
	^result!
*/
}
/**
 * Essential. Break this into an ascending sequence of disjoint intervals (which may be
 * unbounded).
 */
public Stepper intervals(OrderSpec order) {
	return simpleRegions();
/*
udanax-top.st:68979:IntegerRegion methodsFor: 'enumerating'!
{Stepper INLINE CLIENT of: IntegerRegion} intervals: order {OrderSpec unused default: NULL}
	"Essential. Break this into an ascending sequence of disjoint intervals (which may be unbounded)."
	
	^self simpleRegions!
*/
}
/**
 * Actually uses the 'order' argument correctly to enumerate the
 * positions. Treats NULL the same as ascending. Iff I am bounded left
 * am I enumerable in ascending order. Similarly, only if I am bounded
 * right am I enumerable in descending order.
 */
public boolean isEnumerable(OrderSpec order) {
	if (order == null || (order.followsInt(1, 0))) {
		return isBoundedBelow();
	}
	else {
		return isBoundedAbove();
	}
/*
udanax-top.st:68984:IntegerRegion methodsFor: 'enumerating'!
{BooleanVar} isEnumerable: order {OrderSpec default: NULL}
	"Actually uses the 'order' argument correctly to enumerate the 
	positions. Treats NULL the same as ascending. Iff I am bounded left 
	am I enumerable in ascending order. Similarly, only if I am bounded 
	right am I enumerable in descending order."
	(order == NULL or: [order followsInt: 1 with: IntegerVar0])
		ifTrue: [^self isBoundedBelow]
		ifFalse: [^self isBoundedAbove]!
*/
}
/**
 * Whether this Region is a non-empty interval, i.e. if A, B in the Region and A <= C <= B
 * then C is in the Region. This includes inequalities (e.g. {x | x > 5}) and the fullRegion
 * in addition to ordinary two-ended intervals.
 */
public boolean isInterval() {
	return isSimple();
/*
udanax-top.st:68994:IntegerRegion methodsFor: 'enumerating'!
{BooleanVar CLIENT INLINE} isInterval
	"Whether this Region is a non-empty interval, i.e. if A, B in the Region and A <= C <= B then C is in the Region. This includes inequalities (e.g. {x | x > 5}) and the fullRegion in addition to ordinary two-ended intervals."
	
	^self isSimple!
*/
}
public ScruSet distinctions() {
	IntegerRegion intReg;
	if ( ! (isSimple())) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	if (isFull()) {
		return ImmuSet.make();
	}
	if (isEmpty() || (myTransitionCount == 1)) {
		return ImmuSet.make().with(this);
	}
	intReg = new IntegerRegion(myStartsInside, 1, myTransitions);
	return (ImmuSet.make().with(intReg)).with((IntegerRegion.before(stop())));
/*
udanax-top.st:69001:IntegerRegion methodsFor: 'breaking up'!
{ScruSet of: XnRegion} distinctions
	| intReg {IntegerRegion} |
	self isSimple ifFalse:
		[Heaper BLAST: #InvalidRequest].
	self isFull ifTrue:
		[^ImmuSet make].
	(self isEmpty or: [myTransitionCount = 1]) ifTrue:
		[^ImmuSet make with: self].
	intReg _ IntegerRegion create: myStartsInside with: 1 with: myTransitions.
	^(ImmuSet make with: intReg)
		with: (IntegerRegion before: self stop)!
*/
}
/**
 * Treats NULL the same as ascending. For the moment, will only work
 * with an ascending OrderSpec. If a descending OrderSpec is provided,
 * it will currently BLAST, but later will work correctly.
 * Returns a stepper on a disjoint set of simple regions in ascending
 * order.  No difference with disjointSimpleRegions
 */
public Stepper simpleRegions(OrderSpec order) {
	if ( ! (order == null || (order.followsInt(1, 0)))) {
		throw new UnimplementedException();
	}
	if (myTransitionCount == 0) {
		if (myStartsInside) {
			return Stepper.itemStepper(this);
		}
		else {
			return Stepper.emptyStepper();
		}
	}
	return new IntegerSimpleRegionStepper(myTransitions, myTransitionCount, ! myStartsInside);
/*
udanax-top.st:69014:IntegerRegion methodsFor: 'breaking up'!
{Stepper} simpleRegions: order {OrderSpec default: NULL} 
	"Treats NULL the same as ascending. For the moment, will only work 
	with an ascending OrderSpec. If a descending OrderSpec is provided, 
	it will currently BLAST, but later will work correctly.
	
	Returns a stepper on a disjoint set of simple regions in ascending 
	order.  No difference with disjointSimpleRegions"
	(order == NULL or: [order followsInt: 1 with: Int32Zero])
		ifFalse: [self unimplemented].
	myTransitionCount == Int32Zero ifTrue: [myStartsInside
			ifTrue: [^Stepper itemStepper: self]
			ifFalse: [^Stepper emptyStepper]].
	^IntegerSimpleRegionStepper
		create: myTransitions
		with: myTransitionCount
		with: myStartsInside not!
*/
}
/**
 * The actuall array. DO NOT MODIFY
 */
public IntegerVarArray secretTransitions() {
	return myTransitions;
/*
udanax-top.st:69034:IntegerRegion methodsFor: 'private:'!
{IntegerVarArray INLINE} secretTransitions
	"The actuall array. DO NOT MODIFY"
	
	^myTransitions!
*/
}
/**
 * the simple region at the given index in the transition array
 */
public IntegerRegion simpleRegionAtIndex(int i) {
	if ( ! (i < myTransitionCount)) {
		throw new AboraAssertionException();
	}
	if (((i & 1) == 0) == myStartsInside) {
		return IntegerRegion.before((myTransitions.integerVarAt(i)));
	}
	else {
		if (i + 1 < myTransitionCount) {
			return IntegerRegion.make((myTransitions.integerVarAt(i)), (myTransitions.integerVarAt(i + 1)));
		}
		else {
			return IntegerRegion.after((myTransitions.integerVarAt(i)));
		}
	}
/*
udanax-top.st:69039:IntegerRegion methodsFor: 'private:'!
{IntegerRegion} simpleRegionAtIndex: i {UInt32}
	"the simple region at the given index in the transition array"
	(i < myTransitionCount) assert.
	((i bitAnd: 1) = Int32Zero) == myStartsInside ifTrue:
		[^IntegerRegion before: (myTransitions integerVarAt: i)]
	ifFalse: [i + 1 < myTransitionCount ifTrue:
		[^IntegerRegion make: (myTransitions integerVarAt: i) with: (myTransitions integerVarAt: i + 1)]
	ifFalse:
		[^IntegerRegion after: (myTransitions integerVarAt: i)]]!
*/
}
/**
 * Do not send from outside the module. This should not be exported
 * outside the module, but to not export it in this case is some trouble.
 */
public IntegerEdgeStepper edgeStepper() {
	return IntegerEdgeStepper.make( ! myStartsInside, myTransitionCount, myTransitions);
/*
udanax-top.st:69051:IntegerRegion methodsFor: 'private: has friends'!
{IntegerEdgeStepper} edgeStepper
	"Do not send from outside the module. This should not be exported 
	outside the module, but to not export it in this case is some trouble."
	^IntegerEdgeStepper
		make: myStartsInside not
		with: myTransitionCount
		with: myTransitions!
*/
}
/**
 * Do not send from outside the module. This should not be exported
 * outside the module, but to not export it in this case is some trouble.
 * It is used for an efficiency hack in PointRegion.
 */
public int transitionCount() {
	return myTransitionCount;
/*
udanax-top.st:69060:IntegerRegion methodsFor: 'private: has friends'!
{UInt32 INLINE} transitionCount
	"Do not send from outside the module. This should not be exported 
	outside the module, but to not export it in this case is some trouble. 
	It is used for an efficiency hack in PointRegion."
	^myTransitionCount!
*/
}
public Position chooseOne(OrderSpec order) {
	return IntegerPos.make(((order == null || (order.followsInt(1, 0))) ? start() : stop()-1));
/*
udanax-top.st:69069:IntegerRegion methodsFor: 'smalltalk: passe'!
{Position} chooseOne: order {OrderSpec | NULL}
	^((order == NULL or: [order followsInt: 1 with: 0]) ifTrue: [self start]
		ifFalse: [self stop-1]) integer!
*/
}
/**
 * @deprecated
 */
public boolean startsInside() {
	throw new PasseException();
/*
udanax-top.st:69074:IntegerRegion methodsFor: 'smalltalk: passe'!
{BooleanVar} startsInside
	self passe!
*/
}
/**
 * Iff I am bounded left am I enumerable in ascending order. Similarly, only if I am bounded
 * right am I enumerable in descending order.
 */
public Stepper actualStepper(OrderSpec order) {
	if (order.followsInt(1, 0)) {
		return AscendingIntegerStepper.make(myTransitions, myTransitionCount);
	}
	else {
		return DescendingIntegerStepper.make(myTransitions, myTransitionCount);
	}
/*
udanax-top.st:69079:IntegerRegion methodsFor: 'protected: enumerating'!
{Stepper} actualStepper: order {OrderSpec default: NULL} 
	"Iff I am bounded left am I enumerable in ascending order. Similarly, only if I am bounded right am I enumerable in descending order."
	(order followsInt: 1 with: IntegerVar0) ifTrue: 
		[^AscendingIntegerStepper make: myTransitions with: myTransitionCount]
	ifFalse: 
		[^DescendingIntegerStepper make: myTransitions with: myTransitionCount]!
*/
}
public IntegerRegion(Rcvr receiver) {
	super(receiver);
	myStartsInside = receiver.receiveBooleanVar();
	myTransitionCount = receiver.receiveUInt32();
	myTransitions = (IntegerVarArray) receiver.receiveHeaper();
/*
udanax-top.st:69089:IntegerRegion methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myStartsInside _ receiver receiveBooleanVar.
	myTransitionCount _ receiver receiveUInt32.
	myTransitions _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendBooleanVar(myStartsInside);
	xmtr.sendUInt32(myTransitionCount);
	xmtr.sendHeaper(myTransitions);
/*
udanax-top.st:69095:IntegerRegion methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendBooleanVar: myStartsInside.
	xmtr sendUInt32: myTransitionCount.
	xmtr sendHeaper: myTransitions.!
*/
}
/**
 * Essential. Make a region that contains all integers greater than (or equal if inclusive is
 * true) to start.
 */
public static IntegerRegion above(int start, boolean inclusive) {
	int after;
	after = start;
	if ( ! (inclusive)) {
		after = after + 1;
	}
	return IntegerRegion.after(after);
/*
udanax-top.st:69117:IntegerRegion class methodsFor: 'pseudo constructors'!
{IntegerRegion} above: start {IntegerVar} with: inclusive {BooleanVar}
	"Essential. Make a region that contains all integers greater than (or equal if inclusive is true) to start."
	| after {IntegerVar} |
	after _ start.
	inclusive ifFalse: [after _ after + 1].
	^IntegerRegion after: after!
*/
}
/**
 * The region containing all position greater than or equal to start
 */
public static IntegerRegion after(int start) {
	IntegerVarArray table;
	IntegerRegion tir;
	if (LastAfterStart == start) {
		return LastAfterRegion;
	}
	LastAfterStart = start;
	table = IntegerVarArray.zeros(1);
	table.storeIntegerVar(0, start);
	/* temp used to get around static member problem in INIT macro - heh 10 January 1992 */
	tir = new IntegerRegion(false, 1, table);
	LastAfterRegion = tir;
	return LastAfterRegion;
/*
udanax-top.st:69124:IntegerRegion class methodsFor: 'pseudo constructors'!
{IntegerRegion} after: start {IntegerVar}
	"The region containing all position greater than or equal to start"
	| table {IntegerVarArray} tir {IntegerRegion} |
	LastAfterStart = start ifTrue:
		[^LastAfterRegion].
	LastAfterStart _ start.
	table _ IntegerVarArray zeros: 1.
	table at: Int32Zero storeIntegerVar: start.
	"temp used to get around static member problem in INIT macro - heh 10 January 1992"
	tir _ IntegerRegion create: false with: 1 with: table.
	LastAfterRegion _ tir.
	^LastAfterRegion.!
*/
}
/**
 * The full region of this space
 */
public static IntegerRegion allIntegers() {
	return AllIntegers;
/*
udanax-top.st:69138:IntegerRegion class methodsFor: 'pseudo constructors'!
{IntegerRegion INLINE} allIntegers
	"The full region of this space"
	
	^AllIntegers!
*/
}
/**
 * The region of all integers less than end.  Does not include end.
 */
public static IntegerRegion before(int end) {
	IntegerVarArray table;
	IntegerRegion tir;
	if (LastBeforeEnd == end) {
		return LastBeforeRegion;
	}
	LastBeforeEnd = end;
	table = IntegerVarArray.zeros(1);
	table.storeIntegerVar(0, end);
	/* temp used to get around problem with static members & INIT macro - heh 10 January 1992 */
	tir = new IntegerRegion(true, 1, table);
	LastBeforeRegion = tir;
	return LastBeforeRegion;
/*
udanax-top.st:69143:IntegerRegion class methodsFor: 'pseudo constructors'!
{IntegerRegion} before: end {IntegerVar}
	"The region of all integers less than end.  Does not include end."
	
	| table {IntegerVarArray} tir {IntegerRegion} |
	LastBeforeEnd = end ifTrue:
		[^LastBeforeRegion].
	LastBeforeEnd _ end.
	table _ IntegerVarArray zeros: 1.
	table at: Int32Zero storeIntegerVar: end.
	"temp used to get around problem with static members & INIT macro - heh 10 January 1992"
	tir _ IntegerRegion create: true with: 1 with: table.
	LastBeforeRegion _ tir.
	^LastBeforeRegion!
*/
}
/**
 * Make a region that contains all integers less than (or equal if inclusive is true) to
 * stop.
 */
public static IntegerRegion below(int stop, boolean inclusive) {
	int after;
	after = stop;
	if (inclusive) {
		after = after + 1;
	}
	return IntegerRegion.after(after);
/*
udanax-top.st:69157:IntegerRegion class methodsFor: 'pseudo constructors'!
{IntegerRegion} below: stop {IntegerVar} with: inclusive {BooleanVar}
	"Make a region that contains all integers less than (or equal if inclusive is true) to stop."
	| after {IntegerVar} |
	after _ stop.
	inclusive ifTrue: [after _ after + 1].
	^IntegerRegion after: after!
*/
}
/**
 * The region of all integers which are >= start and < start + n
 */
public static IntegerRegion integerExtent(int start, int n) {
	return make(start, start + n);
/*
udanax-top.st:69164:IntegerRegion class methodsFor: 'pseudo constructors'!
{IntegerRegion} integerExtent: start {IntegerVar} with: n {IntegerVar}
	"The region of all integers which are >= start and < start + n"
	
	^self make: start with: start + n!
*/
}
/**
 * The region of all integers which are >= left and < right
 */
public static IntegerRegion interval(int left, int right) {
	IntegerVarArray ivArray;
	if (left >= right) {
		return EmptyIntegerRegion;
	}
	ivArray = IntegerVarArray.zeros(2);
	ivArray.storeIntegerVar(0, left);
	ivArray.storeIntegerVar(1, right);
	return new IntegerRegion(false, 2, ivArray);
/*
udanax-top.st:69169:IntegerRegion class methodsFor: 'pseudo constructors'!
{IntegerRegion} interval: left {IntegerVar} with: right {IntegerVar}
	"The region of all integers which are >= left and < right"
	
	| ivArray {IntegerVarArray} |
	left >= right ifTrue: [^EmptyIntegerRegion].
	ivArray _ IntegerVarArray zeros: 2.
	ivArray at: Int32Zero storeIntegerVar: left.
	ivArray at: 1 storeIntegerVar: right.
	^IntegerRegion create: false with: 2 with: ivArray!
*/
}
/**
 * No integers, the empty region
 */
public static IntegerRegion make() {
	return EmptyIntegerRegion;
/*
udanax-top.st:69179:IntegerRegion class methodsFor: 'pseudo constructors'!
{IntegerRegion INLINE} make
	"No integers, the empty region"
	
	^EmptyIntegerRegion!
*/
}
/**
 * The region with just this one position.  Equivalent to using a converter
 * to convert this position to a region.
 */
public static IntegerRegion make(int singleton) {
	IntegerVarArray table;
	IntegerRegion tir;
	if (singleton == LastSingleton) {
		return LastSingletonRegion;
	}
	LastSingleton = singleton;
	table = IntegerVarArray.zeros(2);
	table.storeIntegerVar(0, singleton);
	table.storeIntegerVar(1, singleton + 1);
	/* temp used to get around problem with static members and INIT macro - heh 10 January 1992 */
	tir = new IntegerRegion(false, 2, table);
	LastSingletonRegion = tir;
	return LastSingletonRegion;
/*
udanax-top.st:69184:IntegerRegion class methodsFor: 'pseudo constructors'!
make: singleton {IntegerVar}
	"The region with just this one position.  Equivalent to using a converter 
	to convert this position to a region."
	
	| table {IntegerVarArray} tir {IntegerRegion} |
	singleton = LastSingleton ifTrue:
		[^LastSingletonRegion].
	LastSingleton _ singleton.
	table _ IntegerVarArray zeros: 2.
	table at: Int32Zero storeIntegerVar: singleton.
	table at: 1 storeIntegerVar: singleton + 1.
	"temp used to get around problem with static members and INIT macro - heh 10 January 1992"
	tir _ IntegerRegion create: false with: 2 with: table.
	LastSingletonRegion _ tir.
	^LastSingletonRegion!
*/
}
/**
 * The region of all integers which are >= left and < right
 */
public static IntegerRegion make(int left, int right) {
	IntegerVarArray ivArray;
	if (left >= right) {
		return EmptyIntegerRegion;
	}
	ivArray = IntegerVarArray.zeros(2);
	ivArray.storeIntegerVar(0, left);
	ivArray.storeIntegerVar(1, right);
	return new IntegerRegion(false, 2, ivArray);
/*
udanax-top.st:69200:IntegerRegion class methodsFor: 'pseudo constructors'!
make: left {IntegerVar} with: right {IntegerVar}
	"The region of all integers which are >= left and < right"
	
	| ivArray {IntegerVarArray} |
	left >= right ifTrue: [^EmptyIntegerRegion].
	ivArray _ IntegerVarArray zeros: 2.
	ivArray at: Int32Zero storeIntegerVar: left.
	ivArray at: 1 storeIntegerVar: right.
	^IntegerRegion create: false with: 2 with: ivArray!
*/
}
public static void initTimeNonInherited() {
	IntegerVarArray empty;
	IntegerRegion tir;
	empty = IntegerVarArray.zeros(1);
	/* temp used to get around problem with static members and INIT macro - heh 10 January 1992 */
	tir = new IntegerRegion(true, 0, empty);
	AllIntegers = tir;
	tir = new IntegerRegion(false, 0, empty);
	EmptyIntegerRegion = tir;
	/* call the pseudo constructors with arguments that are known to flush the caches */
	IntegerRegion.after(0);
	IntegerRegion.before(0);
	IntegerRegion.make(0);
	IntegerRegion.make(0, 2);
/*
udanax-top.st:69212:IntegerRegion class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	| empty {IntegerVarArray} tir {IntegerRegion} |
	self REQUIRES: IntegerVarArray.
	empty _ IntegerVarArray zeros: 1.
	
	"temp used to get around problem with static members and INIT macro - heh 10 January 1992"
	tir _ IntegerRegion create: true with: Int32Zero with: empty.
	AllIntegers _ tir.
	tir _ IntegerRegion create: false with: Int32Zero with: empty.
	EmptyIntegerRegion _ tir.
	"call the pseudo constructors with arguments that are known to flush the caches"
	IntegerRegion after: IntegerVar0.
	IntegerRegion before: IntegerVar0.
	IntegerRegion make: IntegerVar0.
	IntegerRegion make: IntegerVar0 with: 2!
*/
}
public static void linkTimeNonInherited() {
	AllIntegers = null;
	EmptyIntegerRegion = null;
	LastAfterRegion = null;
	LastAfterStart = 13;
	LastBeforeEnd = 13;
	LastBeforeRegion = null;
	LastInterval = null;
	LastLeft = 13;
	LastRight = 13;
	LastSingleton = 13;
	LastSingletonRegion = null;
/*
udanax-top.st:69228:IntegerRegion class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	AllIntegers _ NULL.
	EmptyIntegerRegion _ NULL.
	LastAfterRegion _ NULL.
	LastAfterStart _ 13.
	LastBeforeEnd _ 13.
	LastBeforeRegion _ NULL.
	LastInterval _ NULL.
	LastLeft _ 13.
	LastRight _ 13.
	LastSingleton _ 13.
	LastSingletonRegion _ NULL.!
*/
}
/**
 * used for an efficiency hack in PointRegion.  Don't use.
 */
public static IntegerVarArray badlyViolatePrivacyOfIntegerRegionTransitions(IntegerRegion reg) {
	return reg.secretTransitions();
/*
udanax-top.st:69243:IntegerRegion class methodsFor: 'privacy violator'!
{IntegerVarArray INLINE} badlyViolatePrivacyOfIntegerRegionTransitions: reg {IntegerRegion} 
	"used for an efficiency hack in PointRegion.  Don't use."
	^reg secretTransitions!
*/
}
public static IntegerRegion usingx(boolean startsInside, int transitionCount, IntegerVarArray transitions) {
	return new IntegerRegion(startsInside, transitionCount, transitions);
/*
udanax-top.st:69249:IntegerRegion class methodsFor: 'private: pseudo constructors'!
{IntegerRegion} usingx: startsInside {BooleanVar}
	with: transitionCount {Int32}
	with: transitions {IntegerVarArray}
	^self create: startsInside with: transitionCount with: transitions!
*/
}
/**
 * {Stepper CLIENT of: RealRegion} intervals: order {OrderSpec default: NULL}
 * {BooleanVar CLIENT} isBoundedAbove
 * {BooleanVar CLIENT} isBoundedBelow
 * {BooleanVar CLIENT} isInterval
 * {IntegerVar CLIENT} start
 * {IntegerVar CLIENT} stop
 */
public static void infostProtocol() {
/*
udanax-top.st:69257:IntegerRegion class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Stepper CLIENT of: RealRegion} intervals: order {OrderSpec default: NULL}
{BooleanVar CLIENT} isBoundedAbove
{BooleanVar CLIENT} isBoundedBelow
{BooleanVar CLIENT} isInterval
{IntegerVar CLIENT} start
{IntegerVar CLIENT} stop
"!
*/
}
public IntegerRegion() {
/*

Generated during transformation
*/
}
}
