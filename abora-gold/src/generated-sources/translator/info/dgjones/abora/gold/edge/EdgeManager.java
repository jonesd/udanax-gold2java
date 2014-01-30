/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.edge;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.edge.EdgeManager;
import info.dgjones.abora.gold.edgeregion.EdgeAccumulator;
import info.dgjones.abora.gold.edgeregion.EdgeSimpleRegionStepper;
import info.dgjones.abora.gold.edgeregion.EdgeStepper;
import info.dgjones.abora.gold.edgeregion.TransitionEdge;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Manages the common code for regions which are represented as a sequence of
 * EdgeTransitions. Each coordinate space should define a subclass which implements the
 * appropriate methods, and then use it to do the various region operations. Clients of the
 * region do not need to see any of these classes.
 */
public class EdgeManager extends Heaper {

/*
udanax-top.st:18173:
Heaper subclass: #EdgeManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-edge'!
*/
/*
udanax-top.st:18177:
EdgeManager comment:
'Manages the common code for regions which are represented as a sequence of EdgeTransitions. Each coordinate space should define a subclass which implements the appropriate methods, and then use it to do the various region operations. Clients of the region do not need to see any of these classes.'!
*/
/*
udanax-top.st:18179:
(EdgeManager getOrMakeCxxClassDescription)
	friends:
'friend class EdgeSimpleRegionStepper;
friend class EdgeAccumulator;
';
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EdgeManager.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Create an accumulator which takes edges and creates a region
 */
public EdgeAccumulator edgeAccumulator(boolean startsInside) {
	return EdgeAccumulator.make(this, startsInside);
/*
udanax-top.st:18188:EdgeManager methodsFor: 'private:'!
{EdgeAccumulator} edgeAccumulator: startsInside {BooleanVar}
	"Create an accumulator which takes edges and creates a region"
	
	^EdgeAccumulator
		make: self
		with: startsInside!
*/
}
/**
 * Create a stepper for iterating through the edges of the region
 */
public EdgeStepper edgeStepper(XnRegion region) {
	return EdgeStepper.make( ! (startsInside(region)), (transitions(region)), (transitionsCount(region)));
/*
udanax-top.st:18195:EdgeManager methodsFor: 'private:'!
{EdgeStepper} edgeStepper: region {XnRegion}
	"Create a stepper for iterating through the edges of the region"
	
	^EdgeStepper make: (self startsInside: region) not
		with: (self transitions: region)
		with: (self transitionsCount: region)!
*/
}
public TransitionEdge lowerEdge(XnRegion region) {
	PtrArray transitions;
	transitions = transitions(region);
	if ((startsInside(region)) || ((transitionsCount(region)) == 0)) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	return (TransitionEdge) (transitions.fetch(0));
/*
udanax-top.st:18202:EdgeManager methodsFor: 'private:'!
{TransitionEdge} lowerEdge: region {XnRegion}
	| transitions {PtrArray of: TransitionEdge} |
	transitions := self transitions: region.
	((self startsInside: region)
			or: [(self transitionsCount: region) = Int32Zero])
		ifTrue: [Heaper BLAST: #InvalidRequest].
	^(transitions fetch: Int32Zero) cast: TransitionEdge!
*/
}
/**
 * Create a stepper for iterating through the edges of the region
 */
public EdgeStepper singleEdgeStepper(Position pos) {
	return EdgeStepper.make( ! false, (posTransitions(pos)));
/*
udanax-top.st:18211:EdgeManager methodsFor: 'private:'!
{EdgeStepper} singleEdgeStepper: pos {Position}
	"Create a stepper for iterating through the edges of the region"
	
	^EdgeStepper make: false not
		with: (self posTransitions: pos)!
*/
}
public TransitionEdge upperEdge(XnRegion region) {
	PtrArray transitions;
	int transitionsCount;
	transitions = transitions(region);
	transitionsCount = transitionsCount(region);
	if ( ! (isBoundedRight(region)) || (transitionsCount == 0)) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	return (TransitionEdge) (transitions.fetch((transitionsCount - 1)));
/*
udanax-top.st:18217:EdgeManager methodsFor: 'private:'!
{TransitionEdge} upperEdge: region {XnRegion}
	| transitions {PtrArray of: TransitionEdge} transitionsCount {Int32} |
	transitions := self transitions: region.
	transitionsCount := self transitionsCount: region.
	((self isBoundedRight: region) not
			or: [transitionsCount == Int32Zero])
		ifTrue: [Heaper BLAST: #InvalidRequest].
	^(transitions fetch: (transitionsCount - 1)) cast: TransitionEdge!
*/
}
public boolean hasMember(XnRegion region, Position pos) {
	EdgeStepper edges;
	TransitionEdge edge;
	boolean result;
	edges = edgeStepper(region);
	while ((edge = (TransitionEdge) edges.fetch()) != null) {
		if (edge.follows(pos)) {
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
udanax-top.st:18229:EdgeManager methodsFor: 'testing'!
{BooleanVar} hasMember: region {XnRegion} with: pos {Position}
	| edges {EdgeStepper} edge {TransitionEdge} result {BooleanVar} |
	edges := self edgeStepper: region.
	[(edge := edges fetch cast: TransitionEdge) ~~ NULL] whileTrue:
		[(edge follows: pos) ifTrue:
			[result := edges isEntering not.
			edges destroy.
			^ result].
		edges step].
	result := edges isEntering not.
	edges destroy.
	^ result!
*/
}
/**
 * Same meaning as IntegerRegion::isBoundedLeft
 */
public boolean isBoundedLeft(XnRegion region) {
	return ! (startsInside(region));
/*
udanax-top.st:18243:EdgeManager methodsFor: 'testing'!
{BooleanVar} isBoundedLeft: region {XnRegion}
	"Same meaning as IntegerRegion::isBoundedLeft"
	
	^(self startsInside: region) not!
*/
}
/**
 * Same meaning as IntegerRegion::isBoundedRight
 */
public boolean isBoundedRight(XnRegion region) {
	return (((transitionsCount(region)) & 1) == 0) != (startsInside(region));
/*
udanax-top.st:18248:EdgeManager methodsFor: 'testing'!
{BooleanVar} isBoundedRight: region {XnRegion}
	"Same meaning as IntegerRegion::isBoundedRight"
	
	^(((self transitionsCount: region) bitAnd: 1) == Int32Zero)
		~~ (self startsInside: region)!
*/
}
public boolean isEmpty(XnRegion region) {
	return ! (startsInside(region)) && ((transitionsCount(region)) == 0);
/*
udanax-top.st:18254:EdgeManager methodsFor: 'testing'!
{BooleanVar} isEmpty: region {XnRegion}
	^(self startsInside: region) not
		and: [(self transitionsCount: region) = Int32Zero]!
*/
}
/**
 * Here is one place where the *infinite* of the infinite divisibility assumed by
 * OrderedRegion about the full ordering comes in (see class comment).
 * An interval whose left edge is not the same as the right edge is assumed to contain an
 * infinite number of positions
 */
public boolean isFinite(XnRegion region) {
	PtrArray transitions;
	int transitionsCount;
	transitions = transitions(region);
	transitionsCount = transitionsCount(region);
	if ((startsInside(region)) || ((transitionsCount & 1) != 0)) {
		return false;
	}
	for (int i = 0; i < transitionsCount; i += 2 ) {
		if ( ! (((TransitionEdge) (transitions.fetch(i))).isFollowedBy(((TransitionEdge) (transitions.fetch(i + 1)))))) {
			return false;
		}
	}
	return true;
/*
udanax-top.st:18259:EdgeManager methodsFor: 'testing'!
{BooleanVar} isFinite: region {XnRegion}
	"Here is one place where the *infinite* of the infinite divisibility assumed by OrderedRegion about the full ordering comes in (see class comment).  
	An interval whose left edge is not the same as the right edge is assumed to contain an infinite number of positions"
	
	| transitions {PtrArray of: TransitionEdge} transitionsCount {Int32} |
	transitions := self transitions: region.
	transitionsCount := self transitionsCount: region.
	((self startsInside: region) or: [(transitionsCount bitAnd: 1) ~~ Int32Zero]) ifTrue:
		[^false].
	Int32Zero almostTo: transitionsCount by: 2 do: [ :i {Int32} |
		(((transitions fetch: i) cast: TransitionEdge)
				isFollowedBy: ((transitions fetch: i + 1) cast: TransitionEdge))
			ifFalse: [^false]].
	^true!
*/
}
public boolean isFull(XnRegion region) {
	return (startsInside(region)) && ((transitionsCount(region)) == 0);
/*
udanax-top.st:18274:EdgeManager methodsFor: 'testing'!
{BooleanVar} isFull: region {XnRegion}
	^(self startsInside: region)
		and: [(self transitionsCount: region) = Int32Zero]!
*/
}
public boolean isSimple(XnRegion region) {
	int testVal;
	if (startsInside(region)) {
		testVal = 1;
	}
	else {
		testVal = 2;
	}
	return (transitionsCount(region)) <= testVal;
/*
udanax-top.st:18279:EdgeManager methodsFor: 'testing'!
{BooleanVar} isSimple: region {XnRegion}
	| testVal {Int32} |
	(self startsInside: region) ifTrue: [testVal _ 1] ifFalse: [testVal _ 2].
	^(self transitionsCount: region) <= testVal!
*/
}
public boolean isSubsetOf(XnRegion me, XnRegion other) {
	EdgeStepper mine;
	EdgeStepper others;
	boolean result;
	if (isEmpty(other)) {
		return isEmpty(me);
	}
	mine = edgeStepper(me);
	others = edgeStepper(other);
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
		if ( ! (others.getEdge().isGE(mine.getEdge()))) {
			if ( ! others.isEntering() && ( ! mine.isEntering())) {
				mine.destroy();
				others.destroy();
				return false;
			}
			others.step();
		}
		else {
			if ( ! (mine.getEdge().isGE(others.getEdge()))) {
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
/*
udanax-top.st:18284:EdgeManager methodsFor: 'testing'!
{BooleanVar} isSubsetOf: me {XnRegion} with: other {XnRegion}
	| mine {EdgeStepper} others {EdgeStepper} result {BooleanVar} |
	(self isEmpty: other)
		ifTrue: [^self isEmpty: me].
	mine := self edgeStepper: me.
	others := self edgeStepper: other.
	(mine hasValue or: [others hasValue])
		ifFalse: [
			result := mine isEntering or: [others isEntering not].
			mine destroy.
			others destroy.
			^ result].
	(mine isEntering not and: [others isEntering]) ifTrue: [mine destroy. others destroy. ^false].
	[mine hasValue and: [others hasValue]]
		whileTrue: 
			[(others getEdge isGE: mine getEdge) not
				ifTrue: 
					[(others isEntering not and: [mine isEntering not]) ifTrue: [mine destroy. others destroy. ^false].
					others step]
				ifFalse: 
					[(mine getEdge isGE: others getEdge) not
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
	^ result!
*/
}
/**
 * Because Edge Regions should only be used on infinitely divisible spaces (like rationals),
 * if it's finite then it is bounded on both sides, and all the internal intervals are
 * singletons
 */
public int count(XnRegion region) {
	if ( ! (isFinite(region))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_FINITE);
	}
	return (transitionsCount(region)) / 2;
/*
udanax-top.st:18321:EdgeManager methodsFor: 'enumerating'!
{IntegerVar} count: region {XnRegion}
	"Because Edge Regions should only be used on infinitely divisible spaces (like rationals), if it's finite then it is bounded on both sides, and all the internal intervals are singletons"
	(self isFinite: region) ifFalse: [Heaper BLAST: #MustBeFinite].
	^(self transitionsCount: region) // 2!
*/
}
public XnRegion asSimpleRegion(XnRegion region) {
	if (isSimple(region)) {
		return region;
	}
	if (isBoundedLeft(region)) {
		if (isBoundedRight(region)) {
			return makeNew(false, ((PtrArray) (PrimSpec.pointer().arrayWithTwo((lowerEdge(region)), (upperEdge(region))))));
		}
		else {
			return makeNew(false, ((PtrArray) (PrimSpec.pointer().arrayWith((lowerEdge(region))))));
		}
	}
	else {
		if (isBoundedRight(region)) {
			return makeNew(true, ((PtrArray) (PrimSpec.pointer().arrayWith((upperEdge(region))))));
		}
		else {
			return makeNew(true, PtrArray.empty());
		}
	}
/*
udanax-top.st:18329:EdgeManager methodsFor: 'accessing'!
{XnRegion} asSimpleRegion: region {XnRegion}
	(self isSimple: region) ifTrue: [^region].
	(self isBoundedLeft: region)
		ifTrue: [(self isBoundedRight: region)
			ifTrue: [^self makeNew: false with: ((PrimSpec pointer
				arrayWithTwo: (self lowerEdge: region) with: (self upperEdge: region)) cast: PtrArray)]
			ifFalse: [^self makeNew: false with: ((PrimSpec pointer
				arrayWith: (self lowerEdge: region)) cast: PtrArray)]]
		ifFalse: [(self isBoundedRight: region)
			ifTrue: [^self makeNew: true with: ((PrimSpec pointer
				arrayWith: (self upperEdge: region)) cast: PtrArray)]
			ifFalse: [^self makeNew: true with: PtrArray empty]]!
*/
}
/**
 * The largest position such that no other positions in the region are any less than it. In
 * other words, this is the lower bounding element. We choose to avoid the terms 'lowerBound'
 * and 'upperBound' as their meanings in IntegerRegion are significantly different. Here,
 * both 'all numbers >= 3' and 'all numbers > 3' have a 'greatestLowerBound' of 3 even though
 * the latter doesn't include 3. To tell whether a bound is included, good old 'hasMember'
 * should do a fine job.
 */
public Position greatestLowerBound(XnRegion region) {
	return edgePosition((lowerEdge(region)));
/*
udanax-top.st:18343:EdgeManager methodsFor: 'accessing'!
{Position} greatestLowerBound: region {XnRegion}
	"The largest position such that no other positions in the region are any less than it. In other words, this is the lower bounding element. We choose to avoid the terms 'lowerBound' and 'upperBound' as their meanings in IntegerRegion are significantly different. Here, both 'all numbers >= 3' and 'all numbers > 3' have a 'greatestLowerBound' of 3 even though the latter doesn't include 3. To tell whether a bound is included, good old 'hasMember' should do a fine job."
	^self edgePosition: (self lowerEdge: region)!
*/
}
/**
 * The smallest position such that no other positions in the region are
 * any greater than it. In other words, this is the upper bounding element.
 * We choose to avoid the terms 'lowerBound' and 'upperBound' as
 * their meanings in IntegerRegion are significantly different. Here, both
 * 'all numbers <= 3' and 'all numbers < 3' have a 'leastUpperBound'
 * of 3 even though the latter doesn't include 3. To tell whether a
 * bound is included, good old 'hasMember' should do a fine job.
 */
public Position leastUpperBound(XnRegion region) {
	return edgePosition((upperEdge(region)));
/*
udanax-top.st:18348:EdgeManager methodsFor: 'accessing'!
{Position} leastUpperBound: region {XnRegion}
	"The smallest position such that no other positions in the region are 
	any greater than it. In other words, this is the upper bounding element. 
	We choose to avoid the terms 'lowerBound' and 'upperBound' as 
	their meanings in IntegerRegion are significantly different. Here, both 
	'all numbers <= 3' and 'all numbers < 3' have a 'leastUpperBound' 
	of 3 even though the latter doesn't include 3. To tell whether a 
	bound is included, good old 'hasMember' should do a fine job."
	^self edgePosition: (self upperEdge: region)!
*/
}
public XnRegion simpleUnion(XnRegion me, XnRegion other) {
	if (isEmpty(me)) {
		return asSimpleRegion(other);
	}
	if (isEmpty(other)) {
		return asSimpleRegion(me);
	}
	if ((isBoundedLeft(me)) && (isBoundedLeft(other))) {
		if ((isBoundedRight(me)) && (isBoundedRight(other))) {
			return makeNew(false, ((PtrArray) (PrimSpec.pointer().arrayWithTwo(((lowerEdge(me)).floor((lowerEdge(other)))), ((upperEdge(me)).ceiling((upperEdge(other))))))));
		}
		else {
			return makeNew(false, ((PtrArray) (PrimSpec.pointer().arrayWith(((lowerEdge(me)).floor((lowerEdge(other))))))));
		}
	}
	else {
		if ((isBoundedRight(me)) && (isBoundedRight(other))) {
			return makeNew(true, ((PtrArray) (PrimSpec.pointer().arrayWith(((upperEdge(me)).ceiling((upperEdge(other))))))));
		}
		else {
			return makeNew(true, PtrArray.empty());
		}
	}
/*
udanax-top.st:18359:EdgeManager methodsFor: 'accessing'!
{XnRegion} simpleUnion: me {XnRegion} with: other {XnRegion}
	(self isEmpty: me) ifTrue: [^self asSimpleRegion: other].
	(self isEmpty: other) ifTrue: [^self asSimpleRegion: me].
	((self isBoundedLeft: me) and: [self isBoundedLeft: other])
		ifTrue: [((self isBoundedRight: me) and: [self isBoundedRight: other])
			ifTrue: [^self makeNew: false with: ((PrimSpec pointer
				arrayWithTwo: ((self lowerEdge: me) floor: (self lowerEdge: other))
				with: ((self upperEdge: me) ceiling: (self upperEdge: other))) cast: PtrArray)]
			ifFalse: [^self makeNew: false with: ((PrimSpec pointer arrayWith: ((self lowerEdge: me) floor: (self lowerEdge: other))) cast: PtrArray)]]
		ifFalse: [((self isBoundedRight: me) and: [self isBoundedRight: other])
			ifTrue: [^self makeNew: true with: ((PrimSpec pointer arrayWith: ((self upperEdge: me) ceiling: (self upperEdge: other))) cast: PtrArray)]
			ifFalse: [^self makeNew: true with: PtrArray empty]]!
*/
}
public void printRegionOn(XnRegion region, PrintWriter oo) {
	if (isEmpty(region)) {
		oo.print("{}");
	}
	else {
		EdgeStepper edges;
		TransitionEdge previous;
		edges = edgeStepper(region);
		if ( ! (isSimple(region))) {
			oo.print("{");
		}
		if ( ! (edges.isEntering())) {
			oo.print("(-inf");
		}
		previous = null;
		Stepper stomper = edges;
		for (; stomper.hasValue(); stomper.step()) {
			TransitionEdge edge = (TransitionEdge) stomper.fetch();
			if (edge == null) {
				continue ;
			}
			edge.printTransitionOn(oo, edges.isEntering(), (previous != null && (previous.touches(edge))));
			previous = edge;
		}
		stomper.destroy();
		if ( ! (isBoundedRight(region))) {
			oo.print(" +inf)");
		}
		if ( ! (isSimple(region))) {
			oo.print("}");
		}
	}
/*
udanax-top.st:18375:EdgeManager methodsFor: 'printing'!
{void} printRegionOn: region {XnRegion} with: oo {ostream reference}
	(self isEmpty: region) ifTrue:
		[oo << '{}']
	ifFalse:
		[ | edges {EdgeStepper} previous {TransitionEdge} |
		edges := self edgeStepper: region.
		(self isSimple: region) ifFalse: [oo << '{'].
		edges isEntering ifFalse: [oo << '(-inf'].
		previous := NULL.
		edges forEach: [ :edge {TransitionEdge} |
			edge printTransitionOn: oo
				with: edges isEntering
				with: (previous ~~ NULL and: [previous touches: edge]).
			previous := edge].
		(self isBoundedRight: region) ifFalse: [oo << ' +inf)'].
		(self isSimple: region) ifFalse: [oo << '}']]!
*/
}
public XnRegion complement(XnRegion region) {
	return makeNew( ! (startsInside(region)), (transitions(region)), (transitionsCount(region)));
/*
udanax-top.st:18395:EdgeManager methodsFor: 'operations'!
{XnRegion} complement: region {XnRegion}
	^self makeNew: (self startsInside: region) not with: (self transitions: region) with: (self transitionsCount: region)!
*/
}
public ScruSet distinctions(XnRegion region) {
	MuSet result;
	if ( ! (isSimple(region))) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	if (isEmpty(region)) {
		return ImmuSet.make().with(region);
	}
	if (isFull(region)) {
		return ImmuSet.make();
	}
	if ((transitionsCount(region)) == 1) {
		return ImmuSet.make().with(region);
	}
	result = MuSet.make();
	result.store((makeNew(false, ((PtrArray) (PrimSpec.pointer().arrayWith((lowerEdge(region))))))));
	result.store((makeNew(true, ((PtrArray) (PrimSpec.pointer().arrayWith((upperEdge(region))))))));
	return result.asImmuSet();
/*
udanax-top.st:18399:EdgeManager methodsFor: 'operations'!
{ScruSet of: XnRegion} distinctions: region {XnRegion}
	| result {MuSet} |
	(self isSimple: region) ifFalse:
		[Heaper BLAST: #InvalidRequest].
	(self isEmpty: region) ifTrue:
		[^ImmuSet make with: region].
	(self isFull: region) ifTrue:
		[^ImmuSet make].
	(self transitionsCount: region) = 1 ifTrue:
		[^ImmuSet make with: region].
	result := MuSet make.
	result store: (self makeNew: false with: ((PrimSpec pointer arrayWith: (self lowerEdge: region)) cast: PtrArray)).
	result store: (self makeNew: true with: ((PrimSpec pointer arrayWith: (self upperEdge: region)) cast: PtrArray)).
	^result asImmuSet!
*/
}
public XnRegion intersect(XnRegion meRegion, XnRegion otherRegion) {
	EdgeStepper mine;
	EdgeStepper others;
	EdgeAccumulator result;
	XnRegion resultReg;
	if (isEmpty(otherRegion)) {
		return otherRegion;
	}
	mine = edgeStepper(meRegion);
	others = edgeStepper(otherRegion);
	result = edgeAccumulator(((startsInside(meRegion)) && (startsInside(otherRegion))));
	while (mine.hasValue() && (others.hasValue())) {
		TransitionEdge me;
		TransitionEdge other;
		me = mine.getEdge();
		other = others.getEdge();
		if ( ! (me.isGE(other))) {
			if ( ! others.isEntering()) {
				result.edge(me);
			}
			mine.step();
		}
		else {
			if ( ! mine.isEntering()) {
				result.edge(other);
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
/*
udanax-top.st:18415:EdgeManager methodsFor: 'operations'!
{XnRegion} intersect: meRegion {XnRegion} with: otherRegion {XnRegion}
	| mine {EdgeStepper} others {EdgeStepper} result {EdgeAccumulator} resultReg {XnRegion} |
	(self isEmpty: otherRegion)
		ifTrue: [^otherRegion].
	mine := self edgeStepper: meRegion.
	others := self edgeStepper: otherRegion.
	result := self edgeAccumulator: ((self startsInside: meRegion)
			and: [self startsInside: otherRegion]).
	[mine hasValue and: [others hasValue]] whileTrue:
		[ | me {TransitionEdge} other {TransitionEdge} |
		me := mine getEdge.
		other := others getEdge.
		(me isGE: other) not
			ifTrue:
				[others isEntering not ifTrue: [result edge: me].
				mine step]
			ifFalse:
				[mine isEntering not ifTrue: [result edge: other].
				others step]].
	(mine hasValue and: [others isEntering not]) ifTrue: [result edges: mine].
	(others hasValue and: [mine isEntering not]) ifTrue: [result edges: others].
	mine destroy.
	others destroy.
	resultReg := result region.
	result destroy.
	^ resultReg!
*/
}
public Stepper simpleRegions(XnRegion region, OrderSpec order) {
	if (order != null) {
		throw new UnimplementedException();
	}
	return EdgeSimpleRegionStepper.make(this, (edgeStepper(region)));
/*
udanax-top.st:18443:EdgeManager methodsFor: 'operations'!
{Stepper} simpleRegions: region {XnRegion} with: order {OrderSpec default: NULL}
	order ~~ NULL ifTrue: 
		[self unimplemented].
	^EdgeSimpleRegionStepper make: self
		with: (self edgeStepper: region)!
*/
}
public XnRegion unionWith(XnRegion meRegion, XnRegion otherRegion) {
	EdgeStepper mine;
	EdgeStepper others;
	EdgeAccumulator result;
	XnRegion resultReg;
	if (isEmpty(otherRegion)) {
		return meRegion;
	}
	mine = edgeStepper(meRegion);
	others = edgeStepper(otherRegion);
	result = edgeAccumulator(((startsInside(meRegion)) || (startsInside(otherRegion))));
	while (mine.hasValue() && (others.hasValue())) {
		TransitionEdge me;
		TransitionEdge other;
		me = mine.getEdge();
		other = others.getEdge();
		if ( ! (me.isGE(other))) {
			if (others.isEntering()) {
				result.edge(me);
			}
			mine.step();
		}
		else {
			if (mine.isEntering()) {
				result.edge(other);
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
/*
udanax-top.st:18450:EdgeManager methodsFor: 'operations'!
{XnRegion} unionWith: meRegion {XnRegion} with: otherRegion {XnRegion}
	| mine {EdgeStepper} others {EdgeStepper} result {EdgeAccumulator} resultReg {XnRegion} |
	(self isEmpty: otherRegion)
		ifTrue: [^meRegion].
	mine := self edgeStepper: meRegion.
	others := self edgeStepper: otherRegion.
	result := self edgeAccumulator: ((self startsInside: meRegion)
			or: [self startsInside: otherRegion]).
	[mine hasValue and: [others hasValue]] whileTrue:
		[ | me {TransitionEdge} other {TransitionEdge} |
		me := mine getEdge.
		other := others getEdge.
		(me isGE: other) not
			ifTrue:
				[others isEntering ifTrue: [result edge: me].
				mine step]
			ifFalse:
				[mine isEntering ifTrue: [result edge: other].
				others step]].
	(mine hasValue and: [others isEntering]) ifTrue: [result edges: mine].
	(others hasValue and: [mine isEntering]) ifTrue: [result edges: others].
	mine destroy.
	others destroy.
	resultReg := result region.
	result destroy.
	^ resultReg!
*/
}
public XnRegion with(XnRegion meRegion, Position newPos) {
	EdgeStepper mine;
	EdgeStepper others;
	EdgeAccumulator result;
	XnRegion resultReg;
	mine = edgeStepper(meRegion);
	others = singleEdgeStepper(newPos);
	result = edgeAccumulator((startsInside(meRegion)));
	while (mine.hasValue() && (others.hasValue())) {
		TransitionEdge me;
		TransitionEdge other;
		me = mine.getEdge();
		other = others.getEdge();
		if ( ! (me.isGE(other))) {
			if (others.isEntering()) {
				result.edge(me);
			}
			mine.step();
		}
		else {
			if (mine.isEntering()) {
				result.edge(other);
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
/*
udanax-top.st:18478:EdgeManager methodsFor: 'operations'!
{XnRegion} with: meRegion {XnRegion} with: newPos {Position}
	| mine {EdgeStepper} others {EdgeStepper} result {EdgeAccumulator} resultReg {XnRegion} |
	mine := self edgeStepper: meRegion.
	others := self singleEdgeStepper: newPos.
	result := self edgeAccumulator: (self startsInside: meRegion).
	[mine hasValue and: [others hasValue]] whileTrue:
		[ | me {TransitionEdge} other {TransitionEdge} |
		me := mine getEdge.
		other := others getEdge.
		(me isGE: other) not
			ifTrue:
				[others isEntering ifTrue: [result edge: me].
				mine step]
			ifFalse:
				[mine isEntering ifTrue: [result edge: other].
				others step]].
	(mine hasValue and: [others isEntering]) ifTrue: [result edges: mine].
	(others hasValue and: [mine isEntering]) ifTrue: [result edges: others].
	mine destroy.
	others destroy.
	resultReg := result region.
	result destroy.
	^ resultReg!
*/
}
/**
 * The position associated with the given edge. Blast if there is none
 */
public Position edgePosition(TransitionEdge edge) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:18505:EdgeManager methodsFor: 'protected:'!
{Position} edgePosition: edge {TransitionEdge}
	"The position associated with the given edge. Blast if there is none"
	
	self subclassResponsibility!
*/
}
/**
 * Make a new region of the right type
 */
public XnRegion makeNew(boolean startsInside, PtrArray transitions) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:18510:EdgeManager methodsFor: 'protected:'!
{XnRegion} makeNew: startsInside {BooleanVar} with: transitions {PtrArray of: TransitionEdge}
	"Make a new region of the right type"
	
	self subclassResponsibility!
*/
}
/**
 * Make a new region of the right type
 */
public XnRegion makeNew(boolean startsInside, PtrArray transitions, int count) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:18515:EdgeManager methodsFor: 'protected:'!
{XnRegion} makeNew: startsInside {BooleanVar} with: transitions {PtrArray of: TransitionEdge} with: count {Int32}
	"Make a new region of the right type"
	
	self subclassResponsibility!
*/
}
public PtrArray posTransitions(Position pos) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:18520:EdgeManager methodsFor: 'protected:'!
{PtrArray of: TransitionEdge} posTransitions: pos {Position}
	self subclassResponsibility!
*/
}
public boolean startsInside(XnRegion region) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:18524:EdgeManager methodsFor: 'protected:'!
{BooleanVar} startsInside: region {XnRegion}
	self subclassResponsibility!
*/
}
public PtrArray transitions(XnRegion region) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:18528:EdgeManager methodsFor: 'protected:'!
{PtrArray of: TransitionEdge} transitions: region {XnRegion}
	self subclassResponsibility!
*/
}
public int transitionsCount(XnRegion region) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:18532:EdgeManager methodsFor: 'protected:'!
{Int32} transitionsCount: region {XnRegion}
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:18538:EdgeManager methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:18540:EdgeManager methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public EdgeManager() {
/*

Generated during transformation
*/
}
public EdgeManager(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
