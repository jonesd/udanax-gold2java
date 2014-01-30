/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.filter;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.filter.AndFilter;
import info.dgjones.abora.gold.filter.ClosedFilter;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterPosition;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.filter.Joint;
import info.dgjones.abora.gold.filter.NotSubsetFilter;
import info.dgjones.abora.gold.filter.NotSupersetFilter;
import info.dgjones.abora.gold.filter.OpenFilter;
import info.dgjones.abora.gold.filter.OrFilter;
import info.dgjones.abora.gold.filter.RegionDelta;
import info.dgjones.abora.gold.filter.SubsetFilter;
import info.dgjones.abora.gold.filter.SupersetFilter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A position in a FilterSpace is a region in the baseSpace, and a filter is a set of regions
 * in the baseSpace. It is often more useful to think of a Filter as a Boolean function whose
 * input is a region in the baseSpace, and of unions, intersections, and complements of
 * filters as ORs, ANDs, and NOTs of functions. Not all possible such functions can be
 * represented as Filters, since there is an uncountable infinity of them for any non-finite
 * CoordinateSpace. There are representations for some basic filters, and any filters
 * resulting from a finite sequence of unions, intersections, and complements among them. The
 * basic filters are:
 * subsetFilter(cs,R) -- all subsets of R (i.e. all R1 such that R1->isSubsetOf(R))
 * supersetFilter(cs,R) -- all supersets of R (i.e. all R1 such that R->isSubsetOf(R1))
 * Mathematically, this is all that is necessary, since other useful filters like
 * intersection filters can be generated from these. (e.g. intersectionFilter(R) is
 * subsetFilter(R->complement())->complement()). However, there are several more pseudo
 * constructors provided as shortcuts, including intersectionFilters, closedFilters,
 * emptyFilters, and intersections and unions of sets of filters.
 */
public class Filter extends XnRegion {

	protected FilterSpace myCS;
/*
udanax-top.st:66068:
XnRegion subclass: #Filter
	instanceVariableNames: 'myCS {FilterSpace}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Filter'!
*/
/*
udanax-top.st:66072:
Filter comment:
'A position in a FilterSpace is a region in the baseSpace, and a filter is a set of regions in the baseSpace. It is often more useful to think of a Filter as a Boolean function whose input is a region in the baseSpace, and of unions, intersections, and complements of filters as ORs, ANDs, and NOTs of functions. Not all possible such functions can be represented as Filters, since there is an uncountable infinity of them for any non-finite CoordinateSpace. There are representations for some basic filters, and any filters resulting from a finite sequence of unions, intersections, and complements among them. The basic filters are:
	subsetFilter(cs,R) -- all subsets of R (i.e. all R1 such that R1->isSubsetOf(R))
	supersetFilter(cs,R) -- all supersets of R (i.e. all R1 such that R->isSubsetOf(R1))
Mathematically, this is all that is necessary, since other useful filters like intersection filters can be generated from these. (e.g. intersectionFilter(R) is subsetFilter(R->complement())->complement()). However, there are several more pseudo constructors provided as shortcuts, including intersectionFilters, closedFilters, emptyFilters, and intersections and unions of sets of filters.'!
*/
/*
udanax-top.st:66077:
(Filter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #COPY; yourself)!
*/
/*
udanax-top.st:66404:
Filter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:66407:
(Filter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Filter.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public XnRegion complement() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66082:Filter methodsFor: 'operations'!
{XnRegion} complement
	
	self subclassResponsibility!
*/
}
public XnRegion intersect(XnRegion other) {
	XnRegion result;
	result = fetchIntersect(other);
	if (result != null) {
		return result;
	}
	return complexIntersect(other);
/*
udanax-top.st:66086:Filter methodsFor: 'operations'!
{XnRegion} intersect: other {XnRegion}
	| result {XnRegion} |
	result := self fetchIntersect: other.
	result ~~ NULL ifTrue: [^result].
	^self complexIntersect: other!
*/
}
public XnRegion simpleUnion(XnRegion other) {
	return unionWith(other);
/*
udanax-top.st:66093:Filter methodsFor: 'operations'!
{XnRegion INLINE} simpleUnion: other {XnRegion}
	^self unionWith: other!
*/
}
public XnRegion unionWith(XnRegion other) {
	XnRegion result;
	result = fetchUnion(other);
	if (result != null) {
		return result;
	}
	return complexUnion(other);
/*
udanax-top.st:66097:Filter methodsFor: 'operations'!
{XnRegion} unionWith: other {XnRegion}
	| result {XnRegion} |
	result := self fetchUnion: other.
	result ~~ NULL ifTrue: [^result].
	^self complexUnion: other!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:66106:Filter methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public boolean hasMember(Position pos) {
	return match(((FilterPosition) pos).baseRegion());
/*
udanax-top.st:66110:Filter methodsFor: 'testing'!
{BooleanVar} hasMember: pos {Position}
	^self match: (pos cast: FilterPosition) baseRegion!
*/
}
/**
 * Essential. Whether this is an 'all' Filter, i.e. it only matches Regions which contain
 * everything in the baseRegion
 */
public boolean isAllFilter() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66114:Filter methodsFor: 'testing'!
{BooleanVar CLIENT} isAllFilter
	"Essential. Whether this is an 'all' Filter, i.e. it only matches Regions which contain everything in the baseRegion"
	
	self subclassResponsibility!
*/
}
/**
 * Essential. Whether this is an 'any' Filter, i.e. it matches Regions which contain anything
 * in the baseRegion
 */
public boolean isAnyFilter() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66119:Filter methodsFor: 'testing'!
{BooleanVar CLIENT} isAnyFilter
	"Essential. Whether this is an 'any' Filter, i.e. it matches Regions which contain anything in the baseRegion"
	
	self subclassResponsibility!
*/
}
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66124:Filter methodsFor: 'testing'!
{BooleanVar} isEmpty
	self subclassResponsibility!
*/
}
public boolean isEnumerable(OrderSpec order) {
	Someone.knownBug();
	/* The singleton region should act enumerably */
	return false;
/*
udanax-top.st:66128:Filter methodsFor: 'testing'!
{BooleanVar} isEnumerable: order {OrderSpec default: NULL}
	
	self knownBug. "The singleton region should act enumerably"
	^false!
*/
}
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66133:Filter methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	self subclassResponsibility!
*/
}
public boolean isFinite() {
	return false;
/*
udanax-top.st:66137:Filter methodsFor: 'testing'!
{BooleanVar INLINE} isFinite
	^false!
*/
}
public boolean isFull() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66141:Filter methodsFor: 'testing'!
{BooleanVar} isFull	
	self subclassResponsibility!
*/
}
public boolean isSimple() {
	return true;
/*
udanax-top.st:66145:Filter methodsFor: 'testing'!
{BooleanVar INLINE} isSimple
	^true!
*/
}
public boolean isSubsetOf(XnRegion other) {
	Filter o;
	o = (Filter) other;
	if ((fetchSpecialSubset(o)) == this) {
		return true;
	}
	if ((o.fetchSpecialSubset(this)) == this) {
		return true;
	}
	return ! (intersects((o.complement())));
/*
udanax-top.st:66148:Filter methodsFor: 'testing'!
{BooleanVar} isSubsetOf: other {XnRegion}
	| o {Filter} |
	o _ other cast: Filter.
	(self fetchSpecialSubset: o) == self ifTrue: [^true].
	(o fetchSpecialSubset: self) == self ifTrue: [^true].
	^(self intersects: (o complement)) not!
*/
}
public int count() {
	throw new UnimplementedException();
/*
udanax-top.st:66157:Filter methodsFor: 'enumerating'!
{IntegerVar} count
	self unimplemented.
	^IntegerVar0!
*/
}
/**
 * Essential. Break this up into a bunch of Filters which when intersected (anded) together
 * give this one. If there is only one sub Filter it will be the receiver; otherwise, the sub
 * Filters will be simple enough that either they or their complements will return true from
 * at least one of isAndFilter or isOrFilter. If this is full (i.e. an open filter) then the
 * stepper will have no elements.
 */
public Stepper intersectedFilters() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66161:Filter methodsFor: 'enumerating'!
{Stepper CLIENT of: Filter} intersectedFilters
	"Essential. Break this up into a bunch of Filters which when intersected (anded) together give this one. If there is only one sub Filter it will be the receiver; otherwise, the sub Filters will be simple enough that either they or their complements will return true from at least one of isAndFilter or isOrFilter. If this is full (i.e. an open filter) then the stepper will have no elements."
	
	self subclassResponsibility!
*/
}
/**
 * Essential. Break this up into a bunch of Filters which when unioned (ored) together give
 * this one. If there is only one sub Filter it will be the receiver; otherwise, the sub
 * Filters will be simple enough that either they or their complements will return true from
 * at least one of isAndFilter or isOrFilter. The sub Filters might not be disjoint Regions.
 * If this is empty (i.e. a closed filter) then the stepper will have no elements.
 */
public Stepper unionedFilters() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66166:Filter methodsFor: 'enumerating'!
{Stepper CLIENT of: Filter} unionedFilters
	"Essential. Break this up into a bunch of Filters which when unioned (ored) together give this one. If there is only one sub Filter it will be the receiver; otherwise, the sub Filters will be simple enough that either they or their complements will return true from at least one of isAndFilter or isOrFilter. The sub Filters might not be disjoint Regions. If this is empty (i.e. a closed filter) then the stepper will have no elements."
	
	self subclassResponsibility!
*/
}
public XnRegion asSimpleRegion() {
	return this;
/*
udanax-top.st:66173:Filter methodsFor: 'accessing'!
{XnRegion INLINE} asSimpleRegion
	^self!
*/
}
/**
 * Essential. A region in the base space identifying what kind of filter this is. Succeeds
 * only if this isAnyFilter or isAllFilter.
 */
public XnRegion baseRegion() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66177:Filter methodsFor: 'accessing'!
{XnRegion CLIENT} baseRegion
	"Essential. A region in the base space identifying what kind of filter this is. Succeeds only if this isAnyFilter or isAllFilter."
	
	self subclassResponsibility!
*/
}
public CoordinateSpace coordinateSpace() {
	return myCS;
/*
udanax-top.st:66182:Filter methodsFor: 'accessing'!
{CoordinateSpace INLINE} coordinateSpace
	^myCS!
*/
}
/**
 * The region which is relevant to this filter, i.e. whose presence or absence in a region
 * can change whether it passes the filter
 */
public XnRegion relevantRegion() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66186:Filter methodsFor: 'accessing'!
{XnRegion} relevantRegion
	"The region which is relevant to this filter, i.e. whose presence or absence in a region can change whether it passes the filter"
	
	self subclassResponsibility!
*/
}
/**
 * Whether there might be anything in the tree beneath the Joint that would pass the filter.
 */
public boolean doesPass(Joint joint) {
	return ! (pass(joint)).isEmpty();
/*
udanax-top.st:66193:Filter methodsFor: 'filtering'!
{BooleanVar} doesPass: joint {Joint}
	"Whether there might be anything in the tree beneath the Joint that would pass the filter."
	^(self pass: joint) isEmpty not!
*/
}
/**
 * Whether the change causes a change in the state of the filter. (I.E. Whether the old
 * region was in and the new out, or vice versa.)
 */
public boolean isSwitchedBy(RegionDelta delta) {
	return (match(delta.before())) != (match(delta.after()));
/*
udanax-top.st:66198:Filter methodsFor: 'filtering'!
{BooleanVar} isSwitchedBy: delta {RegionDelta}
	"Whether the change causes a change in the state of the filter. (I.E. Whether the old region was in and the new out, or vice versa.)"
	
	^(self match: delta before) ~~ (self match: delta after)!
*/
}
/**
 * Whether the change switches the state of the filter from on to off. (I.E. Whether the old
 * region was inside the filter and the new region outside.)
 */
public boolean isSwitchedOffBy(RegionDelta delta) {
	return (match(delta.before())) && ( ! (match(delta.after())));
/*
udanax-top.st:66203:Filter methodsFor: 'filtering'!
{BooleanVar} isSwitchedOffBy: delta {RegionDelta}
	"Whether the change switches the state of the filter from on to off. (I.E. Whether the old region was inside the filter and the new region outside.)"
	
	^(self match: delta before) and: [(self match: delta after) not]!
*/
}
/**
 * Whether the change switches the state of the filter from off to on. (I.E. Whether the old
 * region was outside the filter and the new region inside.)
 */
public boolean isSwitchedOnBy(RegionDelta delta) {
	return ! (match(delta.before())) && (match(delta.after()));
/*
udanax-top.st:66208:Filter methodsFor: 'filtering'!
{BooleanVar} isSwitchedOnBy: delta {RegionDelta}
	"Whether the change switches the state of the filter from off to on. (I.E. Whether the old region was outside the filter and the new region inside.)"
	
	^(self match: delta before) not and: [self match: delta after]!
*/
}
/**
 * Whether a region is inside this filter.
 */
public boolean match(XnRegion region) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66213:Filter methodsFor: 'filtering'!
{BooleanVar CLIENT} match: region {XnRegion}
	"Whether a region is inside this filter."
	
	self subclassResponsibility!
*/
}
/**
 * The simplest filter for looking at the tree beneath the node.
 * The Joint keeps the intersection and union of all the nodes beneath it, so the result of
 * this message can be used to prune a search. If the result is full, then everything beneath
 * the node is in the filter (e.g. if this filter contained all subsets of S and the union
 * was a superset of S). If the result is empty, then nothing beneath the node is in the
 * filter (e.g. if this filter contained all subsets of S and the intersection was not a
 * subset of S). In less extreme cases, this operation may at least simplify the filter by
 * throwing out irrelevant search criteria.
 */
public Filter pass(Joint parent) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66218:Filter methodsFor: 'filtering'!
{Filter} pass: parent {Joint}
	"The simplest filter for looking at the tree beneath the node.
	The Joint keeps the intersection and union of all the nodes beneath it, so the result of this message can be used to prune a search. If the result is full, then everything beneath the node is in the filter (e.g. if this filter contained all subsets of S and the union was a superset of S). If the result is empty, then nothing beneath the node is in the filter (e.g. if this filter contained all subsets of S and the intersection was not a subset of S). In less extreme cases, this operation may at least simplify the filter by throwing out irrelevant search criteria."
	
	self subclassResponsibility!
*/
}
public Filter(FilterSpace cs) {
	super();
	myCS = cs;
/*
udanax-top.st:66226:Filter methodsFor: 'creation'!
create: cs {FilterSpace}
	super create.
	myCS _ cs!
*/
}
public ScruSet distinctions() {
	if (isFull()) {
		return ImmuSet.make();
	}
	else {
		return ImmuSet.make().with(this);
	}
/*
udanax-top.st:66232:Filter methodsFor: 'components'!
{ScruSet of: XnRegion} distinctions
	self isFull
		ifTrue: [^ImmuSet make]
		ifFalse: [^ImmuSet make with: self]!
*/
}
public Stepper simpleRegions(OrderSpec order) {
	if (isEmpty()) {
		return ImmuSet.make().stepper();
	}
	else {
		return (ImmuSet.make().with(this)).stepper();
	}
/*
udanax-top.st:66238:Filter methodsFor: 'components'!
{Stepper} simpleRegions: order {OrderSpec unused default: NULL}
	self isEmpty
		ifTrue: [^ImmuSet make stepper]
		ifFalse: [^(ImmuSet make with: self) stepper]!
*/
}
public XnRegion complexIntersect(XnRegion other) {
	Filter a;
	Filter b;
	Pair canon;
	if (isKindOf(AboraSupport.findCategory(OrFilter.class))) {
		a = this;
		b = (Filter) other;
	}
	else {
		b = this;
		a = (Filter) other;
	}
	if (a instanceof OrFilter) {
		if (b instanceof OrFilter) {
			return Filter.orFilterPrivate(myCS, (Filter.distributeIntersect(coordinateSpace(), ((OrFilter) a).subFilters(), ((OrFilter) b).subFilters())));
		}
		else {
			return Filter.orFilterPrivate(myCS, (Filter.distributeIntersect(coordinateSpace(), ((OrFilter) a).subFilters(), b)));
		}
	}
	if ( ! (a instanceof AndFilter)) {
		Filter t;
		t = a;
		a = b;
		b = t;
	}
	if (a instanceof AndFilter) {
		if (b instanceof AndFilter) {
			return Filter.andFilterPrivate(myCS, (Filter.combineIntersect(((AndFilter) a).subFilters(), ((AndFilter) b).subFilters())));
		}
		return Filter.andFilterPrivate(myCS, (Filter.combineIntersect(((AndFilter) a).subFilters(), b)));
	}
	canon = getPairIntersect(((Filter) other));
	return Filter.andFilterPrivate(myCS, ((ImmuSet.make().with(canon.left())).with(canon.right())));
/*
udanax-top.st:66246:Filter methodsFor: 'vulnerable: internal'!
{XnRegion} complexIntersect: other {XnRegion}
	| a {Filter} b {Filter} canon {Pair of: Filter} |
	(self isKindOf: OrFilter)
		ifTrue: [a _ self. b _ other cast: Filter]
		ifFalse: [b _ self. a _ other cast: Filter].
	(a isKindOf: OrFilter) ifTrue:
		[(b isKindOf: OrFilter)
			ifTrue: [^Filter orFilterPrivate: myCS
				with: (Filter distributeIntersect: self coordinateSpace
					with: (a cast: OrFilter) subFilters
					with.ImmuSet: (b cast: OrFilter) subFilters)]
			ifFalse: [^Filter orFilterPrivate: myCS
				with: (Filter distributeIntersect: self coordinateSpace
					with: (a cast: OrFilter) subFilters
					with.Filter: b)]].
	(a isKindOf: AndFilter)
		ifFalse: [ | t {Filter} | t _ a. a _ b. b _ t].
	(a isKindOf: AndFilter) ifTrue:
		[(b isKindOf: AndFilter)
			ifTrue: [^Filter andFilterPrivate: myCS
				with: (Filter combineIntersect: (a cast: AndFilter) subFilters
					with.ImmuSet:  (b cast: AndFilter) subFilters)].
		^Filter andFilterPrivate: myCS
				with: (Filter combineIntersect: (a cast: AndFilter) subFilters
					with.Filter:  b)].
	canon _ self getPairIntersect: (other cast: Filter).
	^Filter andFilterPrivate: myCS
			with: ((ImmuSet make with: canon left) with: canon right)!
*/
}
public XnRegion complexUnion(XnRegion other) {
	Filter a;
	Filter b;
	Pair canon;
	if (isKindOf(AboraSupport.findCategory(OrFilter.class))) {
		a = this;
		b = (Filter) other;
	}
	else {
		b = this;
		a = (Filter) other;
	}
	if (a instanceof OrFilter) {
		if (b instanceof OrFilter) {
			return Filter.orFilterPrivate(myCS, (Filter.combineUnion(((OrFilter) a).subFilters(), ((OrFilter) b).subFilters())));
		}
		if (b instanceof AndFilter) {
			return Filter.orFilterPrivate(myCS, (Filter.distributeUnion(coordinateSpace(), ((AndFilter) b).subFilters(), ((OrFilter) a).subFilters())));
		}
		return Filter.orFilterPrivate(myCS, (Filter.combineUnion(((OrFilter) a).subFilters(), b)));
	}
	if ( ! (a instanceof AndFilter)) {
		Filter t;
		t = a;
		a = b;
		b = t;
	}
	if (a instanceof AndFilter) {
		return Filter.orFilterPrivate(myCS, (Filter.distributeUnion(coordinateSpace(), ((AndFilter) a).subFilters(), b)));
	}
	canon = getPairUnion(((Filter) other));
	return Filter.orFilterPrivate(myCS, ((ImmuSet.make().with(canon.left())).with(canon.right())));
/*
udanax-top.st:66275:Filter methodsFor: 'vulnerable: internal'!
{XnRegion} complexUnion: other {XnRegion}
	| a {Filter} b {Filter} canon {Pair of: Filter} |
	(self isKindOf: OrFilter)
		ifTrue: [a _ self. b _ other cast: Filter]
		ifFalse: [b _ self. a _ other cast: Filter].
	(a isKindOf: OrFilter) ifTrue:
		[(b isKindOf: OrFilter)
			ifTrue: [^Filter orFilterPrivate: myCS
				with: (Filter combineUnion: (a cast: OrFilter) subFilters
					with.ImmuSet:  (b cast: OrFilter) subFilters)].
		(b isKindOf: AndFilter)
			ifTrue: [^Filter orFilterPrivate: myCS
				with: (Filter distributeUnion: self coordinateSpace
					with: (b cast: AndFilter) subFilters
					with.ImmuSet:  (a cast: OrFilter) subFilters)].
		^Filter orFilterPrivate: myCS
				with: (Filter combineUnion: (a cast: OrFilter) subFilters
					with.Filter:  b)].
	(a isKindOf: AndFilter) ifFalse:
		[ | t {Filter} | t _ a. a _ b. b _ t].
	(a isKindOf: AndFilter) ifTrue:
		[^Filter orFilterPrivate: myCS
			with: (Filter distributeUnion: self coordinateSpace
				with: (a cast: AndFilter) subFilters
				with.Filter: b)].
	canon _ self getPairUnion: (other cast: Filter).
	^Filter orFilterPrivate: myCS
		with: ((ImmuSet make with: canon left) with: canon right)!
*/
}
/**
 * return NULL, or the pair of canonical filters (left == new1 | self, right == new2 | other)
 */
public Pair fetchCanonicalIntersect(Filter other) {
	return null;
/*
udanax-top.st:66304:Filter methodsFor: 'vulnerable: internal'!
{Pair of: Filter} fetchCanonicalIntersect: other {Filter unused}
	"return NULL, or the pair of canonical filters (left == new1 | self, right == new2 | other)"
	^NULL!
*/
}
/**
 * return NULL, or the pair of canonical filters (left == new1 | self, right == new2 | other)
 */
public Pair fetchCanonicalUnion(Filter other) {
	return null;
/*
udanax-top.st:66308:Filter methodsFor: 'vulnerable: internal'!
{Pair of: Filter} fetchCanonicalUnion: other {Filter unused}
	"return NULL, or the pair of canonical filters (left == new1 | self, right == new2 | other)"
	^NULL!
*/
}
public XnRegion fetchIntersect(XnRegion other) {
	XnRegion temp;
	temp = fetchSpecialSubset(other);
	if (temp == null) {
		temp = ((Filter) other).fetchSpecialSubset(this);
	}
	if (temp == this) {
		return this;
	}
	if ((temp) == other) {
		return other;
	}
	temp = fetchSpecialIntersect(other);
	if (temp != null) {
		return temp;
	}
	return ((Filter) other).fetchSpecialIntersect(this);
/*
udanax-top.st:66312:Filter methodsFor: 'vulnerable: internal'!
{XnRegion} fetchIntersect: other {XnRegion}
	| temp {XnRegion} |
	temp _ self fetchSpecialSubset: other.
	temp == NULL ifTrue:
		[temp _ (other cast: Filter) fetchSpecialSubset: self].
	temp == self ifTrue: [^self].
	(temp basicCast: Heaper star) == other ifTrue: [^other].
	temp _ self fetchSpecialIntersect: other.
	temp ~~ NULL ifTrue: [^temp].
	^(other cast: Filter) fetchSpecialIntersect: self!
*/
}
/**
 * return the pair of canonical filters (left == new1 | self, right == new2 | other)
 */
public Pair fetchPairIntersect(Filter other) {
	Pair result;
	result = fetchCanonicalIntersect(other);
	if (result != null) {
		return result;
	}
	result = other.fetchCanonicalIntersect(this);
	if (result != null) {
		return result.reversed();
	}
	return null;
/*
udanax-top.st:66323:Filter methodsFor: 'vulnerable: internal'!
{Pair of: Filter} fetchPairIntersect: other {Filter}
	"return the pair of canonical filters (left == new1 | self, right == new2 | other)"
	| result {Pair of: Filter} |
	result _ self fetchCanonicalIntersect: other.
	result ~~ NULL ifTrue: [^result].
	result _ other fetchCanonicalIntersect: self.
	result ~~ NULL ifTrue: [^result reversed].
	^NULL!
*/
}
/**
 * return the pair of canonical filters (left == new1 | self, right == new2 | other)
 */
public Pair fetchPairUnion(Filter other) {
	Pair result;
	result = fetchCanonicalUnion(other);
	if (result != null) {
		return result;
	}
	result = other.fetchCanonicalUnion(this);
	if (result != null) {
		return result.reversed();
	}
	return null;
/*
udanax-top.st:66332:Filter methodsFor: 'vulnerable: internal'!
{Pair of: Filter} fetchPairUnion: other {Filter}
	"return the pair of canonical filters (left == new1 | self, right == new2 | other)"
	| result {Pair of: Filter} |
	result _ self fetchCanonicalUnion: other.
	result ~~ NULL ifTrue: [^result].
	result _ other fetchCanonicalUnion: self.
	result ~~ NULL ifTrue: [^result reversed].
	^NULL!
*/
}
public XnRegion fetchSpecialIntersect(XnRegion other) {
	return null;
/*
udanax-top.st:66341:Filter methodsFor: 'vulnerable: internal'!
{XnRegion} fetchSpecialIntersect: other {XnRegion unused}
	^NULL!
*/
}
/**
 * return self or other if one is clearly a subset of the other, else NULL
 */
public XnRegion fetchSpecialSubset(XnRegion other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:66344:Filter methodsFor: 'vulnerable: internal'!
{XnRegion} fetchSpecialSubset: other {XnRegion}
	"return self or other if one is clearly a subset of the other, else NULL"
	self subclassResponsibility!
*/
}
public XnRegion fetchSpecialUnion(XnRegion other) {
	return null;
/*
udanax-top.st:66348:Filter methodsFor: 'vulnerable: internal'!
{XnRegion} fetchSpecialUnion: other {XnRegion unused}
	^NULL!
*/
}
public XnRegion fetchUnion(XnRegion other) {
	XnRegion temp;
	temp = fetchSpecialSubset(other);
	if (temp == null) {
		temp = ((Filter) other).fetchSpecialSubset(this);
	}
	if (temp == this) {
		return other;
	}
	if ((temp) == other) {
		return this;
	}
	temp = fetchSpecialUnion(other);
	if (temp != null) {
		return temp;
	}
	return ((Filter) other).fetchSpecialUnion(this);
/*
udanax-top.st:66351:Filter methodsFor: 'vulnerable: internal'!
{XnRegion} fetchUnion: other {XnRegion}
	| temp {XnRegion} |
	temp _ self fetchSpecialSubset: other.
	temp == NULL ifTrue:
		[temp _ (other cast: Filter) fetchSpecialSubset: self].
	temp == self ifTrue: [^other].
	(temp basicCast: Heaper star) == other ifTrue: [^self].
	temp _ self fetchSpecialUnion: other.
	temp ~~ NULL ifTrue: [^temp].
	^(other cast: Filter) fetchSpecialUnion: self!
*/
}
/**
 * return the pair of canonical filters (left == new1 | self, right == new2 | other)
 */
public Pair getPairIntersect(Filter other) {
	Pair result;
	result = fetchCanonicalIntersect(other);
	if (result != null) {
		return result;
	}
	result = other.fetchCanonicalIntersect(this);
	if (result != null) {
		return result.reversed();
	}
	return Pair.make(this, other);
/*
udanax-top.st:66362:Filter methodsFor: 'vulnerable: internal'!
{Pair of: Filter} getPairIntersect: other {Filter}
	"return the pair of canonical filters (left == new1 | self, right == new2 | other)"
	| result {Pair of: Filter} |
	result _ self fetchCanonicalIntersect: other.
	result ~~ NULL ifTrue: [^result].
	result _ other fetchCanonicalIntersect: self.
	result ~~ NULL ifTrue: [^result reversed].
	^Pair make: self with: other!
*/
}
/**
 * return the pair of canonical filters (left == new1 | self, right == new2 | other)
 */
public Pair getPairUnion(Filter other) {
	Pair result;
	result = fetchCanonicalUnion(other);
	if (result != null) {
		return result;
	}
	result = other.fetchCanonicalUnion(this);
	if (result != null) {
		return result.reversed();
	}
	return Pair.make(this, other);
/*
udanax-top.st:66371:Filter methodsFor: 'vulnerable: internal'!
{Pair of: Filter} getPairUnion: other {Filter}
	"return the pair of canonical filters (left == new1 | self, right == new2 | other)"
	| result {Pair of: Filter} |
	result _ self fetchCanonicalUnion: other.
	result ~~ NULL ifTrue: [^result].
	result _ other fetchCanonicalUnion: self.
	result ~~ NULL ifTrue: [^result reversed].
	^Pair make: self with: other!
*/
}
public Stepper actualStepper(OrderSpec order) {
	Ravi.shouldImplement();
	return null;
/*
udanax-top.st:66382:Filter methodsFor: 'protected: enumerating'!
{Stepper of: Position} actualStepper: order {OrderSpec} 
	
	Ravi shouldImplement.
	^NULL "fodder"!
*/
}
public FilterSpace filterSpace() {
	return myCS;
/*
udanax-top.st:66389:Filter methodsFor: 'protected:'!
{FilterSpace INLINE} filterSpace
	^myCS!
*/
}
public Filter(Rcvr receiver) {
	super(receiver);
	myCS = (FilterSpace) receiver.receiveHeaper();
/*
udanax-top.st:66395:Filter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCS _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCS);
/*
udanax-top.st:66399:Filter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCS.!
*/
}
/**
 * A filter that matches only regions that all of the filters in the set would have matched.
 */
public static Filter andFilter(CoordinateSpace cs, ScruSet subs) {
	ImmuSet result;
	result = ImmuSet.make();
	Stepper stomper = subs.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		if (sub instanceof ClosedFilter) {
			ClosedFilter cSub = (ClosedFilter) sub;
			return Filter.closedFilter(cs);
		}
		else if (sub instanceof AndFilter) {
			AndFilter aSub = (AndFilter) sub;
			result = combineIntersect(result, aSub.subFilters());
		}
		else if (sub instanceof OpenFilter) {
			OpenFilter oSub = (OpenFilter) sub;
		}
		else {
			result = combineIntersect(result, sub);
		}
	}
	stomper.destroy();
	return Filter.andFilterPrivate(((FilterSpace) cs), result);
/*
udanax-top.st:66412:Filter class methodsFor: 'pseudo constructors'!
{Filter} andFilter: cs {CoordinateSpace} with: subs {ScruSet of: Filter}
	"A filter that matches only regions that all of the filters in the set would have matched."
	
	| result {ImmuSet} |
	result _ ImmuSet make.
	subs stepper forEach: [ :sub {Filter} |
		sub
			cast: ClosedFilter into: [:cSub |
				^Filter closedFilter: cs]
			cast: AndFilter into: [:aSub |
				result _ self combineIntersect: result
					with.ImmuSet: aSub subFilters]
			cast: OpenFilter into: [:oSub | ]
			others:
				[result _ self combineIntersect: result
					with.Filter: sub]].
	^Filter andFilterPrivate: (cs cast: FilterSpace) with: result!
*/
}
/**
 * An filter that does match any region.
 */
public static Filter closedFilter(CoordinateSpace cs) {
	return new ClosedFilter(((FilterSpace) cs));
/*
udanax-top.st:66430:Filter class methodsFor: 'pseudo constructors'!
{Filter} closedFilter: cs {CoordinateSpace}
	"An filter that does match any region."
	
	^ClosedFilter create: (cs cast: FilterSpace)!
*/
}
/**
 * A filter that matches any region that intersects the given region.
 */
public static Filter intersectionFilter(CoordinateSpace cs, XnRegion region) {
	return new NotSubsetFilter(((FilterSpace) cs), region.complement());
/*
udanax-top.st:66435:Filter class methodsFor: 'pseudo constructors'!
{Filter} intersectionFilter: cs {CoordinateSpace} with: region {XnRegion}
	"A filter that matches any region that intersects the given region."
	
	^NotSubsetFilter create: (cs cast: FilterSpace) with: region complement!
*/
}
/**
 * A filter matching any regions that is not a subset of the given region.
 */
public static Filter notSubsetFilter(CoordinateSpace cs, XnRegion region) {
	if (region.isFull()) {
		return Filter.closedFilter(cs);
	}
	return new NotSubsetFilter(((FilterSpace) cs), region);
/*
udanax-top.st:66440:Filter class methodsFor: 'pseudo constructors'!
{Filter} notSubsetFilter: cs {CoordinateSpace} with: region {XnRegion}
	"A filter matching any regions that is not a subset of the given region."
	
	region isFull ifTrue: [^Filter closedFilter: cs].
	^NotSubsetFilter create: (cs cast: FilterSpace) with: region!
*/
}
/**
 * A filter that matches any region that is not a superset of the given region.
 */
public static Filter notSupersetFilter(CoordinateSpace cs, XnRegion region) {
	if (region.isEmpty()) {
		return Filter.closedFilter(cs);
	}
	return new NotSupersetFilter(((FilterSpace) cs), region);
/*
udanax-top.st:66446:Filter class methodsFor: 'pseudo constructors'!
{Filter} notSupersetFilter: cs {CoordinateSpace} with: region {XnRegion}
	"A filter that matches any region that is not a superset of the given region."
	
	region isEmpty ifTrue: [^Filter closedFilter: cs].
	^NotSupersetFilter create: (cs cast: FilterSpace) with: region!
*/
}
/**
 * A filter that matches any region.
 */
public static Filter openFilter(CoordinateSpace cs) {
	return new OpenFilter(((FilterSpace) cs));
/*
udanax-top.st:66452:Filter class methodsFor: 'pseudo constructors'!
{Filter} openFilter: cs {CoordinateSpace}
	"A filter that matches any region."
	
	^OpenFilter create: (cs cast: FilterSpace)!
*/
}
/**
 * A filter that matches any region that any of the filters in the set would have matched.
 */
public static Filter orFilter(CoordinateSpace cs, ScruSet subs) {
	ImmuSet result;
	result = ImmuSet.make();
	Stepper stomper = subs.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		if (sub instanceof OpenFilter) {
			OpenFilter oSub = (OpenFilter) sub;
			return Filter.openFilter(cs);
		}
		else if (sub instanceof OrFilter) {
			OrFilter orSub = (OrFilter) sub;
			result = combineUnion(result, orSub.subFilters());
		}
		else if (sub instanceof ClosedFilter) {
			ClosedFilter cSub = (ClosedFilter) sub;
		}
		else {
			result = combineUnion(result, sub);
		}
	}
	stomper.destroy();
	return Filter.orFilterPrivate(((FilterSpace) cs), result);
/*
udanax-top.st:66457:Filter class methodsFor: 'pseudo constructors'!
{Filter} orFilter: cs {CoordinateSpace} with: subs {ScruSet of: Filter}
	"A filter that matches any region that any of the filters in the set would have matched."
	
	| result {ImmuSet of: Filter} |
	result _ ImmuSet make.
	subs stepper forEach: [ :sub {Filter} |
		sub
			cast: OpenFilter into: [:oSub |
				^Filter openFilter: cs]
			cast: OrFilter into: [:orSub |
				result _ self combineUnion: result
					with.ImmuSet: orSub subFilters]
			cast: ClosedFilter into: [:cSub | ]
			others:
				[result _ self combineUnion: result
					with.Filter: sub]].
	^Filter orFilterPrivate: (cs cast: FilterSpace) with: result!
*/
}
/**
 * A filter that matches any region that is a subset of the given region.
 */
public static Filter subsetFilter(CoordinateSpace cs, XnRegion region) {
	if (region.isFull()) {
		return Filter.openFilter(cs);
	}
	return new SubsetFilter(((FilterSpace) cs), region);
/*
udanax-top.st:66475:Filter class methodsFor: 'pseudo constructors'!
{Filter} subsetFilter: cs {CoordinateSpace} with: region {XnRegion}
	"A filter that matches any region that is a subset of the given region."
	
	region isFull ifTrue: [^Filter openFilter: cs].
	^SubsetFilter create: (cs cast: FilterSpace) with: region!
*/
}
/**
 * A region that matches any region that is a superset of the given region.
 */
public static Filter supersetFilter(CoordinateSpace cs, XnRegion region) {
	if (region.isEmpty()) {
		return Filter.openFilter(cs);
	}
	return new SupersetFilter(((FilterSpace) cs), region);
/*
udanax-top.st:66481:Filter class methodsFor: 'pseudo constructors'!
{Filter} supersetFilter: cs {CoordinateSpace} with: region {XnRegion}
	"A region that matches any region that is a superset of the given region."
	
	region isEmpty ifTrue: [^Filter openFilter: cs].
	^SupersetFilter create: (cs cast: FilterSpace) with: region!
*/
}
/**
 * assumes that the interactions between elements have already been removed
 */
public static Filter andFilterPrivate(FilterSpace cs, ImmuSet subs) {
	if (subs.isEmpty()) {
		return Filter.openFilter(cs);
	}
	if (subs.count() == 1) {
		return (Filter) subs.theOne();
	}
	return new AndFilter(cs, subs);
/*
udanax-top.st:66489:Filter class methodsFor: 'private: functions'!
{Filter} andFilterPrivate: cs {FilterSpace} with: subs {ImmuSet of: Filter}
	"assumes that the interactions between elements have already been removed"
	subs isEmpty ifTrue: [^Filter openFilter: cs].
	subs count = 1 ifTrue: [^subs theOne cast: Filter].
	^AndFilter create: cs with: subs!
*/
}
/**
 * keep going around doing intersections until there are no more special intersects
 */
public static ImmuSet combineIntersect(ImmuSet set, Filter filter) {
	Stepper subs;
	MuSet nonspecial;
	SetAccumulator result;
	Filter test;
	result = SetAccumulator.make();
	test = filter;
	subs = set.stepper();
	nonspecial = set.asMuSet();
	while (subs.hasValue()) {
		Filter special;
		Filter sub;
		sub = ((Filter) subs.fetch());
		subs.step();
		special = (Filter) (sub.fetchIntersect(test));
		if (special != null) {
			test = special;
			result = SetAccumulator.make();
			nonspecial.remove(sub);
			subs = nonspecial.stepper();
		}
		else {
			Pair canon;
			canon = sub.fetchPairIntersect(test);
			if (canon == null) {
				result.step(sub);
			}
			else {
				test = (Filter) canon.right();
				nonspecial.remove(sub);
				result = SetAccumulator.make();
				nonspecial = (Filter.combineIntersect(nonspecial.asImmuSet(), ((Filter) canon.left()))).asMuSet();
				subs = nonspecial.stepper();
			}
		}
	}
	return ((ImmuSet) result.value()).with(test);
/*
udanax-top.st:66495:Filter class methodsFor: 'private: functions'!
{ImmuSet of: Filter} combineIntersect: set {ImmuSet of: Filter} with.Filter: filter {Filter}
	"keep going around doing intersections until there are no more special intersects"
	| subs {Stepper} nonspecial {MuSet of: Filter} result {SetAccumulator} test {Filter} |
	result _ SetAccumulator make.
	test _ filter.
	subs _ set stepper.
	nonspecial _ set asMuSet.
	[subs hasValue] whileTrue:
		[ | special {Filter} sub {Filter} |
		sub _ (subs fetch cast: Filter).
		subs step.
		special _ (sub fetchIntersect: test) cast: Filter.
		special ~~ NULL ifTrue:
			[test _ special.
			result _ SetAccumulator make.
			nonspecial remove: sub.
			subs _ nonspecial stepper]
		ifFalse:
			[ | canon {Pair of: Filter} |
			canon _ sub fetchPairIntersect: test.
			canon == NULL ifTrue:
				[result step: sub]
			ifFalse:
				[test _ canon right cast: Filter.
				nonspecial remove: sub.
				result _ SetAccumulator make.
				nonspecial _ (Filter combineIntersect: nonspecial asImmuSet
					with.Filter: (canon left cast: Filter)) asMuSet.
				subs _ nonspecial stepper]]].
	^(result value cast: ImmuSet) with: test!
*/
}
public static ImmuSet combineIntersect(ImmuSet a, ImmuSet b) {
	ImmuSet result;
	result = a;
	Stepper stomper = b.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		result = combineIntersect(result, sub);
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:66526:Filter class methodsFor: 'private: functions'!
{ImmuSet of: Filter} combineIntersect: a {ImmuSet of: Filter} with.ImmuSet: b {ImmuSet of: Filter}
	| result {ImmuSet of: Filter} |
	result _ a.
	b stepper forEach: [ :sub {Filter} |
		result _ self combineIntersect: result with.Filter: sub].
	^result!
*/
}
/**
 * keep going around doing unions until there are no more special unions
 */
public static ImmuSet combineUnion(ImmuSet set, Filter filter) {
	Stepper subs;
	MuSet nonspecial;
	SetAccumulator result;
	Filter test;
	result = SetAccumulator.make();
	test = filter;
	subs = set.stepper();
	nonspecial = set.asMuSet();
	while (subs.hasValue()) {
		Filter special;
		Filter sub;
		sub = ((Filter) subs.fetch());
		subs.step();
		special = (Filter) (sub.fetchUnion(test));
		if (special != null) {
			test = special;
			result = SetAccumulator.make();
			nonspecial.remove(sub);
			subs = nonspecial.stepper();
		}
		else {
			Pair canon;
			canon = sub.fetchPairUnion(test);
			if (canon == null) {
				result.step(sub);
			}
			else {
				test = (Filter) canon.right();
				nonspecial.remove(sub);
				result = SetAccumulator.make();
				nonspecial = (Filter.combineUnion(nonspecial.asImmuSet(), ((Filter) canon.left()))).asMuSet();
				subs = nonspecial.stepper();
			}
		}
	}
	return ((ImmuSet) result.value()).with(test);
/*
udanax-top.st:66533:Filter class methodsFor: 'private: functions'!
{ImmuSet of: Filter} combineUnion: set {ImmuSet of: Filter} with.Filter: filter {Filter}
	"keep going around doing unions until there are no more special unions"
	| subs {Stepper} nonspecial {MuSet of: Filter} result {SetAccumulator} test {Filter} |
	result _ SetAccumulator make.
	test _ filter.
	subs _ set stepper.
	nonspecial _ set asMuSet.
	[subs hasValue] whileTrue:
		[ | special {Filter} sub {Filter} |
		sub _ (subs fetch cast: Filter).
		subs step.
		special _ (sub fetchUnion: test) cast: Filter.
		special ~~ NULL ifTrue:
			[test _ special.
			result _ SetAccumulator make.
			nonspecial remove: sub.
			subs _ nonspecial stepper]
		ifFalse:
			[ | canon {Pair of: Filter} |
			canon _ sub fetchPairUnion: test.
			canon == NULL ifTrue:
				[result step: sub]
			ifFalse:
				[test _ canon right cast: Filter.
				nonspecial remove: sub.
				result _ SetAccumulator make.
				nonspecial _ (Filter combineUnion: nonspecial asImmuSet
					with.Filter: (canon left cast: Filter)) asMuSet.
				subs _ nonspecial stepper]]].
	^(result value cast: ImmuSet) with: test!
*/
}
public static ImmuSet combineUnion(ImmuSet a, ImmuSet b) {
	ImmuSet result;
	result = a;
	Stepper stomper = b.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		result = combineUnion(result, sub);
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:66564:Filter class methodsFor: 'private: functions'!
{ImmuSet of: Filter} combineUnion: a {ImmuSet of: Filter} with.ImmuSet: b {ImmuSet of: Filter}
	| result {ImmuSet of: Filter} |
	result _ a.
	b stepper forEach: [ :sub {Filter} |
		result _ self combineUnion: result with.Filter: sub].
	^result!
*/
}
/**
 * distribute the intersection of a filter with the union of a set of filters
 */
public static ImmuSet distributeIntersect(CoordinateSpace cs, ImmuSet set, Filter filter) {
	Filter special;
	SetAccumulator nonspecial;
	special = Filter.closedFilter(cs);
	nonspecial = SetAccumulator.make();
	Stepper stomper = set.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		XnRegion quick;
		quick = sub.fetchIntersect(filter);
		if (quick == null) {
			nonspecial.step((sub.complexIntersect(filter)));
		}
		else {
			special = (Filter) (special.unionWith(quick));
		}
	}
	stomper.destroy();
	if (((ImmuSet) nonspecial.value()).isEmpty()) {
		return ImmuSet.make().with(special);
	}
	else {
		return ((ImmuSet) nonspecial.value()).with(special);
	}
/*
udanax-top.st:66571:Filter class methodsFor: 'private: functions'!
{ImmuSet of: Filter} distributeIntersect: cs {CoordinateSpace} with: set {ImmuSet of: Filter} with.Filter: filter {Filter}
	"distribute the intersection of a filter with the union of a set of filters"
	| special {Filter} nonspecial {SetAccumulator of: Filter} |
	special _ Filter closedFilter: cs.
	nonspecial _ SetAccumulator make.
	set stepper forEach: [ :sub {Filter} | | quick {XnRegion} |
		quick _ sub fetchIntersect: filter.
		quick == NULL
			ifTrue: [nonspecial step: (sub complexIntersect: filter)]
			ifFalse: [special _ (special unionWith: quick) cast: Filter]].
	(nonspecial value cast: ImmuSet) isEmpty
		ifTrue: [^ImmuSet make with: special]
		ifFalse: [^(nonspecial value cast: ImmuSet) with: special]!
*/
}
/**
 * distribute the intersection of two unions of sets of filters
 */
public static ImmuSet distributeIntersect(CoordinateSpace cs, ImmuSet a, ImmuSet b) {
	Filter special;
	ImmuSet nonspecial;
	special = Filter.closedFilter(cs);
	nonspecial = ImmuSet.make();
	Stepper stomper = a.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter other = (Filter) stomper.fetch();
		if (other == null) {
			continue ;
		}
		Stepper stomper2 = b.stepper();
		for (; stomper2.hasValue(); stomper2.step()) {
			Filter sub = (Filter) stomper2.fetch();
			if (sub == null) {
				continue ;
			}
			XnRegion intersection;
			intersection = sub.fetchIntersect(other);
			if (intersection == null) {
				nonspecial = Filter.combineUnion(nonspecial, ((Filter) (sub.complexIntersect(other))));
			}
			else {
				special = (Filter) (special.unionWith(intersection));
			}
		}
		stomper2.destroy();
	}
	stomper.destroy();
	return Filter.combineUnion(nonspecial, special);
/*
udanax-top.st:66585:Filter class methodsFor: 'private: functions'!
{ImmuSet of: Filter} distributeIntersect: cs {CoordinateSpace} 
	with: a {ImmuSet of: Filter} 
	with.ImmuSet: b {ImmuSet of: Filter}
	"distribute the intersection of two unions of sets of filters"
	| special {Filter} nonspecial {ImmuSet of: Filter} |
	special _ Filter closedFilter: cs.
	nonspecial _ ImmuSet make.
	a stepper forEach: [ :other {Filter} |
		b stepper forEach: [ :sub {Filter} | | intersection {XnRegion} |
			intersection _ sub fetchIntersect: other.
			intersection == NULL
				ifTrue: [nonspecial _ Filter combineUnion: nonspecial
					with.Filter: ((sub complexIntersect: other) cast: Filter)]
				ifFalse: [special _ (special unionWith: intersection) cast: Filter]]].
	^Filter combineUnion: nonspecial with.Filter: special!
*/
}
/**
 * distribute the union of a filter with the intersection of a set of filters
 */
public static ImmuSet distributeUnion(CoordinateSpace cs, ImmuSet set, Filter filter) {
	Filter special;
	ImmuSet nonspecial;
	special = Filter.openFilter(cs);
	nonspecial = ImmuSet.make();
	Stepper stomper = set.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		XnRegion quick;
		quick = sub.fetchUnion(filter);
		if (quick == null) {
			nonspecial = Filter.combineIntersect(nonspecial, sub);
		}
		else {
			special = (Filter) (special.intersect(quick));
		}
	}
	stomper.destroy();
	if (nonspecial.isEmpty()) {
		return ImmuSet.make().with(special);
	}
	else {
		return (ImmuSet.make().with((Filter.andFilterPrivate(((FilterSpace) cs), (Filter.combineIntersect(nonspecial, special)))))).with(filter);
	}
/*
udanax-top.st:66601:Filter class methodsFor: 'private: functions'!
{ImmuSet of: Filter} distributeUnion: cs {CoordinateSpace} with: set {ImmuSet of: Filter} with.Filter: filter {Filter}
	"distribute the union of a filter with the intersection of a set of filters"
	| special {Filter} nonspecial {ImmuSet of: Filter} |
	special _ Filter openFilter: cs.
	nonspecial _ ImmuSet make.
	set stepper forEach: [ :sub {Filter} | | quick {XnRegion} |
		quick _ sub fetchUnion: filter.
		quick == NULL
			ifTrue: [nonspecial _ Filter combineIntersect: nonspecial with.Filter: sub]
			ifFalse: [special _ (special intersect: quick) cast: Filter]].
	nonspecial isEmpty
		ifTrue: [^ImmuSet make with: special]
		ifFalse: [^(ImmuSet make
			with: (Filter andFilterPrivate: (cs cast: FilterSpace)
				with: (Filter combineIntersect: nonspecial with.Filter: special)))
			with: filter]!
*/
}
/**
 * distribute the union of an intersection and a union of sets of filters
 */
public static ImmuSet distributeUnion(CoordinateSpace cs, ImmuSet anded, ImmuSet ored) {
	Filter distributed;
	distributed = (Filter) ored.theOne();
	return combineUnion((distributeUnion(cs, anded, distributed)), (ored.without(distributed)));
/*
udanax-top.st:66618:Filter class methodsFor: 'private: functions'!
{ImmuSet of: Filter} distributeUnion: cs {CoordinateSpace} with: anded {ImmuSet of: Filter} with.ImmuSet: ored {ImmuSet of: Filter}
	"distribute the union of an intersection and a union of sets of filters"
	| distributed {Filter} |
	distributed _ ored theOne cast: Filter.
	^self combineUnion: (self distributeUnion: cs with: anded with.Filter: distributed)
		with.ImmuSet: (ored without: distributed)!
*/
}
/**
 * assumes that the interactions between elements have already been removed
 */
public static Filter orFilterPrivate(FilterSpace cs, ImmuSet subs) {
	if (subs.isEmpty()) {
		return Filter.closedFilter(cs);
	}
	if (subs.count() == 1) {
		return (Filter) subs.theOne();
	}
	return new OrFilter(cs, subs);
/*
udanax-top.st:66625:Filter class methodsFor: 'private: functions'!
{Filter} orFilterPrivate: cs {FilterSpace} with: subs {ImmuSet of: Filter}
	"assumes that the interactions between elements have already been removed"
	subs isEmpty ifTrue: [^Filter closedFilter: cs].
	subs count = 1 ifTrue: [^subs theOne cast: Filter].
	^OrFilter create: cs with: subs!
*/
}
/**
 * {XnRegion CLIENT} baseRegion
 * {Stepper CLIENT of: Filter} intersectedFilters
 * {BooleanVar CLIENT} isAllFilter
 * {BooleanVar CLIENT} isAnyFilter
 * {BooleanVar CLIENT} match: region {XnRegion}
 * {Stepper CLIENT of: Filter} unionedFilters
 */
public static void infostProtocol() {
/*
udanax-top.st:66633:Filter class methodsFor: 'smalltalk: system'!
info.stProtocol
"{XnRegion CLIENT} baseRegion
{Stepper CLIENT of: Filter} intersectedFilters
{BooleanVar CLIENT} isAllFilter
{BooleanVar CLIENT} isAnyFilter
{BooleanVar CLIENT} match: region {XnRegion}
{Stepper CLIENT of: Filter} unionedFilters
"!
*/
}
public Filter() {
/*

Generated during transformation
*/
}
}
