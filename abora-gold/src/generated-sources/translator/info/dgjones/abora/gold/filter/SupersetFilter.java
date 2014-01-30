/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.filter;

import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.filter.Joint;
import info.dgjones.abora.gold.filter.NotSubsetFilter;
import info.dgjones.abora.gold.filter.SubsetFilter;
import info.dgjones.abora.gold.filter.SupersetFilter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class SupersetFilter extends Filter {

	protected XnRegion myRegion;
/*
udanax-top.st:67597:
Filter subclass: #SupersetFilter
	instanceVariableNames: 'myRegion {XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Filter'!
*/
/*
udanax-top.st:67601:
(SupersetFilter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SupersetFilter.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * tell whether a region passes this filter
 */
public boolean match(XnRegion region) {
	return myRegion.isSubsetOf(region);
/*
udanax-top.st:67606:SupersetFilter methodsFor: 'filtering'!
{BooleanVar} match: region {XnRegion}
	"tell whether a region passes this filter"
	^myRegion isSubsetOf: region!
*/
}
/**
 * return the simplest filter for looking at the children
 */
public Filter pass(Joint parent) {
	if (myRegion.isSubsetOf(parent.intersected())) {
		return Filter.openFilter(coordinateSpace());
	}
	if ( ! (myRegion.isSubsetOf(parent.unioned()))) {
		return Filter.closedFilter(coordinateSpace());
	}
	return this;
/*
udanax-top.st:67610:SupersetFilter methodsFor: 'filtering'!
{Filter} pass: parent {Joint}
	"return the simplest filter for looking at the children"
	(myRegion isSubsetOf: parent intersected) ifTrue:
		[^Filter openFilter: self coordinateSpace].
	(myRegion isSubsetOf: parent unioned) ifFalse:
		[^Filter closedFilter: self coordinateSpace].
	^self!
*/
}
public XnRegion region() {
	return myRegion;
/*
udanax-top.st:67618:SupersetFilter methodsFor: 'filtering'!
{XnRegion} region
	^myRegion!
*/
}
public XnRegion complement() {
	return Filter.notSupersetFilter(coordinateSpace(), myRegion);
/*
udanax-top.st:67623:SupersetFilter methodsFor: 'operations'!
{XnRegion} complement
	^Filter notSupersetFilter: self coordinateSpace with: myRegion!
*/
}
public int actualHashForEqual() {
	return (coordinateSpace().hashForEqual() ^ myRegion.hashForEqual()) ^ HashHelper.hashForEqual(this.getClass());
/*
udanax-top.st:67629:SupersetFilter methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(self coordinateSpace hashForEqual bitXor: myRegion hashForEqual)
		bitXor: #cat.U.SupersetFilter hashForEqual!
*/
}
public boolean isAllFilter() {
	return true;
/*
udanax-top.st:67633:SupersetFilter methodsFor: 'testing'!
{BooleanVar} isAllFilter
	^true!
*/
}
public boolean isAnyFilter() {
	return false;
/*
udanax-top.st:67637:SupersetFilter methodsFor: 'testing'!
{BooleanVar} isAnyFilter
	
	^false!
*/
}
public boolean isEmpty() {
	return false;
/*
udanax-top.st:67641:SupersetFilter methodsFor: 'testing'!
{BooleanVar} isEmpty
	^false!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof SupersetFilter) {
		SupersetFilter ssf = (SupersetFilter) other;
		return ssf.region().isEqual(myRegion);
	}
	else {
		return false;
	}
/*
udanax-top.st:67644:SupersetFilter methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: SupersetFilter into: [:ssf |
			^ssf region isEqual: myRegion]
		others: [^false].
	^false "fodder"!
*/
}
public boolean isFull() {
	return false;
/*
udanax-top.st:67652:SupersetFilter methodsFor: 'testing'!
{BooleanVar} isFull	
	^false!
*/
}
public XnRegion fetchSpecialUnion(XnRegion other) {
	return null;
/*
udanax-top.st:67658:SupersetFilter methodsFor: 'protected: protected operations'!
{XnRegion} fetchSpecialUnion: other {XnRegion unused}
	^NULL!
*/
}
public SupersetFilter(FilterSpace cs, XnRegion region) {
	super(cs);
	myRegion = region;
/*
udanax-top.st:67663:SupersetFilter methodsFor: 'creation'!
create: cs {FilterSpace} with: region {XnRegion}
	super create: cs.
	myRegion _ region!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myRegion);
	oo.print(")");
/*
udanax-top.st:67669:SupersetFilter methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myRegion << ')'!
*/
}
/**
 * return NULL, or the pair of canonical filters (left == new1 | self, right == new2 | other)
 */
public Pair fetchCanonicalUnion(Filter other) {
	if (other instanceof NotSubsetFilter) {
		NotSubsetFilter nsf = (NotSubsetFilter) other;
		XnRegion others;
		others = nsf.region();
		if ( ! (myRegion.isSubsetOf(others))) {
			return Pair.make((Filter.supersetFilter(coordinateSpace(), (myRegion.intersect(nsf.region())))), other);
		}
	}
	return null;
/*
udanax-top.st:67674:SupersetFilter methodsFor: 'protected operations'!
{Pair of: Filter} fetchCanonicalUnion: other {Filter}
	"return NULL, or the pair of canonical filters (left == new1 | self, right == new2 | other)"
	
	other
		cast: NotSubsetFilter into: [:nsf |
			| others {XnRegion} |
			others _ nsf region.
			(myRegion isSubsetOf: others)
				ifFalse: [^Pair make: (Filter supersetFilter: self coordinateSpace
						with: (myRegion intersect: nsf region))
					with: other]]
		others: [].
	^NULL!
*/
}
public XnRegion fetchSpecialIntersect(XnRegion other) {
	if (other instanceof SubsetFilter) {
		SubsetFilter subF = (SubsetFilter) other;
		if ( ! (myRegion.isSubsetOf(subF.region()))) {
			return Filter.closedFilter(coordinateSpace());
		}
	}
	else if (other instanceof SupersetFilter) {
		SupersetFilter superF = (SupersetFilter) other;
		return Filter.supersetFilter(coordinateSpace(), (myRegion.unionWith(superF.region())));
	}
	return null;
/*
udanax-top.st:67688:SupersetFilter methodsFor: 'protected operations'!
{XnRegion} fetchSpecialIntersect: other {XnRegion}
	other
		cast: SubsetFilter into: [:subF |
			(myRegion isSubsetOf: subF region)
				ifFalse: [^Filter closedFilter: self coordinateSpace]]
		cast: SupersetFilter into: [:superF |
			^Filter supersetFilter: self coordinateSpace
				with: (myRegion unionWith: superF region)]
		others: [].
	^NULL!
*/
}
public XnRegion fetchSpecialSubset(XnRegion other) {
	if (other instanceof NotSubsetFilter) {
		NotSubsetFilter nSubF = (NotSubsetFilter) other;
		if ( ! (myRegion.isSubsetOf(nSubF.region()))) {
			return this;
		}
	}
	else if (other instanceof SupersetFilter) {
		SupersetFilter superF = (SupersetFilter) other;
		XnRegion others;
		others = superF.region();
		if (myRegion.isSubsetOf(others)) {
			return other;
		}
		if (others.isSubsetOf(myRegion)) {
			return this;
		}
	}
	return null;
/*
udanax-top.st:67700:SupersetFilter methodsFor: 'protected operations'!
{XnRegion} fetchSpecialSubset: other {XnRegion}
	other
		cast: NotSubsetFilter  into: [:nSubF |
			(myRegion isSubsetOf: nSubF region)
				ifFalse: [^self]]
		cast: SupersetFilter into: [:superF |
			| others {XnRegion} |
			others _ superF region.
			(myRegion isSubsetOf: others) ifTrue: [^other].
			(others isSubsetOf: myRegion) ifTrue: [^self]]
		others: [].
	^NULL!
*/
}
public Stepper intersectedFilters() {
	return Stepper.itemStepper(this);
/*
udanax-top.st:67716:SupersetFilter methodsFor: 'enumerating'!
{Stepper of: Filter} intersectedFilters
	^Stepper itemStepper: self!
*/
}
public Stepper unionedFilters() {
	return Stepper.itemStepper(this);
/*
udanax-top.st:67720:SupersetFilter methodsFor: 'enumerating'!
{Stepper of: Filter} unionedFilters
	^Stepper itemStepper: self!
*/
}
public XnRegion baseRegion() {
	return myRegion;
/*
udanax-top.st:67726:SupersetFilter methodsFor: 'accessing'!
{XnRegion} baseRegion
	^myRegion!
*/
}
public XnRegion relevantRegion() {
	return myRegion;
/*
udanax-top.st:67730:SupersetFilter methodsFor: 'accessing'!
{XnRegion} relevantRegion
	^myRegion!
*/
}
public SupersetFilter(Rcvr receiver) {
	super(receiver);
	myRegion = (XnRegion) receiver.receiveHeaper();
/*
udanax-top.st:67736:SupersetFilter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myRegion _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myRegion);
/*
udanax-top.st:67740:SupersetFilter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myRegion.!
*/
}
public SupersetFilter() {
/*

Generated during transformation
*/
}
}
