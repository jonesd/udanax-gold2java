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
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.filter.Joint;
import info.dgjones.abora.gold.filter.NotSubsetFilter;
import info.dgjones.abora.gold.filter.SubsetFilter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class NotSubsetFilter extends Filter {

	protected XnRegion myRegion;
/*
udanax-top.st:66910:
Filter subclass: #NotSubsetFilter
	instanceVariableNames: 'myRegion {XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Filter'!
*/
/*
udanax-top.st:66914:
(NotSubsetFilter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(NotSubsetFilter.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * tell whether a region passes this filter
 */
public boolean match(XnRegion region) {
	return ! (region.isSubsetOf(myRegion));
/*
udanax-top.st:66919:NotSubsetFilter methodsFor: 'filtering'!
{BooleanVar} match: region {XnRegion}
	"tell whether a region passes this filter"
	^(region isSubsetOf: myRegion) not!
*/
}
/**
 * return the simplest filter for looking at the children
 */
public Filter pass(Joint parent) {
	if ( ! (parent.intersected().isSubsetOf(myRegion))) {
		return Filter.openFilter(coordinateSpace());
	}
	if (parent.unioned().isSubsetOf(myRegion)) {
		return Filter.closedFilter(coordinateSpace());
	}
	return this;
/*
udanax-top.st:66923:NotSubsetFilter methodsFor: 'filtering'!
{Filter} pass: parent {Joint}
	"return the simplest filter for looking at the children"
	(parent intersected isSubsetOf: myRegion)
		ifFalse: [^Filter openFilter: self coordinateSpace].
	(parent unioned isSubsetOf: myRegion)
		ifTrue: [^Filter closedFilter: self coordinateSpace].
	^self!
*/
}
public XnRegion region() {
	return myRegion;
/*
udanax-top.st:66931:NotSubsetFilter methodsFor: 'filtering'!
{XnRegion} region
	^myRegion!
*/
}
public int actualHashForEqual() {
	return (coordinateSpace().hashForEqual() ^ myRegion.hashForEqual()) ^ HashHelper.hashForEqual(this.getClass());
/*
udanax-top.st:66936:NotSubsetFilter methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(self coordinateSpace hashForEqual bitXor: myRegion hashForEqual)
		bitXor: #cat.U.NotSubsetFilter hashForEqual!
*/
}
public boolean isAllFilter() {
	return false;
/*
udanax-top.st:66940:NotSubsetFilter methodsFor: 'testing'!
{BooleanVar} isAllFilter
	^false!
*/
}
public boolean isAnyFilter() {
	return true;
/*
udanax-top.st:66944:NotSubsetFilter methodsFor: 'testing'!
{BooleanVar} isAnyFilter
	
	^true!
*/
}
public boolean isEmpty() {
	return false;
/*
udanax-top.st:66948:NotSubsetFilter methodsFor: 'testing'!
{BooleanVar} isEmpty
	^false!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof NotSubsetFilter) {
		NotSubsetFilter nsf = (NotSubsetFilter) other;
		return nsf.region().isEqual(myRegion);
	}
	else {
		return false;
	}
/*
udanax-top.st:66951:NotSubsetFilter methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: NotSubsetFilter into: [:nsf |
			^nsf region isEqual: myRegion]
		others: [^false].
	^false "fodder"!
*/
}
public boolean isFull() {
	return false;
/*
udanax-top.st:66959:NotSubsetFilter methodsFor: 'testing'!
{BooleanVar} isFull	
	^false!
*/
}
public XnRegion complement() {
	return Filter.subsetFilter(coordinateSpace(), myRegion);
/*
udanax-top.st:66965:NotSubsetFilter methodsFor: 'operations'!
{XnRegion} complement
	^Filter subsetFilter: self coordinateSpace with: myRegion!
*/
}
public NotSubsetFilter(FilterSpace cs, XnRegion region) {
	super(cs);
	myRegion = region;
/*
udanax-top.st:66971:NotSubsetFilter methodsFor: 'creation'!
create: cs {FilterSpace} with: region {XnRegion}
	super create: cs.
	myRegion _ region!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("IntersectionFilter(");
	oo.print(myRegion.complement());
	oo.print(")");
/*
udanax-top.st:66977:NotSubsetFilter methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << 'IntersectionFilter(' << myRegion complement << ')'!
*/
}
public XnRegion fetchSpecialIntersect(XnRegion other) {
	if (other instanceof SubsetFilter) {
		SubsetFilter sf = (SubsetFilter) other;
		if (sf.region().isSubsetOf(myRegion)) {
			return Filter.closedFilter(coordinateSpace());
		}
		else {
			return null;
		}
	}
	else {
		return null;
	}
/*
udanax-top.st:66982:NotSubsetFilter methodsFor: 'protected operations'!
{XnRegion} fetchSpecialIntersect: other {XnRegion}
	other
		cast: SubsetFilter into: [:sf |
			(sf region isSubsetOf: myRegion)
				ifTrue: [^Filter closedFilter: self coordinateSpace]
				ifFalse: [^NULL]]
		others: [^NULL].
	^NULL "fodder"!
*/
}
public XnRegion fetchSpecialSubset(XnRegion other) {
	if (other instanceof NotSubsetFilter) {
		NotSubsetFilter nsf = (NotSubsetFilter) other;
		XnRegion others;
		others = nsf.region();
		if (others.isSubsetOf(myRegion)) {
			return this;
		}
		if (myRegion.isSubsetOf(others)) {
			return other;
		}
	}
	return null;
/*
udanax-top.st:66992:NotSubsetFilter methodsFor: 'protected operations'!
{XnRegion} fetchSpecialSubset: other {XnRegion}
	other
		cast: NotSubsetFilter into: [:nsf |
			| others {XnRegion} |
			others _ nsf region.
			(others isSubsetOf: myRegion) ifTrue: [^self].
			(myRegion isSubsetOf: others) ifTrue: [^other]]
		others: [].
	^NULL!
*/
}
public XnRegion fetchSpecialUnion(XnRegion other) {
	if (other instanceof SubsetFilter) {
		SubsetFilter sf = (SubsetFilter) other;
		if (myRegion.isSubsetOf(sf.region())) {
			return Filter.openFilter(coordinateSpace());
		}
		else {
			return null;
		}
	}
	else if (other instanceof NotSubsetFilter) {
		NotSubsetFilter nsf = (NotSubsetFilter) other;
		return Filter.notSubsetFilter(coordinateSpace(), (myRegion.intersect(nsf.region())));
	}
	else {
		return null;
	}
/*
udanax-top.st:67003:NotSubsetFilter methodsFor: 'protected operations'!
{XnRegion} fetchSpecialUnion: other {XnRegion}
	other
		cast: SubsetFilter into: [:sf |
			(myRegion isSubsetOf: sf region)
				ifTrue: [^Filter openFilter: self coordinateSpace]
				ifFalse: [^NULL]]
		cast: NotSubsetFilter into: [:nsf |
			^Filter notSubsetFilter: self coordinateSpace
				with: (myRegion intersect: nsf region)]
		others: [^NULL].
	^NULL "fodder"!
*/
}
public Stepper intersectedFilters() {
	return Stepper.itemStepper(this);
/*
udanax-top.st:67018:NotSubsetFilter methodsFor: 'enumerating'!
{Stepper of: Filter} intersectedFilters
	^Stepper itemStepper: self!
*/
}
public Stepper unionedFilters() {
	return Stepper.itemStepper(this);
/*
udanax-top.st:67022:NotSubsetFilter methodsFor: 'enumerating'!
{Stepper of: Filter} unionedFilters
	^Stepper itemStepper: self!
*/
}
public XnRegion baseRegion() {
	return myRegion.complement();
/*
udanax-top.st:67028:NotSubsetFilter methodsFor: 'accessing'!
{XnRegion} baseRegion
	^myRegion complement!
*/
}
public XnRegion relevantRegion() {
	return myRegion.complement();
/*
udanax-top.st:67032:NotSubsetFilter methodsFor: 'accessing'!
{XnRegion} relevantRegion
	^myRegion complement!
*/
}
public NotSubsetFilter(Rcvr receiver) {
	super(receiver);
	myRegion = (XnRegion) receiver.receiveHeaper();
/*
udanax-top.st:67038:NotSubsetFilter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myRegion _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myRegion);
/*
udanax-top.st:67042:NotSubsetFilter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myRegion.!
*/
}
public NotSubsetFilter() {
/*

Generated during transformation
*/
}
}
