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
import info.dgjones.abora.gold.filter.SubsetFilter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class SubsetFilter extends Filter {

	protected XnRegion myRegion;
/*
udanax-top.st:67469:
Filter subclass: #SubsetFilter
	instanceVariableNames: 'myRegion {XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Filter'!
*/
/*
udanax-top.st:67473:
(SubsetFilter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SubsetFilter.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public SubsetFilter(FilterSpace cs, XnRegion region) {
	super(cs);
	myRegion = region;
/*
udanax-top.st:67478:SubsetFilter methodsFor: 'creation'!
create: cs {FilterSpace} with: region {XnRegion}
	super create: cs.
	myRegion _ region!
*/
}
public XnRegion complement() {
	return Filter.notSubsetFilter(coordinateSpace(), myRegion);
/*
udanax-top.st:67484:SubsetFilter methodsFor: 'operations'!
{XnRegion} complement
	^Filter notSubsetFilter: self coordinateSpace with: myRegion!
*/
}
/**
 * tell whether a region passes this filter
 */
public boolean match(XnRegion region) {
	return region.isSubsetOf(myRegion);
/*
udanax-top.st:67490:SubsetFilter methodsFor: 'filtering'!
{BooleanVar} match: region {XnRegion}
	"tell whether a region passes this filter"
	^region isSubsetOf: myRegion!
*/
}
/**
 * return the simplest filter for looking at the children
 */
public Filter pass(Joint parent) {
	if (parent.unioned().isSubsetOf(myRegion)) {
		return Filter.openFilter(coordinateSpace());
	}
	if ( ! (parent.intersected().isSubsetOf(myRegion))) {
		return Filter.closedFilter(coordinateSpace());
	}
	return this;
/*
udanax-top.st:67494:SubsetFilter methodsFor: 'filtering'!
{Filter} pass: parent {Joint}
	"return the simplest filter for looking at the children"
	(parent unioned isSubsetOf: myRegion) ifTrue:
		[^Filter openFilter: self coordinateSpace].
	(parent intersected isSubsetOf: myRegion) ifFalse:
		[^Filter closedFilter: self coordinateSpace].
	^self!
*/
}
public XnRegion region() {
	return myRegion;
/*
udanax-top.st:67502:SubsetFilter methodsFor: 'filtering'!
{XnRegion} region
	^myRegion!
*/
}
public int actualHashForEqual() {
	return (coordinateSpace().hashForEqual() ^ myRegion.hashForEqual()) ^ HashHelper.hashForEqual(this.getClass());
/*
udanax-top.st:67507:SubsetFilter methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(self coordinateSpace hashForEqual bitXor: myRegion hashForEqual)
		bitXor: #cat.U.SubsetFilter hashForEqual!
*/
}
public boolean isAllFilter() {
	return false;
/*
udanax-top.st:67511:SubsetFilter methodsFor: 'testing'!
{BooleanVar} isAllFilter
	^false!
*/
}
public boolean isAnyFilter() {
	return false;
/*
udanax-top.st:67515:SubsetFilter methodsFor: 'testing'!
{BooleanVar} isAnyFilter
	
	^false!
*/
}
public boolean isEmpty() {
	return false;
/*
udanax-top.st:67519:SubsetFilter methodsFor: 'testing'!
{BooleanVar} isEmpty
	^false!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof SubsetFilter) {
		SubsetFilter ssf = (SubsetFilter) other;
		return ssf.region().isEqual(myRegion);
	}
	else {
		return false;
	}
/*
udanax-top.st:67522:SubsetFilter methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: SubsetFilter into: [:ssf |
			^ssf region isEqual: myRegion]
		others: [^false].
	^false "fodder"!
*/
}
public boolean isFull() {
	return false;
/*
udanax-top.st:67530:SubsetFilter methodsFor: 'testing'!
{BooleanVar} isFull	
	^false!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myRegion);
	oo.print(")");
/*
udanax-top.st:67536:SubsetFilter methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myRegion << ')'!
*/
}
public XnRegion fetchSpecialUnion(XnRegion other) {
	return null;
/*
udanax-top.st:67541:SubsetFilter methodsFor: 'protected: protected operations'!
{XnRegion} fetchSpecialUnion: other {XnRegion unused}
	^NULL!
*/
}
public XnRegion fetchSpecialIntersect(XnRegion other) {
	if (other instanceof SubsetFilter) {
		SubsetFilter sf = (SubsetFilter) other;
		return Filter.subsetFilter(coordinateSpace(), (sf.region().intersect(myRegion)));
	}
	else {
		return null;
	}
/*
udanax-top.st:67546:SubsetFilter methodsFor: 'protected operations'!
{XnRegion} fetchSpecialIntersect: other {XnRegion}
	other
		cast: SubsetFilter into: [:sf |
			^Filter subsetFilter: self coordinateSpace
				with: (sf region intersect: myRegion)]
		others: [^NULL].
	^NULL "fodder"!
*/
}
public XnRegion fetchSpecialSubset(XnRegion other) {
	if (other instanceof SubsetFilter) {
		SubsetFilter sf = (SubsetFilter) other;
		XnRegion others;
		others = sf.region();
		if (others.isSubsetOf(myRegion)) {
			return other;
		}
		if (myRegion.isSubsetOf(others)) {
			return this;
		}
	}
	return null;
/*
udanax-top.st:67555:SubsetFilter methodsFor: 'protected operations'!
{XnRegion} fetchSpecialSubset: other {XnRegion}
	other
		cast: SubsetFilter into: [:sf |
			| others {XnRegion} |
			others _ sf region.
			(others isSubsetOf: myRegion) ifTrue: [^other].
			(myRegion isSubsetOf: others) ifTrue: [^self]]
		others: [].
	^NULL!
*/
}
public Stepper intersectedFilters() {
	return Stepper.itemStepper(this);
/*
udanax-top.st:67568:SubsetFilter methodsFor: 'enumerating'!
{Stepper of: Filter} intersectedFilters
	^Stepper itemStepper: self!
*/
}
public Stepper unionedFilters() {
	return Stepper.itemStepper(this);
/*
udanax-top.st:67572:SubsetFilter methodsFor: 'enumerating'!
{Stepper of: Filter} unionedFilters
	^Stepper itemStepper: self!
*/
}
public XnRegion baseRegion() {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_SIMPLE_ENOUGH);
/*
udanax-top.st:67578:SubsetFilter methodsFor: 'accessing'!
{XnRegion} baseRegion
	Heaper BLAST: #NotSimpleEnough.
	^NULL!
*/
}
public XnRegion relevantRegion() {
	return myRegion.complement();
/*
udanax-top.st:67583:SubsetFilter methodsFor: 'accessing'!
{XnRegion} relevantRegion
	^myRegion complement!
*/
}
public SubsetFilter(Rcvr receiver) {
	super(receiver);
	myRegion = (XnRegion) receiver.receiveHeaper();
/*
udanax-top.st:67589:SubsetFilter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myRegion _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myRegion);
/*
udanax-top.st:67593:SubsetFilter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myRegion.!
*/
}
public SubsetFilter() {
/*

Generated during transformation
*/
}
}
