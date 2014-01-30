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
import info.dgjones.abora.gold.filter.OpenFilter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class OpenFilter extends Filter {

/*
udanax-top.st:67226:
Filter subclass: #OpenFilter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Filter'!
*/
/*
udanax-top.st:67230:
(OpenFilter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
/*
udanax-top.st:67330:
OpenFilter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:67333:
(OpenFilter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OpenFilter.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public XnRegion complement() {
	return Filter.closedFilter(coordinateSpace());
/*
udanax-top.st:67235:OpenFilter methodsFor: 'operations'!
{XnRegion} complement
	^Filter closedFilter: self coordinateSpace!
*/
}
public XnRegion intersect(XnRegion other) {
	return other;
/*
udanax-top.st:67239:OpenFilter methodsFor: 'operations'!
{XnRegion} intersect: other {XnRegion}
	^other!
*/
}
public XnRegion minus(XnRegion other) {
	return other.complement();
/*
udanax-top.st:67242:OpenFilter methodsFor: 'operations'!
{XnRegion} minus: other {XnRegion}
	^other complement!
*/
}
public XnRegion unionWith(XnRegion other) {
	return this;
/*
udanax-top.st:67245:OpenFilter methodsFor: 'operations'!
{XnRegion} unionWith: other {XnRegion unused}
	^self!
*/
}
/**
 * tell whether a region passes this filter
 */
public boolean match(XnRegion region) {
	return true;
/*
udanax-top.st:67250:OpenFilter methodsFor: 'filtering'!
{BooleanVar} match: region {XnRegion unused}
	"tell whether a region passes this filter"
	^true!
*/
}
/**
 * return the simplest filter for looking at the children
 */
public Filter pass(Joint parent) {
	return this;
/*
udanax-top.st:67254:OpenFilter methodsFor: 'filtering'!
{Filter} pass: parent {Joint unused}
	"return the simplest filter for looking at the children"
	^self!
*/
}
public int actualHashForEqual() {
	return coordinateSpace().hashForEqual() ^ HashHelper.hashForEqual(this.getClass());
/*
udanax-top.st:67260:OpenFilter methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self coordinateSpace hashForEqual bitXor: #cat.U.OpenFilter hashForEqual!
*/
}
public boolean isAllFilter() {
	return true;
/*
udanax-top.st:67263:OpenFilter methodsFor: 'testing'!
{BooleanVar} isAllFilter
	^true!
*/
}
public boolean isAnyFilter() {
	return false;
/*
udanax-top.st:67267:OpenFilter methodsFor: 'testing'!
{BooleanVar} isAnyFilter
	
	^false!
*/
}
public boolean isEmpty() {
	return false;
/*
udanax-top.st:67271:OpenFilter methodsFor: 'testing'!
{BooleanVar} isEmpty
	^false!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof OpenFilter) {
		OpenFilter of = (OpenFilter) other;
		return of.coordinateSpace().isEqual(coordinateSpace());
	}
	else {
		return false;
	}
/*
udanax-top.st:67274:OpenFilter methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: OpenFilter into: [:of |
			^of coordinateSpace isEqual: self coordinateSpace]
		others: [^false].
	^false "fodder"!
*/
}
public boolean isFull() {
	return true;
/*
udanax-top.st:67282:OpenFilter methodsFor: 'testing'!
{BooleanVar} isFull	
	^true!
*/
}
public OpenFilter(FilterSpace cs) {
	super(cs);
/*
udanax-top.st:67288:OpenFilter methodsFor: 'creation'!
create: cs {FilterSpace}
	super create: cs!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(coordinateSpace());
	oo.print(")");
/*
udanax-top.st:67293:OpenFilter methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << self coordinateSpace << ')'!
*/
}
public XnRegion fetchSpecialSubset(XnRegion other) {
	return other;
/*
udanax-top.st:67298:OpenFilter methodsFor: 'protected: protected operations'!
{XnRegion} fetchSpecialSubset: other {XnRegion}
	^other!
*/
}
public Stepper intersectedFilters() {
	return Stepper.emptyStepper();
/*
udanax-top.st:67303:OpenFilter methodsFor: 'enumerating'!
{Stepper of: Filter} intersectedFilters
	^Stepper emptyStepper!
*/
}
public Stepper unionedFilters() {
	return Stepper.itemStepper(this);
/*
udanax-top.st:67307:OpenFilter methodsFor: 'enumerating'!
{Stepper of: Filter} unionedFilters
	^Stepper itemStepper: self!
*/
}
public XnRegion baseRegion() {
	return ((FilterSpace) coordinateSpace()).emptyRegion();
/*
udanax-top.st:67313:OpenFilter methodsFor: 'accessing'!
{XnRegion} baseRegion
	^(self coordinateSpace cast: FilterSpace) emptyRegion!
*/
}
public XnRegion relevantRegion() {
	return filterSpace().baseSpace().emptyRegion();
/*
udanax-top.st:67317:OpenFilter methodsFor: 'accessing'!
{XnRegion} relevantRegion
	^self filterSpace baseSpace emptyRegion!
*/
}
public OpenFilter(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:67323:OpenFilter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:67326:OpenFilter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static Filter make(CoordinateSpace space) {
	return new OpenFilter(((FilterSpace) space));
/*
udanax-top.st:67338:OpenFilter class methodsFor: 'pseudo constructors'!
{Filter} make: space {CoordinateSpace}
	^self create: (space cast: FilterSpace)!
*/
}
public OpenFilter() {
/*

Generated during transformation
*/
}
}
