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
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.filter.Joint;
import info.dgjones.abora.gold.filter.OrFilter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class OrFilter extends Filter {

	protected ImmuSet mySubFilters;
/*
udanax-top.st:67342:
Filter subclass: #OrFilter
	instanceVariableNames: 'mySubFilters {ImmuSet of: Filter}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Filter'!
*/
/*
udanax-top.st:67346:
(OrFilter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OrFilter.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public OrFilter(FilterSpace cs, ImmuSet subs) {
	super(cs);
	mySubFilters = subs;
/*
udanax-top.st:67351:OrFilter methodsFor: 'creation'!
create: cs {FilterSpace} with: subs {ImmuSet of: Filter}
	super create: cs.
	mySubFilters _ subs!
*/
}
public int actualHashForEqual() {
	return (coordinateSpace().hashForEqual() ^ mySubFilters.hashForEqual()) ^ HashHelper.hashForEqual(this.getClass());
/*
udanax-top.st:67357:OrFilter methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(self coordinateSpace hashForEqual bitXor: mySubFilters hashForEqual)
		bitXor: #cat.U.OrFilter hashForEqual!
*/
}
public boolean isAllFilter() {
	return false;
/*
udanax-top.st:67361:OrFilter methodsFor: 'testing'!
{BooleanVar} isAllFilter
	^false!
*/
}
public boolean isAnyFilter() {
	return false;
/*
udanax-top.st:67365:OrFilter methodsFor: 'testing'!
{BooleanVar} isAnyFilter
	
	^false!
*/
}
public boolean isEmpty() {
	return false;
/*
udanax-top.st:67369:OrFilter methodsFor: 'testing'!
{BooleanVar} isEmpty
	^false!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof OrFilter) {
		OrFilter of = (OrFilter) other;
		return of.subFilters().isEqual(subFilters());
	}
	else {
		return false;
	}
/*
udanax-top.st:67372:OrFilter methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: OrFilter into: [:of |
			^of subFilters isEqual: self subFilters]
		others: [^false].
	^false "fodder"!
*/
}
public boolean isFull() {
	return false;
/*
udanax-top.st:67380:OrFilter methodsFor: 'testing'!
{BooleanVar} isFull	
	^false!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	subFilters().printOnWithSimpleSyntax(oo, "(", " || ", ")");
/*
udanax-top.st:67386:OrFilter methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name.
	self subFilters printOnWithSimpleSyntax: oo with: '(' with: ' || ' with: ')'!
*/
}
public XnRegion complement() {
	XnRegion result;
	result = Filter.openFilter(coordinateSpace());
	Stepper stomper = subFilters().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion sub = (XnRegion) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		result = result.intersect(sub.complement());
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:67392:OrFilter methodsFor: 'operations'!
{XnRegion} complement
	| result {XnRegion} |
	result _ Filter openFilter: self coordinateSpace.
	self subFilters stepper forEach: [ :sub {XnRegion} |
		result _ result intersect: sub complement].
	^result!
*/
}
/**
 * tell whether a region passes this filter
 */
public boolean match(XnRegion region) {
	Stepper stomper = subFilters().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		if (sub.match(region)) {
			return true;
		}
	}
	stomper.destroy();
	return false;
/*
udanax-top.st:67402:OrFilter methodsFor: 'filtering'!
{BooleanVar} match: region {XnRegion}
	"tell whether a region passes this filter"
	self subFilters stepper forEach: [ :sub {Filter} |
		(sub match: region)
			ifTrue: [^true]].
	^false!
*/
}
/**
 * return the simplest filter for looking at the children
 */
public Filter pass(Joint parent) {
	XnRegion result;
	result = Filter.closedFilter(coordinateSpace());
	Stepper stomper = subFilters().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		result = result.unionWith((sub.pass(parent)));
	}
	stomper.destroy();
	return (Filter) result;
/*
udanax-top.st:67409:OrFilter methodsFor: 'filtering'!
{Filter} pass: parent {Joint}
	"return the simplest filter for looking at the children"
	| result {XnRegion} |
	result _ Filter closedFilter: self coordinateSpace.
	self subFilters stepper forEach: [ :sub {Filter} |
		result _ result unionWith: (sub pass: parent)].
	^result cast: Filter!
*/
}
public ImmuSet subFilters() {
	return mySubFilters;
/*
udanax-top.st:67417:OrFilter methodsFor: 'filtering'!
{ImmuSet of: Filter} subFilters
	^mySubFilters!
*/
}
/**
 * return self or other if one is clearly a subset of the other, else NULL
 */
public XnRegion fetchSpecialSubset(XnRegion other) {
	Filter filter;
	XnRegion defaultRegion;
	filter = (Filter) other;
	defaultRegion = this;
	Stepper stomper = subFilters().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter subfilter = (Filter) stomper.fetch();
		if (subfilter == null) {
			continue ;
		}
		XnRegion a;
		XnRegion b;
		if (((Heaper) (a = subfilter.fetchSpecialSubset(filter))) == other) {
			return other;
		}
		if (((Heaper) (b = filter.fetchSpecialSubset(subfilter))) == other) {
			return other;
		}
		if ( ! ((a) == other || ((b) == other))) {
			defaultRegion = null;
		}
	}
	stomper.destroy();
	return defaultRegion;
/*
udanax-top.st:67422:OrFilter methodsFor: 'protected: protected operations'!
{XnRegion} fetchSpecialSubset: other {XnRegion}
	"return self or other if one is clearly a subset of the other, else NULL"
	| filter {Filter} defaultRegion {XnRegion} |
	filter _ other cast: Filter.
	defaultRegion _ self.
	self subFilters stepper forEach: [ :subfilter {Filter} | 
		| a {XnRegion} b {XnRegion} |
		((a _ subfilter fetchSpecialSubset: filter) basicCast: Heaper star) == other ifTrue: [^other].
		((b _ filter fetchSpecialSubset: subfilter) basicCast: Heaper star) == other ifTrue: [^other].
		((a basicCast: Heaper star) == other or: [(b basicCast: Heaper star) == other]) ifFalse: [defaultRegion _ NULL]].
	^defaultRegion!
*/
}
public Stepper intersectedFilters() {
	return Stepper.itemStepper(this);
/*
udanax-top.st:67436:OrFilter methodsFor: 'enumerating'!
{Stepper of: Filter} intersectedFilters
	^Stepper itemStepper: self!
*/
}
public Stepper unionedFilters() {
	return mySubFilters.stepper();
/*
udanax-top.st:67440:OrFilter methodsFor: 'enumerating'!
{Stepper of: Filter} unionedFilters
	^mySubFilters stepper!
*/
}
public XnRegion baseRegion() {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_SIMPLE_ENOUGH);
/*
udanax-top.st:67446:OrFilter methodsFor: 'accessing'!
{XnRegion} baseRegion
	Heaper BLAST: #NotSimpleEnough.
	^NULL!
*/
}
public XnRegion relevantRegion() {
	XnRegion result;
	result = filterSpace().baseSpace().emptyRegion();
	Stepper stomper = mySubFilters.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		result = result.unionWith(sub.relevantRegion());
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:67451:OrFilter methodsFor: 'accessing'!
{XnRegion} relevantRegion
	| result {XnRegion} |
	result := self filterSpace baseSpace emptyRegion.
	mySubFilters stepper forEach: [ :sub {Filter} |
		result := result unionWith: sub relevantRegion].
	^result!
*/
}
public OrFilter(Rcvr receiver) {
	super(receiver);
	mySubFilters = (ImmuSet) receiver.receiveHeaper();
/*
udanax-top.st:67461:OrFilter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	mySubFilters _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(mySubFilters);
/*
udanax-top.st:67465:OrFilter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: mySubFilters.!
*/
}
public OrFilter() {
/*

Generated during transformation
*/
}
}
