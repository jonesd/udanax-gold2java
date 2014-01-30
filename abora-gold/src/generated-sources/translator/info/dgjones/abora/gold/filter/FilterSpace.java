/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.filter;

import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.filter.ClosedFilter;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterDsp;
import info.dgjones.abora.gold.filter.FilterPosition;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.filter.OpenFilter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * A FilterSpace can be described mathematically as a power space of its baseSpace, i.e. the
 * set of all subsets of the baseSpace. Each position in a FilterSpace is a Region in the
 * baseSpace, and each Filter is a set of Regions taken from the baseSpace. See Filter for
 * more detail.
 */
public class FilterSpace extends CoordinateSpace {

	protected CoordinateSpace myBaseSpace;
/*
udanax-top.st:14879:
CoordinateSpace subclass: #FilterSpace
	instanceVariableNames: 'myBaseSpace {CoordinateSpace}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Filter'!
*/
/*
udanax-top.st:14883:
FilterSpace comment:
'A FilterSpace can be described mathematically as a power space of its baseSpace, i.e. the set of all subsets of the baseSpace. Each position in a FilterSpace is a Region in the baseSpace, and each Filter is a set of Regions taken from the baseSpace. See Filter for more detail.'!
*/
/*
udanax-top.st:14885:
(FilterSpace getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #ON.CLIENT; yourself)!
*/
/*
udanax-top.st:14984:
FilterSpace class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:14987:
(FilterSpace getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #ON.CLIENT; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FilterSpace.class).setAttributes( new Set().add("PSEUDOCOPY").add("CONCRETE").add("ONCLIENT"));
/*

Generated during transformation: AddMethod
*/
}
public FilterSpace(CoordinateSpace base) {
	super();
	finishCreate((ClosedFilter.make(this)), (OpenFilter.make(this)), (FilterDsp.make(this)), null, null);
	myBaseSpace = base;
/*
udanax-top.st:14890:FilterSpace methodsFor: 'creation'!
create: base {CoordinateSpace}
	super create.
	self finishCreate: (ClosedFilter make: self)
		with: (OpenFilter make: self)
		with: (FilterDsp make: self)
		with: NULL
		with: NULL.
	myBaseSpace := base!
*/
}
public int actualHashForEqual() {
	return myBaseSpace.hashForEqual() + 1;
/*
udanax-top.st:14901:FilterSpace methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myBaseSpace hashForEqual + 1!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof FilterSpace) {
		FilterSpace fs = (FilterSpace) other;
		return fs.baseSpace().isEqual(myBaseSpace);
	}
	else {
		return false;
	}
/*
udanax-top.st:14904:FilterSpace methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: FilterSpace into: [:fs |
			^fs baseSpace isEqual: myBaseSpace]
		others: [^false].
	^false "fodder"!
*/
}
/**
 * Essential.  The CoordinateSpace of the Regions that are the input to Filters in this
 * FilterSpace.
 */
public CoordinateSpace baseSpace() {
	return myBaseSpace;
/*
udanax-top.st:14914:FilterSpace methodsFor: 'accessing'!
{CoordinateSpace CLIENT INLINE} baseSpace
	"Essential.  The CoordinateSpace of the Regions that are the input to Filters in this FilterSpace."
	
	^myBaseSpace!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myBaseSpace);
	oo.print(")");
/*
udanax-top.st:14921:FilterSpace methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myBaseSpace << ')'!
*/
}
/**
 * Essential. A region that matches any region that contains all the Positions in, i.e. is a
 * superset of, the given region.
 */
public Filter allFilter(XnRegion region) {
	return Filter.supersetFilter(this, region);
/*
udanax-top.st:14926:FilterSpace methodsFor: 'making'!
{Filter CLIENT INLINE} allFilter: region {XnRegion}
	"Essential. A region that matches any region that contains all the Positions in, i.e. is a superset of, the given region."
	
	^Filter supersetFilter: self with: region!
*/
}
/**
 * Essential. A filter that matches any region that intersects the given region.
 */
public Filter anyFilter(XnRegion baseRegion) {
	return Filter.intersectionFilter(this, baseRegion);
/*
udanax-top.st:14931:FilterSpace methodsFor: 'making'!
{Filter CLIENT INLINE} anyFilter: baseRegion {XnRegion}
	"Essential. A filter that matches any region that intersects the given region."
	
	^Filter intersectionFilter: self with: baseRegion!
*/
}
/**
 * Essential. A filter that matches any region that intersects the given region.
 */
public Filter intersectionFilter(XnRegion region) {
	return Filter.intersectionFilter(this, region);
/*
udanax-top.st:14936:FilterSpace methodsFor: 'making'!
{Filter INLINE} intersectionFilter: region {XnRegion}
	"Essential. A filter that matches any region that intersects the given region."
	
	^Filter intersectionFilter: self with: region!
*/
}
/**
 * A filter matching any regions that is not a subset of the given region.
 */
public Filter notSubsetFilter(XnRegion region) {
	return Filter.notSubsetFilter(this, region);
/*
udanax-top.st:14941:FilterSpace methodsFor: 'making'!
{Filter INLINE} notSubsetFilter: region {XnRegion}
	"A filter matching any regions that is not a subset of the given region."
	
	^Filter notSubsetFilter: self with: region!
*/
}
/**
 * A filter that matches any region that is not a superset of the given region.
 */
public Filter notSupersetFilter(XnRegion region) {
	return Filter.notSupersetFilter(this, region);
/*
udanax-top.st:14946:FilterSpace methodsFor: 'making'!
{Filter INLINE} notSupersetFilter: region {XnRegion}
	"A filter that matches any region that is not a superset of the given region."
	
	^Filter notSupersetFilter: self with: region!
*/
}
/**
 * A filter that matches any region that any of the filters in the set would have matched.
 */
public Filter orFilter(ScruSet subs) {
	return Filter.orFilter(this, subs);
/*
udanax-top.st:14951:FilterSpace methodsFor: 'making'!
{Filter INLINE} orFilter: subs {ScruSet of: Filter}
	"A filter that matches any region that any of the filters in the set would have matched."
	
	^Filter orFilter: self with: subs!
*/
}
/**
 * Essential. Given a Region in the baseSpace, make a Position which corresponds to it, so
 * that
 * filter->hasMember (this->position (baseRegion)) iff filter->match (baseRegion)
 */
public FilterPosition position(XnRegion baseRegion) {
	return FilterPosition.make(baseRegion);
/*
udanax-top.st:14956:FilterSpace methodsFor: 'making'!
{FilterPosition CLIENT INLINE} position: baseRegion {XnRegion}
	"Essential. Given a Region in the baseSpace, make a Position which corresponds to it, so that
		filter->hasMember (this->position (baseRegion)) iff filter->match (baseRegion)"
	
	^FilterPosition make: baseRegion!
*/
}
/**
 * A filter that matches any region that is a subset of the given region.
 */
public Filter subsetFilter(XnRegion region) {
	return Filter.subsetFilter(this, region);
/*
udanax-top.st:14962:FilterSpace methodsFor: 'making'!
{Filter INLINE} subsetFilter: region {XnRegion}
	"A filter that matches any region that is a subset of the given region."
	
	^Filter subsetFilter: self with: region!
*/
}
/**
 * Essential. A region that matches any region that is a superset of the given region.
 */
public Filter supersetFilter(XnRegion region) {
	return Filter.supersetFilter(this, region);
/*
udanax-top.st:14967:FilterSpace methodsFor: 'making'!
{Filter INLINE} supersetFilter: region {XnRegion}
	"Essential. A region that matches any region that is a superset of the given region."
	
	^Filter supersetFilter: self with: region!
*/
}
public void sendFilterSpaceTo(Xmtr xmtr) {
	xmtr.sendHeaper(myBaseSpace);
/*
udanax-top.st:14974:FilterSpace methodsFor: 'hooks:'!
{void SEND.HOOK} sendFilterSpaceTo: xmtr {Xmtr}
	xmtr sendHeaper: myBaseSpace.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	sendFilterSpaceTo(xmtr);
/*
udanax-top.st:14979:FilterSpace methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	
	self sendFilterSpaceTo: xmtr.!
*/
}
/**
 * A FilterSpace on the given base space.
 */
public static FilterSpace make(CoordinateSpace base) {
	return new FilterSpace(base);
/*
udanax-top.st:14992:FilterSpace class methodsFor: 'creation'!
{FilterSpace CLIENT} make: base {CoordinateSpace}
	"A FilterSpace on the given base space."
	
	^FilterSpace create: base!
*/
}
/**
 * {Filter CLIENT} andFilter: baseRegion {XnRegion}
 * {Filter CLIENT} anyFilter: baseRegion {XnRegion}
 * {CoordinateSpace CLIENT} baseSpace
 * {FilterPosition CLIENT} position: baseRegion {XnRegion}
 */
public static void infostProtocol() {
/*
udanax-top.st:14999:FilterSpace class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Filter CLIENT} andFilter: baseRegion {XnRegion}
{Filter CLIENT} anyFilter: baseRegion {XnRegion}
{CoordinateSpace CLIENT} baseSpace
{FilterPosition CLIENT} position: baseRegion {XnRegion}
"!
*/
}
public static Heaper makeRcvr(Rcvr rcvr) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:15008:FilterSpace class methodsFor: 'rcvr pseudo constructors'!
{Heaper} make.Rcvr: rcvr {Rcvr}
	^(FilterSpace new.Become: ((rcvr cast: SpecialistRcvr) makeIbid: FilterSpace))
		 create: (rcvr receiveHeaper cast: CoordinateSpace)!
*/
}
public FilterSpace() {
/*

Generated during transformation
*/
}
public FilterSpace(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
