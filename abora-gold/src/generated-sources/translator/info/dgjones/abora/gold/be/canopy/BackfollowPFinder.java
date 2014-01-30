/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.canopy.BackfollowPFinder;
import info.dgjones.abora.gold.be.canopy.BertPropFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.BertPropJoint;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Finder used to filter the htree walk by the bert canopy when doing a backFollow which uses
 * just permissions filters
 */
public class BackfollowPFinder extends BertPropFinder {

	protected Filter myPermissionsFilter;
/*
udanax-top.st:39575:
BertPropFinder subclass: #BackfollowPFinder
	instanceVariableNames: 'myPermissionsFilter {Filter of: (XnRegion of: ID)}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39579:
BackfollowPFinder comment:
'Finder used to filter the htree walk by the bert canopy when doing a backFollow which uses just permissions filters'!
*/
/*
udanax-top.st:39581:
(BackfollowPFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BackfollowPFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public BackfollowPFinder(int flags, Filter permissionsFilter) {
	super(flags);
	myPermissionsFilter = permissionsFilter;
/*
udanax-top.st:39586:BackfollowPFinder methodsFor: 'creation'!
create: flags {UInt32}
	with: permissionsFilter {Filter of: (XnRegion of: ID)}
	super create: flags.
	myPermissionsFilter _ permissionsFilter!
*/
}
public PropFinder findPast(BeEdition edition) {
	Ravi.thingToDo();
	Stepper stomper = 
	/* use regions in finder so that we don't need to create intermediate objects */
	edition.currentWorks().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		BeWork work = (BeWork) stomper.fetch();
		if (work == null) {
			continue ;
		}
		if ((work.fetchReadClub() != null && (myPermissionsFilter.match(work.fetchReadClub().asRegion()))) || (work.fetchEditClub() != null && (myPermissionsFilter.match(work.fetchEditClub().asRegion())))) {
			return PropFinder.openPropFinder();
		}
	}
	stomper.destroy();
	return this;
/*
udanax-top.st:39593:BackfollowPFinder methodsFor: 'accessing'!
{PropFinder} findPast: edition {BeEdition}
	Ravi thingToDo. "use regions in finder so that we don't need to create intermediate objects"
	edition currentWorks stepper forEach: [ :work {BeWork} |
		((work fetchReadClub ~~ NULL
			and: [myPermissionsFilter match: work fetchReadClub asRegion])
		or:
			[work fetchEditClub ~~ NULL
				and: [myPermissionsFilter match: work fetchEditClub asRegion]])
			ifTrue: [^PropFinder openPropFinder]].
	^self!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	return myPermissionsFilter.match(((BertProp) prop).permissions());
/*
udanax-top.st:39605:BackfollowPFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	"tell whether a prop matches this filter"
	^myPermissionsFilter match: (prop cast: BertProp) permissions!
*/
}
public Filter permissionsFilter() {
	return myPermissionsFilter;
/*
udanax-top.st:39609:BackfollowPFinder methodsFor: 'accessing'!
{Filter of: (XnRegion of: ID)} permissionsFilter
	^myPermissionsFilter!
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual() ^ myPermissionsFilter.hashForEqual();
/*
udanax-top.st:39614:BackfollowPFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual bitXor: myPermissionsFilter hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof BackfollowPFinder) {
		BackfollowPFinder o = (BackfollowPFinder) other;
		return myPermissionsFilter.isEqual(o.permissionsFilter());
	}
	else {
		return false;
	}
/*
udanax-top.st:39618:BackfollowPFinder methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other 
		cast: BackfollowPFinder into: [:o |
			^myPermissionsFilter isEqual: o permissionsFilter]
		others:
			[^false].
	^false "fodder"!
*/
}
/**
 * return a simple enough finder for looking at the children
 */
public PropFinder oldPass(PropJoint parent) {
	return PropFinder.backfollowFinder((myPermissionsFilter.pass(((BertPropJoint) parent).permissionsJoint())));
/*
udanax-top.st:39629:BackfollowPFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint} 
	"return a simple enough finder for looking at the children"
	^PropFinder backfollowFinder: (myPermissionsFilter pass: (parent cast: BertPropJoint) permissionsJoint)!
*/
}
public BackfollowPFinder(Rcvr receiver) {
	super(receiver);
	myPermissionsFilter = (Filter) receiver.receiveHeaper();
/*
udanax-top.st:39635:BackfollowPFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myPermissionsFilter _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myPermissionsFilter);
/*
udanax-top.st:39639:BackfollowPFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myPermissionsFilter.!
*/
}
public BackfollowPFinder() {
/*

Generated during transformation
*/
}
}
