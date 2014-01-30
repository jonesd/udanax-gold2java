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
import info.dgjones.abora.gold.be.canopy.BackfollowFinder;
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
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Finder used to filter the htree walk by the bert canopy when doing a backFollow which uses
 * both permissions and endorsement filters
 */
public class BackfollowFinder extends BertPropFinder {

	protected Filter myPermissionsFilter;
	protected Filter myEndorsementsFilter;
/*
udanax-top.st:39478:
BertPropFinder subclass: #BackfollowFinder
	instanceVariableNames: '
		myPermissionsFilter {Filter of: (XnRegion of: ID)}
		myEndorsementsFilter {Filter of: (XnRegion of: ID)}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39484:
BackfollowFinder comment:
'Finder used to filter the htree walk by the bert canopy when doing a backFollow which uses both permissions and endorsement filters'!
*/
/*
udanax-top.st:39486:
(BackfollowFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BackfollowFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public BackfollowFinder(int flags, Filter permissionsFilter, Filter endorsementsFilter) {
	super(flags);
	myPermissionsFilter = permissionsFilter;
	myEndorsementsFilter = endorsementsFilter;
/*
udanax-top.st:39491:BackfollowFinder methodsFor: 'creation'!
create: flags {UInt32}
	with: permissionsFilter {Filter of: (XnRegion of: ID)} 
	with: endorsementsFilter {Filter of: (CrossRegion of: ID)}
	
	super create: flags.
	myPermissionsFilter _ permissionsFilter.
	myEndorsementsFilter _ endorsementsFilter!
*/
}
public Filter endorsementsFilter() {
	return myEndorsementsFilter;
/*
udanax-top.st:39501:BackfollowFinder methodsFor: 'accessing'!
{Filter of: (XnRegion of: ID)} endorsementsFilter
	^myEndorsementsFilter!
*/
}
public PropFinder findPast(BeEdition edition) {
	boolean canSee;
	XnRegion endorsements;
	Ravi.thingToDo();
	/* use regions in finder so that we don't need to create intermediate objects */
	canSee = false;
	endorsements = edition.endorsements();
	Stepper stomper = edition.currentWorks().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		BeWork work = (BeWork) stomper.fetch();
		if (work == null) {
			continue ;
		}
		if ((work.fetchReadClub() != null && (myPermissionsFilter.match(work.fetchReadClub().asRegion()))) || (work.fetchEditClub() != null && (myPermissionsFilter.match(work.fetchEditClub().asRegion())))) {
			canSee = true;
			endorsements = endorsements.unionWith(work.endorsements());
		}
	}
	stomper.destroy();
	if (myEndorsementsFilter.match(endorsements)) {
		if (canSee) {
			return PropFinder.openPropFinder();
		}
		else {
			return PropFinder.backfollowFinder(myPermissionsFilter);
		}
	}
	return this;
/*
udanax-top.st:39504:BackfollowFinder methodsFor: 'accessing'!
{PropFinder} findPast: edition {BeEdition}
	| canSee {BooleanVar} endorsements {XnRegion} |
	Ravi thingToDo. "use regions in finder so that we don't need to create intermediate objects"
	canSee := false.
	endorsements := edition endorsements.
	edition currentWorks stepper forEach: [ :work {BeWork} |
		((work fetchReadClub ~~ NULL
			and: [myPermissionsFilter match: work fetchReadClub asRegion])
		or:
			[work fetchEditClub ~~ NULL
				and: [myPermissionsFilter match: work fetchEditClub asRegion]])
			ifTrue:
				[canSee := true.
				endorsements := endorsements unionWith: work endorsements]].
	(myEndorsementsFilter match: endorsements) ifTrue:
		[canSee ifTrue:
			[^PropFinder openPropFinder]
		ifFalse:
			[^PropFinder backfollowFinder: myPermissionsFilter]].
	^self!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	BertProp p;
	p = (BertProp) prop;
	return (myPermissionsFilter.match(p.permissions())) && (myEndorsementsFilter.match(p.endorsements()));
/*
udanax-top.st:39526:BackfollowFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	"tell whether a prop matches this filter"
	| p {BertProp wimpy} |
	p _ prop cast: BertProp.
	^(myPermissionsFilter match: p permissions) and: [myEndorsementsFilter match: p endorsements]!
*/
}
public Filter permissionsFilter() {
	return myPermissionsFilter;
/*
udanax-top.st:39532:BackfollowFinder methodsFor: 'accessing'!
{Filter of: (XnRegion of: ID)} permissionsFilter
	^myPermissionsFilter!
*/
}
public int actualHashForEqual() {
	return (getCategory().hashForEqual() ^ myPermissionsFilter.hashForEqual()) ^ myEndorsementsFilter.hashForEqual();
/*
udanax-top.st:39537:BackfollowFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(self getCategory hashForEqual bitXor: myPermissionsFilter hashForEqual)
	  bitXor: myEndorsementsFilter hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof BackfollowFinder) {
		BackfollowFinder o = (BackfollowFinder) other;
		return (myPermissionsFilter.isEqual(o.permissionsFilter())) && (myEndorsementsFilter.isEqual(o.endorsementsFilter()));
	}
	else {
		return false;
	}
/*
udanax-top.st:39542:BackfollowFinder methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other 
		cast: BackfollowFinder into: [:o |
			^(myPermissionsFilter isEqual: o permissionsFilter) 
			 and: [myEndorsementsFilter isEqual: o endorsementsFilter]]
		others:
			[^false].
	^false "fodder"!
*/
}
/**
 * return a simple enough finder for looking at the children
 */
public PropFinder oldPass(PropJoint parent) {
	BertPropJoint p;
	p = (BertPropJoint) parent;
	return PropFinder.backfollowFinder((myPermissionsFilter.pass(p.permissionsJoint())), (myEndorsementsFilter.pass(p.endorsementsJoint())));
/*
udanax-top.st:39554:BackfollowFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint} 
	"return a simple enough finder for looking at the children"
	| p {BertPropJoint wimpy} |
	p _ parent cast: BertPropJoint.
	^PropFinder  
		backfollowFinder: (myPermissionsFilter pass: p permissionsJoint)
		with: (myEndorsementsFilter pass: p endorsementsJoint)!
*/
}
public BackfollowFinder(Rcvr receiver) {
	super(receiver);
	myPermissionsFilter = (Filter) receiver.receiveHeaper();
	myEndorsementsFilter = (Filter) receiver.receiveHeaper();
/*
udanax-top.st:39565:BackfollowFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myPermissionsFilter _ receiver receiveHeaper.
	myEndorsementsFilter _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myPermissionsFilter);
	xmtr.sendHeaper(myEndorsementsFilter);
/*
udanax-top.st:39570:BackfollowFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myPermissionsFilter.
	xmtr sendHeaper: myEndorsementsFilter.!
*/
}
public BackfollowFinder() {
/*

Generated during transformation
*/
}
}
