/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.backrec.EditionRecorder;
import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.backrec.WorkRecorder;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.OriginalResultRecorderEFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.canopy.SimpleRecorderFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.be.canopy.prop.SensorProp;
import info.dgjones.abora.gold.filter.RegionDelta;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.SensorPropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Looks for recorders which might be triggered by an increase in endorsements on my
 * RangeElement itself
 */
public class OriginalResultRecorderEFinder extends SimpleRecorderFinder {

	protected IDRegion myPermissions;
	protected RegionDelta myEndorsementsDelta;
	protected CrossRegion myNewEndorsements;
/*
udanax-top.st:40593:
SimpleRecorderFinder subclass: #OriginalResultRecorderEFinder
	instanceVariableNames: '
		myPermissions {IDRegion}
		myEndorsementsDelta {RegionDelta of: CrossRegion}
		myNewEndorsements {CrossRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:40600:
OriginalResultRecorderEFinder comment:
'Looks for recorders which might be triggered by an increase in endorsements on my RangeElement itself'!
*/
/*
udanax-top.st:40602:
(OriginalResultRecorderEFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:40701:
OriginalResultRecorderEFinder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:40704:
(OriginalResultRecorderEFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OriginalResultRecorderEFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean shouldTrigger(ResultRecorder recorder, RecorderFossil fossil) {
	if (recorder instanceof EditionRecorder) {
		EditionRecorder er = (EditionRecorder) recorder;
		return (er.directFilter().isSwitchedOnBy(myEndorsementsDelta)) && (edition().anyPasses((PropFinder.backfollowFinder(er.permissionsFilter(), er.indirectFilter()))));
	}
	else if (recorder instanceof WorkRecorder) {
		WorkRecorder wr = (WorkRecorder) recorder;
		return (wr.endorsementsFilter().isSwitchedOnBy(myEndorsementsDelta)) && (work().canBeReadBy(wr.keyMaster()));
	}
	return false;
/*
udanax-top.st:40607:OriginalResultRecorderEFinder methodsFor: 'recording'!
{BooleanVar} shouldTrigger: recorder {ResultRecorder}
	with: fossil {RecorderFossil}
	 recorder cast: EditionRecorder into: [ :er |
	 	^(er directFilter isSwitchedOnBy: myEndorsementsDelta)
	 		and: [self edition anyPasses: (PropFinder
	 			backfollowFinder: er permissionsFilter
	 			with: er indirectFilter)]]
	 cast: WorkRecorder into: [ :wr |
	 	^(wr endorsementsFilter isSwitchedOnBy: myEndorsementsDelta)
	 		and: [self work canBeReadBy: wr keyMaster]].
	^false "fodder"!
*/
}
public RegionDelta endorsementsDelta() {
	return myEndorsementsDelta;
/*
udanax-top.st:40622:OriginalResultRecorderEFinder methodsFor: 'accessing'!
{RegionDelta of: CrossRegion} endorsementsDelta
	^myEndorsementsDelta!
*/
}
public boolean match(Prop prop) {
	if (prop instanceof SensorProp) {
		SensorProp p = (SensorProp) prop;
		return (p.relevantEndorsements().intersects(myNewEndorsements)) && (p.relevantPermissions().intersects(myPermissions));
	}
	return false;
/*
udanax-top.st:40626:OriginalResultRecorderEFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	prop cast: SensorProp into: [ :p |
		^(p relevantEndorsements intersects: myNewEndorsements)
			and: [p relevantPermissions intersects: myPermissions]].
	^false "fodder"!
*/
}
public CrossRegion newEndorsements() {
	return myNewEndorsements;
/*
udanax-top.st:40633:OriginalResultRecorderEFinder methodsFor: 'accessing'!
{CrossRegion} newEndorsements
	^myNewEndorsements!
*/
}
public IDRegion permissions() {
	return myPermissions;
/*
udanax-top.st:40637:OriginalResultRecorderEFinder methodsFor: 'accessing'!
{IDRegion} permissions 
	
	^myPermissions!
*/
}
public OriginalResultRecorderEFinder(int flags, BeRangeElement element, IDRegion permissions, RegionDelta endorsementsDelta, CrossRegion newEndorsements) {
	super(flags, element);
	myPermissions = permissions;
	myEndorsementsDelta = endorsementsDelta;
	myNewEndorsements = newEndorsements;
/*
udanax-top.st:40643:OriginalResultRecorderEFinder methodsFor: 'create'!
create: flags {UInt32}
	with: element {BeRangeElement}
	with: permissions {IDRegion}
	with: endorsementsDelta {RegionDelta of: CrossRegion}
	with: newEndorsements {CrossRegion}
	
	super create: flags with: element.
	myPermissions := permissions.
	myEndorsementsDelta := endorsementsDelta.
	myNewEndorsements := newEndorsements.!
*/
}
public int actualHashForEqual() {
	return ((rangeElement().hashForEqual() ^ myPermissions.hashForEqual()) ^ myEndorsementsDelta.hashForEqual()) ^ myNewEndorsements.hashForEqual();
/*
udanax-top.st:40656:OriginalResultRecorderEFinder methodsFor: 'testing'!
{UInt32	} actualHashForEqual
	^((self rangeElement hashForEqual
		bitXor: myPermissions hashForEqual)
		bitXor: myEndorsementsDelta hashForEqual)
		bitXor: myNewEndorsements hashForEqual!
*/
}
public boolean isEqual(Heaper heaper) {
	if (heaper instanceof OriginalResultRecorderEFinder) {
		OriginalResultRecorderEFinder other = (OriginalResultRecorderEFinder) heaper;
		return (rangeElement().isEqual(other.rangeElement())) && ((myPermissions.isEqual(other.permissions())) && ((myEndorsementsDelta.isEqual(other.endorsementsDelta())) && (myNewEndorsements.isEqual(other.newEndorsements()))));
	}
	else {
		return false;
	}
/*
udanax-top.st:40663:OriginalResultRecorderEFinder methodsFor: 'testing'!
{BooleanVar} isEqual: heaper {Heaper}
	heaper cast: OriginalResultRecorderEFinder into: [ :other |
		^(self rangeElement isEqual: other rangeElement)
			and: [(myPermissions isEqual: other permissions)
			and: [(myEndorsementsDelta isEqual: other endorsementsDelta)
			and: [myNewEndorsements isEqual: other newEndorsements]]]]
	others:
		[^false].
	^ false "compiler fodder"!
*/
}
public PropFinder oldPass(PropJoint parent) {
	if (parent instanceof SensorPropJoint) {
		SensorPropJoint p = (SensorPropJoint) parent;
		return OriginalResultRecorderEFinder.make(rangeElement(), ((IDRegion) (myPermissions.intersect(p.relevantPermissions()))), myEndorsementsDelta, ((CrossRegion) (myNewEndorsements.intersect(p.relevantEndorsements()))));
	}
	return null;
/*
udanax-top.st:40676:OriginalResultRecorderEFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	parent cast: SensorPropJoint into: [ :p |
		^OriginalResultRecorderEFinder
			make: self rangeElement
			with: ((myPermissions intersect: p relevantPermissions) cast: IDRegion)
			with: myEndorsementsDelta
			with: ((myNewEndorsements intersect: p relevantEndorsements) cast: CrossRegion)].
	^NULL "fodder"!
*/
}
public OriginalResultRecorderEFinder(Rcvr receiver) {
	super(receiver);
	myPermissions = (IDRegion) receiver.receiveHeaper();
	myEndorsementsDelta = (RegionDelta) receiver.receiveHeaper();
	myNewEndorsements = (CrossRegion) receiver.receiveHeaper();
/*
udanax-top.st:40688:OriginalResultRecorderEFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myPermissions _ receiver receiveHeaper.
	myEndorsementsDelta _ receiver receiveHeaper.
	myNewEndorsements _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myPermissions);
	xmtr.sendHeaper(myEndorsementsDelta);
	xmtr.sendHeaper(myNewEndorsements);
/*
udanax-top.st:40694:OriginalResultRecorderEFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myPermissions.
	xmtr sendHeaper: myEndorsementsDelta.
	xmtr sendHeaper: myNewEndorsements.!
*/
}
public static PropFinder make(BeRangeElement element, IDRegion permissions, RegionDelta endorsementsDelta) {
	return make(element, permissions, endorsementsDelta, ((CrossRegion) (endorsementsDelta.after().minus(endorsementsDelta.before()))));
/*
udanax-top.st:40709:OriginalResultRecorderEFinder class methodsFor: 'create'!
{PropFinder} make: element {BeRangeElement}
	with: permissions {IDRegion}
	with: endorsementsDelta {RegionDelta of: CrossRegion}
	
	^self make: element
		with: permissions
		with: endorsementsDelta
		with: ((endorsementsDelta after minus: endorsementsDelta before) cast: CrossRegion)!
*/
}
public static PropFinder make(BeRangeElement element, IDRegion permissions, RegionDelta endorsementsDelta, CrossRegion newEndorsements) {
	if (permissions.isEmpty() || (newEndorsements.isEmpty())) {
		return PropFinder.closedPropFinder();
	}
	return new OriginalResultRecorderEFinder((SensorCrum.flagsFor(permissions, newEndorsements, false)), element, permissions, endorsementsDelta, newEndorsements);
/*
udanax-top.st:40718:OriginalResultRecorderEFinder class methodsFor: 'create'!
{PropFinder} make: element {BeRangeElement}
	with: permissions {IDRegion}
	with: endorsementsDelta {RegionDelta of: CrossRegion}
	with: newEndorsements {CrossRegion}
	
	(permissions isEmpty or: [newEndorsements isEmpty]) ifTrue:
		[^PropFinder closedPropFinder].
	^self create: (SensorCrum
			flagsFor: permissions
			with: newEndorsements
			with: false)
		with: element
		with: permissions
		with: endorsementsDelta
		with: newEndorsements.!
*/
}
public OriginalResultRecorderEFinder() {
/*

Generated during transformation
*/
}
}
