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
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.ResultRecorderPFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.canopy.SimpleRecorderFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.be.canopy.prop.SensorProp;
import info.dgjones.abora.gold.filter.RegionDelta;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.SensorPropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Looks for records which might be triggered by in increase in visibility of my RangeElement
 */
public class ResultRecorderPFinder extends SimpleRecorderFinder {

	protected RegionDelta myPermissionsDelta;
	protected IDRegion myNewPermissions;
	protected CrossRegion myEndorsements;
/*
udanax-top.st:40734:
SimpleRecorderFinder subclass: #ResultRecorderPFinder
	instanceVariableNames: '
		myPermissionsDelta {RegionDelta}
		myNewPermissions {IDRegion}
		myEndorsements {CrossRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:40741:
ResultRecorderPFinder comment:
'Looks for records which might be triggered by in increase in visibility of my RangeElement'!
*/
/*
udanax-top.st:40743:
(ResultRecorderPFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:40839:
ResultRecorderPFinder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:40842:
(ResultRecorderPFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ResultRecorderPFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public ResultRecorderPFinder(int flags, BeRangeElement element, RegionDelta permissionsDelta, IDRegion newPermissions, CrossRegion endorsements) {
	super(flags, element);
	myPermissionsDelta = permissionsDelta;
	myNewPermissions = newPermissions;
	myEndorsements = endorsements;
/*
udanax-top.st:40748:ResultRecorderPFinder methodsFor: 'create'!
create: flags {UInt32}
	with: element {BeRangeElement}
	with: permissionsDelta {RegionDelta}
	with: newPermissions {IDRegion}
	with: endorsements {CrossRegion}
	super create: flags with: element.
	myPermissionsDelta := permissionsDelta.
	myNewPermissions := newPermissions.
	myEndorsements := endorsements.!
*/
}
public CrossRegion endorsements() {
	return myEndorsements;
/*
udanax-top.st:40761:ResultRecorderPFinder methodsFor: 'accessing'!
{CrossRegion} endorsements
	^myEndorsements!
*/
}
public boolean match(Prop prop) {
	if (prop instanceof SensorProp) {
		SensorProp p = (SensorProp) prop;
		return (p.relevantPermissions().intersects(myPermissionsDelta.after())) && (p.relevantEndorsements().intersects(myEndorsements));
	}
	return false;
/*
udanax-top.st:40765:ResultRecorderPFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop} 
	prop cast: SensorProp into: [ :p |
		^(p relevantPermissions intersects: myPermissionsDelta after)
			and: [p relevantEndorsements intersects: myEndorsements]].
	^false "fodder"!
*/
}
public IDRegion newPermissions() {
	return myNewPermissions;
/*
udanax-top.st:40772:ResultRecorderPFinder methodsFor: 'accessing'!
{IDRegion} newPermissions 
	
	^myNewPermissions!
*/
}
public RegionDelta permissionsDelta() {
	return myPermissionsDelta;
/*
udanax-top.st:40776:ResultRecorderPFinder methodsFor: 'accessing'!
{RegionDelta of: IDRegion} permissionsDelta
	
	^myPermissionsDelta!
*/
}
public boolean shouldTrigger(ResultRecorder recorder, RecorderFossil fossil) {
	if (recorder.permissionsFilter().isSwitchedOnBy(myPermissionsDelta)) {
		if (recorder instanceof EditionRecorder) {
			EditionRecorder er = (EditionRecorder) recorder;
			Object currentKeyMasterOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentKeyMaster, er.keyMaster());
			try {
				return er.directFilter().match(edition().visibleEndorsements());
			}
			finally {
				AboraBlockSupport.exitFluidBindDuring(CurrentKeyMaster, currentKeyMasterOldValue);
			}
		}
		else if (recorder instanceof WorkRecorder) {
			WorkRecorder wr = (WorkRecorder) recorder;
			return wr.endorsementsFilter().match(work().endorsements());
		}
	}
	else {
		return false;
	}
	return false;
/*
udanax-top.st:40782:ResultRecorderPFinder methodsFor: 'recording'!
{BooleanVar} shouldTrigger: recorder {ResultRecorder}
	with: fossil {RecorderFossil}
	(recorder permissionsFilter isSwitchedOnBy: myPermissionsDelta) ifTrue:
		[recorder cast: EditionRecorder into: [ :er |
			CurrentKeyMaster fluidBind: er keyMaster during:
				[^er directFilter match: self edition visibleEndorsements]]
		cast: WorkRecorder into: [ :wr |
			^wr endorsementsFilter match: self work endorsements]]
	ifFalse:
		[^false].
	^false "fodder"!
*/
}
public int actualHashForEqual() {
	return (myPermissionsDelta.hashForEqual() ^ myNewPermissions.hashForEqual()) ^ myEndorsements.hashForEqual();
/*
udanax-top.st:40797:ResultRecorderPFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(myPermissionsDelta hashForEqual
		bitXor: myNewPermissions hashForEqual)
		bitXor: myEndorsements hashForEqual!
*/
}
public boolean isEqual(Heaper heaper) {
	if (heaper instanceof ResultRecorderPFinder) {
		ResultRecorderPFinder other = (ResultRecorderPFinder) heaper;
		return (myPermissionsDelta.isEqual(other.permissionsDelta())) && ((myNewPermissions.isEqual(other.newPermissions())) && (myEndorsements.isEqual(other.endorsements())));
	}
	else {
		return false;
	}
/*
udanax-top.st:40803:ResultRecorderPFinder methodsFor: 'testing'!
{BooleanVar} isEqual: heaper {Heaper}
	heaper cast: ResultRecorderPFinder into: [ :other |
		^(myPermissionsDelta isEqual: other permissionsDelta)
			and: [(myNewPermissions isEqual: other newPermissions)
			and: [myEndorsements isEqual: other endorsements]]]
	others:
		[^false].
	^false "fodder"!
*/
}
public PropFinder oldPass(PropJoint parent) {
	if (parent instanceof SensorPropJoint) {
		SensorPropJoint p = (SensorPropJoint) parent;
		return ResultRecorderPFinder.make(rangeElement(), myPermissionsDelta, ((IDRegion) (myNewPermissions.intersect(p.relevantPermissions()))), ((CrossRegion) (myEndorsements.intersect(p.relevantEndorsements()))));
	}
	return null;
/*
udanax-top.st:40815:ResultRecorderPFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	parent cast: SensorPropJoint into: [ :p |
		^ResultRecorderPFinder make: self rangeElement
			with: myPermissionsDelta
			with: ((myNewPermissions intersect: p relevantPermissions) cast: IDRegion)
			with: ((myEndorsements intersect: p relevantEndorsements) cast: CrossRegion)].
	^NULL "fodder"!
*/
}
public ResultRecorderPFinder(Rcvr receiver) {
	super(receiver);
	myPermissionsDelta = (RegionDelta) receiver.receiveHeaper();
	myNewPermissions = (IDRegion) receiver.receiveHeaper();
	myEndorsements = (CrossRegion) receiver.receiveHeaper();
/*
udanax-top.st:40826:ResultRecorderPFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myPermissionsDelta _ receiver receiveHeaper.
	myNewPermissions _ receiver receiveHeaper.
	myEndorsements _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myPermissionsDelta);
	xmtr.sendHeaper(myNewPermissions);
	xmtr.sendHeaper(myEndorsements);
/*
udanax-top.st:40832:ResultRecorderPFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myPermissionsDelta.
	xmtr sendHeaper: myNewPermissions.
	xmtr sendHeaper: myEndorsements.!
*/
}
public static PropFinder make(BeRangeElement element, RegionDelta permissionsDelta, CrossRegion endorsements) {
	return make(element, permissionsDelta, ((IDRegion) (permissionsDelta.after().minus(permissionsDelta.before()))), endorsements);
/*
udanax-top.st:40847:ResultRecorderPFinder class methodsFor: 'create'!
{PropFinder} make: element {BeRangeElement}
	with: permissionsDelta {RegionDelta}
	with: endorsements {CrossRegion}
	^self make: element
		with: permissionsDelta
		with: ((permissionsDelta after minus: permissionsDelta before) cast: IDRegion)
		with: endorsements.!
*/
}
public static PropFinder make(BeRangeElement element, RegionDelta permissionsDelta, IDRegion newPermissions, CrossRegion endorsements) {
	if (newPermissions.isEmpty() || (endorsements.isEmpty())) {
		return PropFinder.closedPropFinder();
	}
	return new ResultRecorderPFinder((SensorCrum.flagsFor(newPermissions, endorsements, false)), element, permissionsDelta, newPermissions, endorsements);
/*
udanax-top.st:40856:ResultRecorderPFinder class methodsFor: 'create'!
{PropFinder} make: element {BeRangeElement}
	with: permissionsDelta {RegionDelta}
	with: newPermissions {IDRegion}
	with: endorsements {CrossRegion}
	(newPermissions isEmpty or: [endorsements isEmpty]) ifTrue:
		[^PropFinder closedPropFinder].
	^self create: (SensorCrum
			flagsFor: newPermissions
			with: endorsements
			with: false)
		with: element with: permissionsDelta with: newPermissions with: endorsements.!
*/
}
public ResultRecorderPFinder() {
/*

Generated during transformation
*/
}
}
