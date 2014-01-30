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
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.ContainedEditionRecorderEFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
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
 * Looks for recorders which might be triggered by an increase in endorsements in something
 * containing my edition. Keep the total endorsements on my edition for quick reject?
 */
public class ContainedEditionRecorderEFinder extends SimpleRecorderFinder {

	protected IDRegion myPermissions;
	protected RegionDelta myEndorsementsDelta;
	protected CrossRegion myNewEndorsements;
/*
udanax-top.st:40451:
SimpleRecorderFinder subclass: #ContainedEditionRecorderEFinder
	instanceVariableNames: '
		myPermissions {IDRegion}
		myEndorsementsDelta {RegionDelta of: CrossRegion}
		myNewEndorsements {CrossRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:40458:
ContainedEditionRecorderEFinder comment:
'Looks for recorders which might be triggered by an increase in endorsements in something containing my edition. Keep the total endorsements on my edition for quick reject?'!
*/
/*
udanax-top.st:40460:
(ContainedEditionRecorderEFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:40558:
ContainedEditionRecorderEFinder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:40561:
(ContainedEditionRecorderEFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ContainedEditionRecorderEFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean shouldTrigger(ResultRecorder recorder, RecorderFossil fossil) {
	if (recorder instanceof EditionRecorder) {
		EditionRecorder er = (EditionRecorder) recorder;
		Object currentKeyMasterOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentKeyMaster, er.keyMaster());
		try {
			return (er.indirectFilter().isSwitchedOnBy(myEndorsementsDelta)) && ((er.directFilter().match(edition().visibleEndorsements())) && (edition().anyPasses((PropFinder.backfollowFinder(er.permissionsFilter())))));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentKeyMaster, currentKeyMasterOldValue);
		}
	}
	return false;
/*
udanax-top.st:40465:ContainedEditionRecorderEFinder methodsFor: 'recording'!
{BooleanVar} shouldTrigger: recorder {ResultRecorder}
	with: fossil {RecorderFossil}
	recorder cast: EditionRecorder into: [ :er |
		[FeServer] USES.
		CurrentKeyMaster fluidBind: er keyMaster during:
			[^(er indirectFilter isSwitchedOnBy: myEndorsementsDelta)
				and: [(er directFilter match: self edition visibleEndorsements)
				and: [self edition anyPasses: (PropFinder
					backfollowFinder: er permissionsFilter)]]]].
	^false "fodder"!
*/
}
public RegionDelta endorsementsDelta() {
	return myEndorsementsDelta;
/*
udanax-top.st:40479:ContainedEditionRecorderEFinder methodsFor: 'accessing'!
{RegionDelta of: CrossRegion} endorsementsDelta
	^myEndorsementsDelta!
*/
}
public boolean match(Prop prop) {
	if (prop instanceof SensorProp) {
		SensorProp p = (SensorProp) prop;
		return (p.relevantPermissions().intersects(myPermissions)) && (p.relevantEndorsements().intersects(myNewEndorsements));
	}
	return false;
/*
udanax-top.st:40483:ContainedEditionRecorderEFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	prop cast: SensorProp into: [ :p |
		^(p relevantPermissions intersects: myPermissions)
			and: [p relevantEndorsements intersects: myNewEndorsements]].
	^false "fodder"!
*/
}
public CrossRegion newEndorsements() {
	return myNewEndorsements;
/*
udanax-top.st:40490:ContainedEditionRecorderEFinder methodsFor: 'accessing'!
{CrossRegion} newEndorsements
	^myNewEndorsements!
*/
}
public IDRegion permissions() {
	return myPermissions;
/*
udanax-top.st:40494:ContainedEditionRecorderEFinder methodsFor: 'accessing'!
{IDRegion} permissions 
	
	^myPermissions!
*/
}
public ContainedEditionRecorderEFinder(int flags, BeRangeElement element, IDRegion permissions, RegionDelta endorsementsDelta, CrossRegion newEndorsements) {
	super(flags, element);
	myPermissions = permissions;
	myEndorsementsDelta = endorsementsDelta;
	myNewEndorsements = newEndorsements;
/*
udanax-top.st:40500:ContainedEditionRecorderEFinder methodsFor: 'create'!
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
udanax-top.st:40513:ContainedEditionRecorderEFinder methodsFor: 'testing'!
{UInt32	} actualHashForEqual
	^((self rangeElement hashForEqual
		bitXor: myPermissions hashForEqual)
		bitXor: myEndorsementsDelta hashForEqual)
		bitXor: myNewEndorsements hashForEqual!
*/
}
public boolean isEqual(Heaper heaper) {
	if (heaper instanceof ContainedEditionRecorderEFinder) {
		ContainedEditionRecorderEFinder other = (ContainedEditionRecorderEFinder) heaper;
		return (rangeElement().isEqual(other.rangeElement())) && ((myPermissions.isEqual(other.permissions())) && ((myEndorsementsDelta.isEqual(other.endorsementsDelta())) && (myNewEndorsements.isEqual(other.newEndorsements()))));
	}
	else {
		return false;
	}
/*
udanax-top.st:40520:ContainedEditionRecorderEFinder methodsFor: 'testing'!
{BooleanVar} isEqual: heaper {Heaper}
	heaper cast: ContainedEditionRecorderEFinder into: [ :other |
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
		return ContainedEditionRecorderEFinder.make(edition(), ((IDRegion) (myPermissions.intersect(p.relevantPermissions()))), myEndorsementsDelta, ((CrossRegion) (myNewEndorsements.intersect(p.relevantEndorsements()))));
	}
	return null;
/*
udanax-top.st:40533:ContainedEditionRecorderEFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	parent cast: SensorPropJoint into: [ :p |
		^ContainedEditionRecorderEFinder
			make: self edition
			with: ((myPermissions intersect: p relevantPermissions) cast: IDRegion)
			with: myEndorsementsDelta
			with: ((myNewEndorsements intersect: p relevantEndorsements) cast: CrossRegion)].
	^NULL "fodder"!
*/
}
public ContainedEditionRecorderEFinder(Rcvr receiver) {
	super(receiver);
	myPermissions = (IDRegion) receiver.receiveHeaper();
	myEndorsementsDelta = (RegionDelta) receiver.receiveHeaper();
	myNewEndorsements = (CrossRegion) receiver.receiveHeaper();
/*
udanax-top.st:40545:ContainedEditionRecorderEFinder methodsFor: 'generated:'!
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
udanax-top.st:40551:ContainedEditionRecorderEFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myPermissions.
	xmtr sendHeaper: myEndorsementsDelta.
	xmtr sendHeaper: myNewEndorsements.!
*/
}
public static PropFinder make(BeRangeElement element, IDRegion permissions, RegionDelta endorsementsDelta) {
	Ravi.thingToDo();
	/* Separate out relevant endorsements from new endorsements; relevant is those on contained edition - so you can exclude paths which would never care to record that Edition. At the moment all spawned Contained...Finders will vanish at the same time, i.e. when noone cares about the new endorsements any more. Putting in the relevant endorsements as well allows them to vanish earlier. This could also be done by testing self edition totalEndorsements in match and pass. */
	return make(element, permissions, endorsementsDelta, ((CrossRegion) (endorsementsDelta.after().minus(endorsementsDelta.before()))));
/*
udanax-top.st:40566:ContainedEditionRecorderEFinder class methodsFor: 'create'!
{PropFinder} make: element {BeRangeElement}
	with: permissions {IDRegion}
	with: endorsementsDelta {RegionDelta of: CrossRegion}
	
	Ravi thingToDo. "Separate out relevant endorsements from new endorsements; relevant is those on contained edition - so you can exclude paths which would never care to record that Edition. At the moment all spawned Contained...Finders will vanish at the same time, i.e. when noone cares about the new endorsements any more. Putting in the relevant endorsements as well allows them to vanish earlier. This could also be done by testing self edition totalEndorsements in match and pass."
	
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
	return new ContainedEditionRecorderEFinder((SensorCrum.flagsFor(permissions, newEndorsements, false)), element, permissions, endorsementsDelta, newEndorsements);
/*
udanax-top.st:40577:ContainedEditionRecorderEFinder class methodsFor: 'create'!
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
public ContainedEditionRecorderEFinder() {
/*

Generated during transformation
*/
}
}
