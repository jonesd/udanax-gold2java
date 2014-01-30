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
import info.dgjones.abora.gold.be.canopy.AnyRecorderEFinder;
import info.dgjones.abora.gold.be.canopy.AnyRecorderFinder;
import info.dgjones.abora.gold.be.canopy.ContainedEditionRecorderEFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.be.canopy.prop.SensorProp;
import info.dgjones.abora.gold.filter.RegionDelta;
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
 * Generates finders for recorders triggered by an increase in endorsements. Also remembers
 * the (approximate) permissions on the object whose endorsements changed
 */
public class AnyRecorderEFinder extends AnyRecorderFinder {

	protected IDRegion myPermissions;
	protected RegionDelta myEndorsementsDelta;
	protected CrossRegion myNewEndorsements;
/*
udanax-top.st:39995:
AnyRecorderFinder subclass: #AnyRecorderEFinder
	instanceVariableNames: '
		myPermissions {IDRegion}
		myEndorsementsDelta {RegionDelta of: CrossRegion}
		myNewEndorsements {CrossRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:40002:
AnyRecorderEFinder comment:
'Generates finders for recorders triggered by an increase in endorsements. Also remembers the (approximate) permissions on the object whose endorsements changed'!
*/
/*
udanax-top.st:40004:
(AnyRecorderEFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:40090:
AnyRecorderEFinder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:40093:
(AnyRecorderEFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(AnyRecorderEFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public RegionDelta endorsementsDelta() {
	return myEndorsementsDelta;
/*
udanax-top.st:40009:AnyRecorderEFinder methodsFor: 'accessing'!
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
udanax-top.st:40013:AnyRecorderEFinder methodsFor: 'accessing'!
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
udanax-top.st:40020:AnyRecorderEFinder methodsFor: 'accessing'!
{CrossRegion} newEndorsements
	^myNewEndorsements!
*/
}
public PropFinder nextFinder(BeEdition edition) {
	return ContainedEditionRecorderEFinder.make(edition, myPermissions, myEndorsementsDelta, myNewEndorsements);
/*
udanax-top.st:40024:AnyRecorderEFinder methodsFor: 'accessing'!
{PropFinder} nextFinder: edition {BeEdition}
	^ContainedEditionRecorderEFinder make: edition
		with: myPermissions
		with: myEndorsementsDelta
		with: myNewEndorsements!
*/
}
public IDRegion permissions() {
	return myPermissions;
/*
udanax-top.st:40031:AnyRecorderEFinder methodsFor: 'accessing'!
{IDRegion} permissions
	^myPermissions!
*/
}
public AnyRecorderEFinder(int flags, IDRegion permissions, RegionDelta endorsementsDelta, CrossRegion newEndorsements) {
	super(flags);
	myPermissions = permissions;
	myEndorsementsDelta = endorsementsDelta;
	myNewEndorsements = newEndorsements;
/*
udanax-top.st:40037:AnyRecorderEFinder methodsFor: 'create'!
create: flags {UInt32}
	with: permissions {IDRegion}
	with: endorsementsDelta {RegionDelta of: CrossRegion}
	with: newEndorsements {CrossRegion}
	
	super create: flags.
	myPermissions := permissions.
	myEndorsementsDelta := endorsementsDelta.
	myNewEndorsements := newEndorsements.!
*/
}
public int actualHashForEqual() {
	return (myPermissions.hashForEqual() ^ myEndorsementsDelta.hashForEqual()) ^ myNewEndorsements.hashForEqual();
/*
udanax-top.st:40049:AnyRecorderEFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(myPermissions hashForEqual
		bitXor: myEndorsementsDelta hashForEqual)
		bitXor: myNewEndorsements hashForEqual!
*/
}
public boolean isEqual(Heaper heaper) {
	if (heaper instanceof AnyRecorderEFinder) {
		AnyRecorderEFinder other = (AnyRecorderEFinder) heaper;
		return (myPermissions.isEqual(other.permissions())) && ((myEndorsementsDelta.isEqual(other.endorsementsDelta())) && (myNewEndorsements.isEqual(other.newEndorsements())));
	}
	else {
		return false;
	}
/*
udanax-top.st:40055:AnyRecorderEFinder methodsFor: 'testing'!
{BooleanVar} isEqual: heaper {Heaper}
	heaper cast: AnyRecorderEFinder into: [ :other |
		^(myPermissions isEqual: other permissions)
			and: [(myEndorsementsDelta isEqual: other endorsementsDelta)
			and: [myNewEndorsements isEqual: other newEndorsements]]]
	others:
		[^false].
	^false "fodder"!
*/
}
public PropFinder oldPass(PropJoint parent) {
	if (parent instanceof SensorPropJoint) {
		SensorPropJoint p = (SensorPropJoint) parent;
		return AnyRecorderEFinder.make(((IDRegion) (p.relevantPermissions().intersect(myPermissions))), myEndorsementsDelta, ((CrossRegion) (p.relevantEndorsements().intersect(myNewEndorsements))));
	}
	return null;
/*
udanax-top.st:40067:AnyRecorderEFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	parent cast: SensorPropJoint into: [ :p |
		^AnyRecorderEFinder make: ((p relevantPermissions intersect: myPermissions) cast: IDRegion)
			with: myEndorsementsDelta
			with: ((p relevantEndorsements intersect: myNewEndorsements) cast: CrossRegion)].
	^NULL "fodder"!
*/
}
public AnyRecorderEFinder(Rcvr receiver) {
	super(receiver);
	myPermissions = (IDRegion) receiver.receiveHeaper();
	myEndorsementsDelta = (RegionDelta) receiver.receiveHeaper();
	myNewEndorsements = (CrossRegion) receiver.receiveHeaper();
/*
udanax-top.st:40077:AnyRecorderEFinder methodsFor: 'generated:'!
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
udanax-top.st:40083:AnyRecorderEFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myPermissions.
	xmtr sendHeaper: myEndorsementsDelta.
	xmtr sendHeaper: myNewEndorsements.!
*/
}
public static PropFinder make(IDRegion permissions, RegionDelta endorsementsDelta) {
	return make(permissions, endorsementsDelta, ((CrossRegion) (endorsementsDelta.after().minus(endorsementsDelta.before()))));
/*
udanax-top.st:40098:AnyRecorderEFinder class methodsFor: 'create'!
{PropFinder} make: permissions {IDRegion}
	with: endorsementsDelta {RegionDelta of: CrossRegion}
	
	^self make: permissions
		with: endorsementsDelta
		with: ((endorsementsDelta after minus: endorsementsDelta before) cast: CrossRegion)!
*/
}
public static PropFinder make(IDRegion permissions, RegionDelta endorsementsDelta, CrossRegion newEndorsements) {
	if (permissions.isEmpty() || (newEndorsements.isEmpty())) {
		return PropFinder.closedPropFinder();
	}
	return new AnyRecorderEFinder((SensorCrum.flagsFor(permissions, newEndorsements, false)), permissions, endorsementsDelta, newEndorsements);
/*
udanax-top.st:40105:AnyRecorderEFinder class methodsFor: 'create'!
{PropFinder} make: permissions {IDRegion}
	with: endorsementsDelta {RegionDelta of: CrossRegion}
	with: newEndorsements {CrossRegion}
	
	(permissions isEmpty or: [newEndorsements isEmpty]) ifTrue:
		[^PropFinder closedPropFinder].
	^self create: (SensorCrum flagsFor: permissions
			with: newEndorsements
			with: false)
		with: permissions with: endorsementsDelta with: newEndorsements!
*/
}
public AnyRecorderEFinder() {
/*

Generated during transformation
*/
}
}
