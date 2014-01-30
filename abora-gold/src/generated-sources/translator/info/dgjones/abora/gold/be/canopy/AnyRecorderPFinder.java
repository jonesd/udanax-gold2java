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
import info.dgjones.abora.gold.be.canopy.AnyRecorderFinder;
import info.dgjones.abora.gold.be.canopy.AnyRecorderPFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.ResultRecorderPFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.be.canopy.prop.SensorProp;
import info.dgjones.abora.gold.filter.RegionDelta;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.SensorPropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Generates finders for recorders triggered by an increase in permissions
 */
public class AnyRecorderPFinder extends AnyRecorderFinder {

	protected RegionDelta myPermissionsDelta;
	protected IDRegion myPermissions;
/*
udanax-top.st:40116:
AnyRecorderFinder subclass: #AnyRecorderPFinder
	instanceVariableNames: '
		myPermissionsDelta {RegionDelta of: IDRegion}
		myPermissions {IDRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:40122:
AnyRecorderPFinder comment:
'Generates finders for recorders triggered by an increase in permissions'!
*/
/*
udanax-top.st:40124:
(AnyRecorderPFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:40198:
AnyRecorderPFinder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:40201:
(AnyRecorderPFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(AnyRecorderPFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean match(Prop prop) {
	if (prop instanceof SensorProp) {
		SensorProp p = (SensorProp) prop;
		return p.relevantPermissions().intersects(myPermissions);
	}
	return false;
/*
udanax-top.st:40129:AnyRecorderPFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	prop cast: SensorProp into: [ :p |
		^p relevantPermissions intersects: myPermissions].
	^false "fodder"!
*/
}
public PropFinder nextFinder(BeEdition edition) {
	return ResultRecorderPFinder.make(edition, myPermissionsDelta, myPermissions, edition.totalEndorsements());
/*
udanax-top.st:40135:AnyRecorderPFinder methodsFor: 'accessing'!
{PropFinder} nextFinder: edition {BeEdition}
	^ResultRecorderPFinder make: edition
		with: myPermissionsDelta
		with: myPermissions
		with: edition totalEndorsements!
*/
}
public IDRegion permissions() {
	return myPermissions;
/*
udanax-top.st:40142:AnyRecorderPFinder methodsFor: 'accessing'!
{IDRegion} permissions 
	
	^myPermissions!
*/
}
public RegionDelta permissionsDelta() {
	return myPermissionsDelta;
/*
udanax-top.st:40146:AnyRecorderPFinder methodsFor: 'accessing'!
{RegionDelta of: IDRegion} permissionsDelta
	
	^myPermissionsDelta!
*/
}
public AnyRecorderPFinder(int flags, RegionDelta permissionsDelta, IDRegion permissions) {
	super(flags);
	myPermissionsDelta = permissionsDelta;
	myPermissions = permissions;
/*
udanax-top.st:40152:AnyRecorderPFinder methodsFor: 'create'!
create: flags {UInt32}
	with: permissionsDelta {RegionDelta of: IDRegion}
	with: permissions {IDRegion}
	
	super create: flags.
	myPermissionsDelta := permissionsDelta.
	myPermissions := permissions.!
*/
}
public int actualHashForEqual() {
	return myPermissionsDelta.hashForEqual() ^ myPermissions.hashForEqual();
/*
udanax-top.st:40162:AnyRecorderPFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myPermissionsDelta hashForEqual
		bitXor: myPermissions hashForEqual!
*/
}
public boolean isEqual(Heaper heaper) {
	if (heaper instanceof AnyRecorderPFinder) {
		AnyRecorderPFinder other = (AnyRecorderPFinder) heaper;
		return (myPermissionsDelta.isEqual(other.permissionsDelta())) && (myPermissions.isEqual(other.permissions()));
	}
	else {
		return false;
	}
/*
udanax-top.st:40167:AnyRecorderPFinder methodsFor: 'testing'!
{BooleanVar} isEqual: heaper {Heaper}
	heaper cast: AnyRecorderPFinder into: [ :other |
		^(myPermissionsDelta isEqual: other permissionsDelta)
			and: [myPermissions isEqual: other permissions]]
	others:
		[^false].
	^false "fodder"!
*/
}
public PropFinder oldPass(PropJoint parent) {
	if (parent instanceof SensorPropJoint) {
		SensorPropJoint p = (SensorPropJoint) parent;
		return AnyRecorderPFinder.make(myPermissionsDelta, ((IDRegion) (p.relevantPermissions().intersect(myPermissions))));
	}
	return null;
/*
udanax-top.st:40178:AnyRecorderPFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	parent cast: SensorPropJoint into: [ :p |
		^AnyRecorderPFinder make: myPermissionsDelta
			with: ((p relevantPermissions intersect: myPermissions) cast: IDRegion)].
	^NULL "fodder"!
*/
}
public AnyRecorderPFinder(Rcvr receiver) {
	super(receiver);
	myPermissionsDelta = (RegionDelta) receiver.receiveHeaper();
	myPermissions = (IDRegion) receiver.receiveHeaper();
/*
udanax-top.st:40187:AnyRecorderPFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myPermissionsDelta _ receiver receiveHeaper.
	myPermissions _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myPermissionsDelta);
	xmtr.sendHeaper(myPermissions);
/*
udanax-top.st:40192:AnyRecorderPFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myPermissionsDelta.
	xmtr sendHeaper: myPermissions.!
*/
}
public static PropFinder make(RegionDelta permissionsDelta) {
	return make(permissionsDelta, ((IDRegion) (permissionsDelta.after().minus(permissionsDelta.before()))));
/*
udanax-top.st:40206:AnyRecorderPFinder class methodsFor: 'create'!
{PropFinder} make: permissionsDelta {RegionDelta of: IDRegion}
	
	^self make: permissionsDelta
		with: ((permissionsDelta after minus: permissionsDelta before) cast: IDRegion)!
*/
}
public static PropFinder make(RegionDelta permissionsDelta, IDRegion newPermissions) {
	if (newPermissions.isEmpty()) {
		return PropFinder.closedPropFinder();
	}
	return new AnyRecorderPFinder((SensorCrum.flagsFor(newPermissions, null, false)), permissionsDelta, newPermissions);
/*
udanax-top.st:40211:AnyRecorderPFinder class methodsFor: 'create'!
{PropFinder} make: permissionsDelta {RegionDelta of: IDRegion}
	with: newPermissions {IDRegion}
	
	newPermissions isEmpty ifTrue:
		[^PropFinder closedPropFinder].
	^self create: (SensorCrum flagsFor: newPermissions with: NULL with: false)
		with: permissionsDelta with: newPermissions.!
*/
}
public AnyRecorderPFinder() {
/*

Generated during transformation
*/
}
}
