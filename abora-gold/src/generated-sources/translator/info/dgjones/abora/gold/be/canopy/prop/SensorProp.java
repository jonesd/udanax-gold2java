/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy.prop;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.be.canopy.prop.SensorProp;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.SensorPropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * The properties which are nevigable towards using the Sensor Canopy.  The permissions and
 * endorsements are those whose changes may affect the triggering of the recorders that
 * decorate the canopy.  myPartialFlag is a property of the o-leaf-stuff which are at the
 * leaves of the Sensor Canopy.
 */
public class SensorProp extends Prop {

	protected IDRegion myRelevantPermissions;
	protected CrossRegion myRelevantEndorsements;
	protected boolean myPartialFlag;
	protected static SensorProp TheIdentitySensorProp;
	protected static SensorProp ThePartialSensorProp;
/*
udanax-top.st:38392:
Prop subclass: #SensorProp
	instanceVariableNames: '
		myRelevantPermissions {IDRegion}
		myRelevantEndorsements {CrossRegion of: IDRegion and: IDRegion}
		myPartialFlag {BooleanVar}'
	classVariableNames: '
		TheIdentitySensorProp {SensorProp} 
		ThePartialSensorProp {SensorProp} '
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy-Prop'!
*/
/*
udanax-top.st:38401:
SensorProp comment:
'The properties which are nevigable towards using the Sensor Canopy.  The permissions and endorsements are those whose changes may affect the triggering of the recorders that decorate the canopy.  myPartialFlag is a property of the o-leaf-stuff which are at the leaves of the Sensor Canopy.'!
*/
/*
udanax-top.st:38403:
(SensorProp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:38507:
SensorProp class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:38510:
(SensorProp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SensorProp.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public SensorProp(IDRegion relevantPermissions, CrossRegion relevantEndorsements, boolean isPartial) {
	super();
	myRelevantPermissions = relevantPermissions;
	myRelevantEndorsements = relevantEndorsements;
	myPartialFlag = isPartial;
/*
udanax-top.st:38408:SensorProp methodsFor: 'creation'!
create: relevantPermissions {IDRegion} 
	with: relevantEndorsements {CrossRegion of: IDRegion and: IDRegion} 
	with: isPartial {BooleanVar}
	
	super create.
	myRelevantPermissions _ relevantPermissions.
	myRelevantEndorsements _ relevantEndorsements.
	myPartialFlag _ isPartial.!
*/
}
public int flags() {
	return SensorCrum.flagsFor(myRelevantPermissions, myRelevantEndorsements, myPartialFlag);
/*
udanax-top.st:38419:SensorProp methodsFor: 'accessing'!
{UInt32} flags
	^SensorCrum flagsFor: myRelevantPermissions
		with: myRelevantEndorsements
		with: myPartialFlag!
*/
}
public boolean isPartial() {
	return myPartialFlag;
/*
udanax-top.st:38425:SensorProp methodsFor: 'accessing'!
{BooleanVar} isPartial
	^myPartialFlag!
*/
}
public CrossRegion relevantEndorsements() {
	return myRelevantEndorsements;
/*
udanax-top.st:38428:SensorProp methodsFor: 'accessing'!
{CrossRegion} relevantEndorsements
	^myRelevantEndorsements!
*/
}
public IDRegion relevantPermissions() {
	return myRelevantPermissions;
/*
udanax-top.st:38432:SensorProp methodsFor: 'accessing'!
{IDRegion} relevantPermissions
	^myRelevantPermissions!
*/
}
public Prop with(Prop other) {
	if (other instanceof SensorProp) {
		SensorProp o = (SensorProp) other;
		return SensorProp.make(((IDRegion) (myRelevantPermissions.unionWith(o.relevantPermissions()))), ((CrossRegion) (myRelevantEndorsements.unionWith(o.relevantEndorsements()))), (myPartialFlag || (o.isPartial())));
	}
	return null;
/*
udanax-top.st:38436:SensorProp methodsFor: 'accessing'!
{Prop} with: other {Prop}
	
	other cast: SensorProp into: [ :o |
		^SensorProp make: ((myRelevantPermissions
				unionWith: o relevantPermissions) cast: IDRegion)
			with: ((myRelevantEndorsements
				unionWith: o relevantEndorsements) cast: CrossRegion)
			with: (myPartialFlag or: [o isPartial])].
	^ NULL "compiler fodder"!
*/
}
public int actualHashForEqual() {
	return myRelevantPermissions.hashForEqual() ^ myRelevantEndorsements.hashForEqual();
/*
udanax-top.st:38448:SensorProp methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myRelevantPermissions hashForEqual
		bitXor: myRelevantEndorsements hashForEqual!
*/
}
public boolean isEqual(Heaper heaper) {
	if (heaper instanceof SensorProp) {
		SensorProp prop = (SensorProp) heaper;
		return (myRelevantEndorsements.isEqual(prop.relevantEndorsements())) && ((myRelevantPermissions.isEqual(prop.relevantPermissions())) && (myPartialFlag == prop.isPartial()));
	}
	else {
		return false;
	}
/*
udanax-top.st:38453:SensorProp methodsFor: 'testing'!
{BooleanVar} isEqual: heaper {Heaper}
	heaper
		cast: SensorProp into: [ :prop |
			^(myRelevantEndorsements isEqual: prop relevantEndorsements)
				and: [(myRelevantPermissions isEqual: prop relevantPermissions)
				and: [myPartialFlag == prop isPartial]]]
		others:
			[^false].
	^ false "compiler fodder"!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("SensorProp(P: ");
	oo.print(myRelevantPermissions);
	oo.print("; E: ");
	oo.print(myRelevantEndorsements);
	if (myPartialFlag) {
		oo.print("; partial");
	}
	oo.print(")");
/*
udanax-top.st:38466:SensorProp methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << 'SensorProp(P: ' << myRelevantPermissions
		<< '; E: ' << myRelevantEndorsements.
	myPartialFlag ifTrue: [oo << '; partial'].
	oo << ')'!
*/
}
/**
 * @deprecated
 */
public Filter endorsementsFilter() {
	throw new PasseException();
/*
udanax-top.st:38475:SensorProp methodsFor: 'smalltalk: passe'!
{Filter of: (XuRegion of: ID)} endorsementsFilter
	self passe!
*/
}
/**
 * @deprecated
 */
public Filter permissionsFilter() {
	throw new PasseException();
/*
udanax-top.st:38479:SensorProp methodsFor: 'smalltalk: passe'!
{Filter of: (XuRegion of: ID)} permissionsFilter
	self passe!
*/
}
public PropJoint joint() {
	Ravi.thingToDo();
	/* implement proper simpleRegions so we can use simpleUnion */
	return SensorPropJoint.make(((IDRegion) myRelevantPermissions
	/* asSimpleRegion */
	), ((CrossRegion) myRelevantEndorsements
	/* asSimpleRegion */
	), myPartialFlag);
/*
udanax-top.st:38485:SensorProp methodsFor: 'smalltalk: suspended'!
{PropJoint} joint
	Ravi thingToDo. "implement proper simpleRegions so we can use simpleUnion"
	^SensorPropJoint make: (myRelevantPermissions "asSimpleRegion" cast: IDRegion)
		with: (myRelevantEndorsements "asSimpleRegion" cast: CrossRegion)
		with: myPartialFlag!
*/
}
public SensorProp(Rcvr receiver) {
	super(receiver);
	myRelevantPermissions = (IDRegion) receiver.receiveHeaper();
	myRelevantEndorsements = (CrossRegion) receiver.receiveHeaper();
	myPartialFlag = receiver.receiveBooleanVar();
/*
udanax-top.st:38494:SensorProp methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myRelevantPermissions _ receiver receiveHeaper.
	myRelevantEndorsements _ receiver receiveHeaper.
	myPartialFlag _ receiver receiveBooleanVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myRelevantPermissions);
	xmtr.sendHeaper(myRelevantEndorsements);
	xmtr.sendBooleanVar(myPartialFlag);
/*
udanax-top.st:38500:SensorProp methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myRelevantPermissions.
	xmtr sendHeaper: myRelevantEndorsements.
	xmtr sendBooleanVar: myPartialFlag.!
*/
}
/**
 * returns an empty SensorProp
 */
public static SensorProp make() {
	if (TheIdentitySensorProp == null) {
		TheIdentitySensorProp = new SensorProp(((IDRegion) ((BeGrandMap) CurrentGrandMap.fluidGet()).globalIDSpace().emptyRegion()), ((CrossRegion) ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementSpace().emptyRegion()), false);
	}
	return TheIdentitySensorProp;
/*
udanax-top.st:38515:SensorProp class methodsFor: 'creation'!
make
	"returns an empty SensorProp"
	
	TheIdentitySensorProp == NULL ifTrue:
		[TheIdentitySensorProp := self
			create: (CurrentGrandMap fluidGet globalIDSpace emptyRegion cast: IDRegion)
			with: (CurrentGrandMap fluidGet endorsementSpace emptyRegion cast: CrossRegion)
			with: false].
	^TheIdentitySensorProp!
*/
}
public static SensorProp make(IDRegion relevantPermissions, CrossRegion relevantEndorsements, boolean isPartial) {
	return new SensorProp(relevantPermissions, relevantEndorsements, isPartial);
/*
udanax-top.st:38525:SensorProp class methodsFor: 'creation'!
make: relevantPermissions {IDRegion} 
	with: relevantEndorsements {CrossRegion} 
	with: isPartial {BooleanVar}
	^self create: relevantPermissions with: relevantEndorsements with: isPartial!
*/
}
/**
 * returns an empty SensorProp with the partial flag on
 */
public static SensorProp partial() {
	if (ThePartialSensorProp == null) {
		ThePartialSensorProp = new SensorProp(((IDRegion) ((BeGrandMap) CurrentGrandMap.fluidGet()).globalIDSpace().emptyRegion()), ((CrossRegion) ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementSpace().emptyRegion()), true);
	}
	return ThePartialSensorProp;
/*
udanax-top.st:38531:SensorProp class methodsFor: 'creation'!
{SensorProp} partial
	"returns an empty SensorProp with the partial flag on"
	ThePartialSensorProp == NULL ifTrue:
		[ThePartialSensorProp := self
			create: (CurrentGrandMap fluidGet globalIDSpace emptyRegion cast: IDRegion)
			with: (CurrentGrandMap fluidGet endorsementSpace emptyRegion cast: CrossRegion)
			with: true].
	^ThePartialSensorProp!
*/
}
public static void linkTimeNonInherited() {
	TheIdentitySensorProp = null;
	ThePartialSensorProp = null;
/*
udanax-top.st:38543:SensorProp class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	TheIdentitySensorProp _ NULL.
	ThePartialSensorProp _ NULL.!
*/
}
public SensorProp() {
/*

Generated during transformation
*/
}
}
