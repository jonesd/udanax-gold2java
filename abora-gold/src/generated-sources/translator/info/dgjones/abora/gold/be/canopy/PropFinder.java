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
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.BackfollowFinder;
import info.dgjones.abora.gold.be.canopy.BackfollowPFinder;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.CannotPartializeFinder;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.ClosedPropFinder;
import info.dgjones.abora.gold.be.canopy.OpenPropFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.RegionDelta;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.XuRegion;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * For filtering by canopies.  Matches against Props and CanopyCrum flags
 */
public class PropFinder extends Heaper {

	protected int myFlags;
/*
udanax-top.st:39307:
Heaper subclass: #PropFinder
	instanceVariableNames: 'myFlags {UInt32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39311:
PropFinder comment:
'For filtering by canopies.  Matches against Props and CanopyCrum flags'!
*/
/*
udanax-top.st:39313:
(PropFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:39367:
PropFinder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:39370:
(PropFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PropFinder.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public PropFinder() {
	super();
	/* for generated code */
/*
udanax-top.st:39318:PropFinder methodsFor: 'create'!
create
	super create "for generated code"!
*/
}
public PropFinder(int flags) {
	super();
	myFlags = flags;
/*
udanax-top.st:39321:PropFinder methodsFor: 'create'!
create: flags {UInt32}
	super create.
	myFlags := flags.!
*/
}
/**
 * return whether the propJoint passes the finder
 */
public boolean doesPass(CanopyCrum parent) {
	return (myFlags | parent.flags()) != 0;
/*
udanax-top.st:39328:PropFinder methodsFor: 'accessing'!
{BooleanVar} doesPass: parent {CanopyCrum}
	"return whether the propJoint passes the finder"
	^(myFlags bitOr: parent flags) ~= UInt32Zero!
*/
}
/**
 * During a southwards walk of a multi-Edition (aka multi-Stamp), normally we simplify the
 * finder by using PropFinder>>pass:.  However, when we cross an internal Edition boundary
 * and are about to walk into the O-plane of that contained edition we call this method
 * (findPast:) to get the new PropFinder.
 */
public PropFinder findPast(BeEdition stamp) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39333:PropFinder methodsFor: 'accessing'!
{PropFinder} findPast: stamp {BeEdition}
	"During a southwards walk of a multi-Edition (aka multi-Stamp), normally we simplify the finder by using PropFinder>>pass:.  However, when we cross an internal Edition boundary and are about to walk into the O-plane of that contained edition we call this method (findPast:) to get the new PropFinder."
	
	self subclassResponsibility!
*/
}
public int flags() {
	return myFlags;
/*
udanax-top.st:39338:PropFinder methodsFor: 'accessing'!
{UInt32} flags
	^myFlags!
*/
}
/**
 * Overridden only in ClosedPropFinder
 */
public boolean isEmpty() {
	return false;
/*
udanax-top.st:39342:PropFinder methodsFor: 'accessing'!
{BooleanVar} isEmpty
	"Overridden only in ClosedPropFinder"
	^false!
*/
}
/**
 * Overridden only in OpenPropFinder
 */
public boolean isFull() {
	return false;
/*
udanax-top.st:39346:PropFinder methodsFor: 'accessing'!
{BooleanVar} isFull
	"Overridden only in OpenPropFinder"
	^false!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39350:PropFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	"tell whether a prop matches this filter"
	self subclassResponsibility!
*/
}
/**
 * return a simple enough finder for looking at the children
 */
public PropFinder pass(CanopyCrum parent) {
	if (doesPass(parent)) {
		return this;
	}
	else {
		return PropFinder.closedPropFinder();
	}
/*
udanax-top.st:39354:PropFinder methodsFor: 'accessing'!
{PropFinder} pass: parent {CanopyCrum}
	"return a simple enough finder for looking at the children"
	(self doesPass: parent)
		ifTrue: [^self]
		ifFalse: [^PropFinder closedPropFinder]!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:39363:PropFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static PropFinder backfollowFinder(Filter permissionsFilter) {
	if (permissionsFilter.isEmpty()) {
		return PropFinder.closedPropFinder();
	}
	else {
		return new BackfollowPFinder((BertCrum.flagsFor(((IDRegion) permissionsFilter.relevantRegion()), null, false, false)), permissionsFilter);
	}
/*
udanax-top.st:39375:PropFinder class methodsFor: 'creation'!
{PropFinder} backfollowFinder: permissionsFilter {Filter of: (XnRegion of: ID)}
	permissionsFilter isEmpty
		ifTrue: [^PropFinder closedPropFinder]
		ifFalse: [^BackfollowPFinder 
			create: (BertCrum
				flagsFor: (permissionsFilter relevantRegion cast: IDRegion)
				with: NULL
				with: false
				with: false)
			with: permissionsFilter]!
*/
}
public static PropFinder backfollowFinder(Filter permissionsFilter, Filter endorsementsFilter) {
	if (permissionsFilter.isEmpty() || (endorsementsFilter.isEmpty())) {
		return PropFinder.closedPropFinder();
	}
	if (endorsementsFilter.isFull()) {
		return new BackfollowPFinder((BertCrum.flagsFor(((IDRegion) permissionsFilter.relevantRegion()), null, false, false)), permissionsFilter);
	}
	return new BackfollowFinder((BertCrum.flagsFor(((IDRegion) permissionsFilter.relevantRegion()), ((CrossRegion) endorsementsFilter.relevantRegion()), false, false)), permissionsFilter, endorsementsFilter);
/*
udanax-top.st:39386:PropFinder class methodsFor: 'creation'!
{PropFinder} backfollowFinder: permissionsFilter {Filter of: (XnRegion of: ID)} 
	with: endorsementsFilter {Filter of: (XnRegion of: ID)}
	(permissionsFilter isEmpty or: [endorsementsFilter isEmpty]) ifTrue:
		[^PropFinder closedPropFinder].
	endorsementsFilter isFull ifTrue:
		[^BackfollowPFinder create: (BertCrum
				flagsFor: (permissionsFilter relevantRegion cast: IDRegion)
				with: NULL
				with: false
				with: false)
			with: permissionsFilter].
	^BackfollowFinder create: (BertCrum
				flagsFor: (permissionsFilter relevantRegion cast: IDRegion)
				with: (endorsementsFilter relevantRegion cast: CrossRegion)
				with: false
				with: false)
			with: permissionsFilter
			with: endorsementsFilter!
*/
}
public static PropFinder cannotPartializeFinder() {
	return new CannotPartializeFinder();
/*
udanax-top.st:39405:PropFinder class methodsFor: 'creation'!
{PropFinder} cannotPartializeFinder
	^CannotPartializeFinder create!
*/
}
public static PropFinder closedPropFinder() {
	return new ClosedPropFinder();
/*
udanax-top.st:39408:PropFinder class methodsFor: 'creation'!
{PropFinder} closedPropFinder
	^ClosedPropFinder create!
*/
}
public static PropFinder openPropFinder() {
	return new OpenPropFinder();
/*
udanax-top.st:39411:PropFinder class methodsFor: 'creation'!
{PropFinder} openPropFinder
	^OpenPropFinder create!
*/
}
public static PropFinder sensorFinder() {
	return new SensorFinder();
/*
udanax-top.st:39414:PropFinder class methodsFor: 'creation'!
{PropFinder} sensorFinder
	^SensorFinder create!
*/
}
/**
 * @deprecated
 */
public static PropFinder partialityFinder() {
	throw new PasseException();
/*
udanax-top.st:39419:PropFinder class methodsFor: 'smalltalk: passe'!
{PropFinder} partialityFinder
	self passe!
*/
}
/**
 * @deprecated
 */
public static PropFinder recorderFinderRegionDelta(RegionDelta permissionsDelta, RegionDelta endorsementsDelta) {
	throw new PasseException();
/*
udanax-top.st:39423:PropFinder class methodsFor: 'smalltalk: passe'!
{PropFinder} recorderFinder.RegionDelta: permissionsDelta {RegionDelta of: (XuRegion of: ID)} 
	with.RegionDelta: endorsementsDelta {RegionDelta of: (XuRegion of: ID)} 
	self passe.!
*/
}
/**
 * @deprecated
 */
public static PropFinder recorderFinderRegionDelta(RegionDelta permissionsDelta, XuRegion endorsements) {
	throw new PasseException();
/*
udanax-top.st:39428:PropFinder class methodsFor: 'smalltalk: passe'!
{PropFinder} recorderFinder.RegionDelta: permissionsDelta {RegionDelta of: (XuRegion of: ID)} 
	with.XuRegion: endorsements {XuRegion of: ID} 
	self passe.!
*/
}
/**
 * @deprecated
 */
public static PropFinder recorderFinderXuRegion(XuRegion permissions, RegionDelta endorsementsDelta) {
	throw new PasseException();
/*
udanax-top.st:39433:PropFinder class methodsFor: 'smalltalk: passe'!
{PropFinder} recorderFinder.XuRegion: permissions {XuRegion of: ID} 
	with.RegionDelta: endorsementsDelta {RegionDelta of: (XuRegion of: ID)} 
	self passe.!
*/
}
/**
 * @deprecated
 */
public static PropFinder recorderPFinder(BeRangeElement element, RegionDelta permissionsDelta, CrossRegion endorsements) {
	throw new PasseException();
/*
udanax-top.st:39438:PropFinder class methodsFor: 'smalltalk: passe'!
{PropFinder} recorderPFinder: element {BeRangeElement}
	with: permissionsDelta {RegionDelta of: IDRegion}
	with: endorsements {CrossRegion of: IDRegion and: IDRegion}
	
	self passe!
*/
}
public PropFinder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
