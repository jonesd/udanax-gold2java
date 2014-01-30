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
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorPropFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Used to filter by the sensor canopy
 */
public class SensorPropFinder extends PropFinder {

/*
udanax-top.st:39847:
PropFinder subclass: #SensorPropFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39851:
SensorPropFinder comment:
'Used to filter by the sensor canopy'!
*/
/*
udanax-top.st:39853:
(SensorPropFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SensorPropFinder.class).setAttributes( new Set().add("NOTATYPE").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public SensorPropFinder() {
	super();
	/* for generated code */
/*
udanax-top.st:39858:SensorPropFinder methodsFor: 'create'!
create
	super create "for generated code"!
*/
}
public SensorPropFinder(int flags) {
	super(flags);
/*
udanax-top.st:39861:SensorPropFinder methodsFor: 'create'!
create: flags {UInt32}
	super create: flags!
*/
}
public PropFinder findPast(BeEdition stamp) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39867:SensorPropFinder methodsFor: 'accessing'!
{PropFinder} findPast: stamp {BeEdition}
	
	self subclassResponsibility!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39871:SensorPropFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	"tell whether a prop matches this filter"
	self subclassResponsibility!
*/
}
public PropFinder oldPass(CanopyCrum crum) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39877:SensorPropFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: crum {CanopyCrum}
	self subclassResponsibility!
*/
}
public SensorPropFinder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
