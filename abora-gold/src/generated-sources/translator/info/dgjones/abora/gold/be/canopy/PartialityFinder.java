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
import info.dgjones.abora.gold.be.canopy.PartialityFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.canopy.SensorPropFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.be.canopy.prop.SensorProp;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.SensorPropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * When walking the o-plane to separate out the "partial" part of an Edition, this finder is
 * used to filter the walk according to the sensor canopy.
 */
public class PartialityFinder extends SensorPropFinder {

/*
udanax-top.st:40869:
SensorPropFinder subclass: #PartialityFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:40873:
PartialityFinder comment:
'When walking the o-plane to separate out the "partial" part of an Edition, this finder is used to filter the walk according to the sensor canopy.'!
*/
/*
udanax-top.st:40875:
(PartialityFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SMALLTALK.ONLY; add: #OBSOLETE; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PartialityFinder.class).setAttributes( new Set().add("COPY").add("SMALLTALKONLY").add("OBSOLETE").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public PartialityFinder() {
	super(SensorCrum.isPartialFlag());
/*
udanax-top.st:40880:PartialityFinder methodsFor: 'create'!
create
	super create: SensorCrum isPartialFlag!
*/
}
public PropFinder findPast(BeEdition stamp) {
	return this;
/*
udanax-top.st:40886:PartialityFinder methodsFor: 'accessing'!
{PropFinder} findPast: stamp {BeEdition unused}
	
	^self!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	return ((SensorProp) prop).isPartial();
/*
udanax-top.st:40890:PartialityFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	"tell whether a prop matches this filter"
	^(prop cast: SensorProp) isPartial!
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual();
/*
udanax-top.st:40896:PartialityFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	return other instanceof PartialityFinder;
/*
udanax-top.st:40900:PartialityFinder methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^other isKindOf: PartialityFinder!
*/
}
/**
 * return a simple enough finder for looking at the children
 */
public PropFinder oldPass(PropJoint parent) {
	if (((SensorPropJoint) parent).isPartial()) {
		return this;
	}
	else {
		return PropFinder.closedPropFinder();
	}
/*
udanax-top.st:40906:PartialityFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	"return a simple enough finder for looking at the children"
	(parent cast: SensorPropJoint) isPartial
		ifTrue: [^self]
		ifFalse: [^PropFinder closedPropFinder]!
*/
}
public PartialityFinder(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:40914:PartialityFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:40917:PartialityFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
}
