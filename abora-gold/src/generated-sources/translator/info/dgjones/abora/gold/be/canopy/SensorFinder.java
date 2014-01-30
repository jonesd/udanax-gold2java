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
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.BertPropFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorFinder;
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.BertPropJoint;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Currently unused but will be re-instated.  Used to find which containing Editions have
 * WaitForCompletionDetectors installed on them so that they can be rung when placegholders
 * get filled in.
 */
public class SensorFinder extends BertPropFinder {

/*
udanax-top.st:39694:
BertPropFinder subclass: #SensorFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39698:
SensorFinder comment:
'Currently unused but will be re-instated.  Used to find which containing Editions have WaitForCompletionDetectors installed on them so that they can be rung when placegholders get filled in.'!
*/
/*
udanax-top.st:39700:
(SensorFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SensorFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public SensorFinder() {
	super(BertCrum.isSensorWaitingFlag());
/*
udanax-top.st:39705:SensorFinder methodsFor: 'create'!
create
	super create: BertCrum isSensorWaitingFlag!
*/
}
public PropFinder findPast(BeEdition edition) {
	return PropFinder.closedPropFinder();
/*
udanax-top.st:39711:SensorFinder methodsFor: 'accessing'!
{PropFinder} findPast: edition {BeEdition unused}
	
	^PropFinder closedPropFinder "Dont look for Detectors past an Edition boundary"!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	return ((BertProp) prop).isSensorWaiting();
/*
udanax-top.st:39715:SensorFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	"tell whether a prop matches this filter"
	^(prop cast: BertProp) isSensorWaiting!
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual();
/*
udanax-top.st:39721:SensorFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	return other instanceof SensorFinder;
/*
udanax-top.st:39725:SensorFinder methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^other isKindOf: SensorFinder!
*/
}
/**
 * return a simple enough finder for looking at the children
 */
public PropFinder oldPass(PropJoint parent) {
	if (((BertPropJoint) parent).isSensorWaiting()) {
		return this;
	}
	else {
		return PropFinder.closedPropFinder();
	}
/*
udanax-top.st:39731:SensorFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	"return a simple enough finder for looking at the children"
	(parent cast: BertPropJoint) isSensorWaiting
		ifTrue: [^self]
		ifFalse: [^PropFinder closedPropFinder]!
*/
}
public SensorFinder(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:39739:SensorFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:39742:SensorFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
}
