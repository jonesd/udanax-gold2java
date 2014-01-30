/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.props;

import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.be.canopy.prop.SensorProp;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.SensorPropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.FullPropChange;
import info.dgjones.abora.gold.props.SensorPropChange;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Use when it is fine to consider that all aspects of the SensorProp may have changed
 */
public class SensorPropChange extends FullPropChange {

/*
udanax-top.st:39147:
FullPropChange subclass: #SensorPropChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-props'!
*/
/*
udanax-top.st:39151:
SensorPropChange comment:
'Use when it is fine to consider that all aspects of the SensorProp may have changed'!
*/
/*
udanax-top.st:39153:
(SensorPropChange getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SensorPropChange.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public PropFinder fetchFinder(Prop before, Prop after, BeRangeElement element, PropFinder oldFinder) {
	return null;
/*
udanax-top.st:39158:SensorPropChange methodsFor: 'accessing'!
{PropFinder | NULL} fetchFinder: before {Prop unused}
	with: after {Prop unused}
	with: element {BeRangeElement unused}
	with: oldFinder {PropFinder unused | NULL}
	^NULL!
*/
}
public boolean isEqualToJointOf(PropJoint a, Prop b) {
	if (a instanceof SensorPropJoint) {
		SensorPropJoint spj = (SensorPropJoint) a;
		if (b instanceof SensorProp) {
			SensorProp sp = (SensorProp) b;
			return (spj.relevantEndorsements().isEqual(sp.relevantEndorsements())) && ((spj.relevantPermissions().isEqual(sp.relevantPermissions())) && (spj.isPartial() == sp.isPartial()));
		}
	}
	return false;
/*
udanax-top.st:39167:SensorPropChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} isEqualToJointOf: a {PropJoint} with: b {Prop}
	
	a cast: SensorPropJoint into: [ :spj |
	b cast: SensorProp into: [ :sp |
		^(spj relevantEndorsements isEqual: sp relevantEndorsements)
			and: [(spj relevantPermissions isEqual: sp relevantPermissions)
			and: [spj isPartial == sp isPartial]]]].
	^false "fodder"!
*/
}
public SensorPropChange(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:39178:SensorPropChange methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:39181:SensorPropChange methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public SensorPropChange() {
/*

Generated during transformation
*/
}
}
