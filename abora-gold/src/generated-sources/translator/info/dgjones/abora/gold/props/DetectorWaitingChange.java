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
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.BertPropJoint;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.DetectorWaitingChange;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * The "Detector Waiting" property is a Bert Canopy property to remember that an Edition has
 * a Detector waiting for PlaceHolders to be filled in.
 */
public class DetectorWaitingChange extends PropChange {

/*
udanax-top.st:38808:
PropChange subclass: #DetectorWaitingChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-props'!
*/
/*
udanax-top.st:38812:
DetectorWaitingChange comment:
'The "Detector Waiting" property is a Bert Canopy property to remember that an Edition has a Detector waiting for PlaceHolders to be filled in.'!
*/
/*
udanax-top.st:38814:
(DetectorWaitingChange getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DetectorWaitingChange.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * compare the changed parts of two Props
 */
public boolean areEqualProps(Prop a, Prop b) {
	return ((BertProp) a).isSensorWaiting() == ((BertProp) b).isSensorWaiting();
/*
udanax-top.st:38819:DetectorWaitingChange methodsFor: 'accessing'!
{BooleanVar} areEqualProps: a {Prop} with: b {Prop}
	"compare the changed parts of two Props"
	^(a cast: BertProp) isSensorWaiting == (b cast: BertProp) isSensorWaiting!
*/
}
public Prop changed(Prop old, Prop a) {
	BertProp bp;
	BertProp abp;
	bp = (BertProp) old;
	abp = (BertProp) a;
	if (bp.isSensorWaiting() == abp.isSensorWaiting()) {
		return old;
	}
	return BertProp.make(bp.permissions(), bp.endorsements(), abp.isSensorWaiting(), bp.isNotPartializable());
/*
udanax-top.st:38823:DetectorWaitingChange methodsFor: 'accessing'!
{Prop} changed: old {Prop} with: a {Prop}
	| bp {BertProp wimpy} abp {BertProp wimpy} |
	bp _ old cast: BertProp.
	abp _ a cast: BertProp.
	bp isSensorWaiting == abp isSensorWaiting ifTrue: [^old].
	^BertProp make: bp permissions
		with: bp endorsements
		with: abp isSensorWaiting
		with: bp isNotPartializable!
*/
}
public PropFinder fetchFinder(Prop before, Prop after, BeRangeElement element, PropFinder oldFinder) {
	return null;
/*
udanax-top.st:38834:DetectorWaitingChange methodsFor: 'accessing'!
{PropFinder | NULL} fetchFinder: before {Prop unused}
	with: after {Prop unused}
	with: element {BeRangeElement unused}
	with: oldFinder {PropFinder unused | NULL}
	^NULL!
*/
}
public boolean isFull() {
	return false;
/*
udanax-top.st:38841:DetectorWaitingChange methodsFor: 'accessing'!
{BooleanVar} isFull
	^false!
*/
}
public Prop with(Prop old, Prop a) {
	BertProp bp;
	BertProp abp;
	bp = (BertProp) old;
	abp = (BertProp) a;
	if (bp.isSensorWaiting() || ( ! abp.isSensorWaiting())) {
		return old;
	}
	else {
		return BertProp.make(bp.permissions(), bp.endorsements(), abp.isSensorWaiting(), bp.isNotPartializable());
	}
/*
udanax-top.st:38845:DetectorWaitingChange methodsFor: 'accessing'!
{Prop} with: old {Prop} with: a {Prop}
	| bp {BertProp wimpy} abp {BertProp wimpy} |
	bp _ old cast: BertProp.
	abp _ a cast: BertProp.
	(bp isSensorWaiting or: [abp isSensorWaiting not])
		ifTrue: [^old]
		ifFalse: [^BertProp make: bp permissions
			with: bp endorsements
			with: abp isSensorWaiting
			with: bp isNotPartializable]!
*/
}
/**
 * compare the changed parts of two PropJoints
 */
public boolean areEqualPropJoints(PropJoint a, PropJoint b) {
	return ((BertPropJoint) a).isSensorWaiting() == ((BertPropJoint) b).isSensorWaiting();
/*
udanax-top.st:38859:DetectorWaitingChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} areEqualPropJoints: a {PropJoint} with: b {PropJoint}
	"compare the changed parts of two PropJoints"
	^(a cast: BertPropJoint) isSensorWaiting == (b cast: BertPropJoint) isSensorWaiting!
*/
}
public PropJoint changedJoint(PropJoint old, PropJoint a) {
	BertPropJoint bpj;
	BertPropJoint one;
	bpj = (BertPropJoint) old;
	one = (BertPropJoint) a;
	if (bpj.isSensorWaiting() == one.isSensorWaiting()) {
		return old;
	}
	else {
		return BertPropJoint.make(bpj.permissionsJoint(), bpj.endorsementsJoint(), one.isSensorWaiting(), bpj.isNotPartializable());
	}
/*
udanax-top.st:38863:DetectorWaitingChange methodsFor: 'smalltalk: suspended'!
{PropJoint} changedJoint: old {PropJoint} with: a {PropJoint}
	
	| bpj {BertPropJoint wimpy} one {BertPropJoint wimpy} |
	bpj _ old cast: BertPropJoint.
	one _ a cast: BertPropJoint.
	bpj isSensorWaiting == one isSensorWaiting
		ifTrue: [^old]
		ifFalse: [^BertPropJoint make: bpj permissionsJoint
				with: bpj endorsementsJoint
				with: one isSensorWaiting
				with: bpj isNotPartializable]!
*/
}
public boolean isEqualToJointOf(PropJoint a, Prop b) {
	return ((BertPropJoint) a).isSensorWaiting() == ((BertProp) b).isSensorWaiting();
/*
udanax-top.st:38875:DetectorWaitingChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} isEqualToJointOf: a {PropJoint} with: b {Prop}
	^(a cast: BertPropJoint) isSensorWaiting == (b cast: BertProp) isSensorWaiting!
*/
}
public PropJoint join(PropJoint old, PropJoint a, PropJoint b) {
	BertPropJoint bpj;
	BertPropJoint one;
	BertPropJoint two;
	bpj = (BertPropJoint) old;
	one = (BertPropJoint) a;
	two = (BertPropJoint) b;
	if (bpj.isSensorWaiting() == (one.isSensorWaiting() || (two.isSensorWaiting()))) {
		return old;
	}
	else {
		return BertPropJoint.make(bpj.permissionsJoint(), bpj.endorsementsJoint(), ! bpj.isSensorWaiting(), bpj.isNotPartializable());
	}
/*
udanax-top.st:38879:DetectorWaitingChange methodsFor: 'smalltalk: suspended'!
{PropJoint} join: old {PropJoint} with: a {PropJoint} with: b {PropJoint}
	| bpj {BertPropJoint wimpy} one {BertPropJoint wimpy} two {BertPropJoint wimpy} |
	bpj _ old cast: BertPropJoint.
	one _ a cast: BertPropJoint.
	two _ b cast: BertPropJoint.
	bpj isSensorWaiting == (one isSensorWaiting
			or: [two isSensorWaiting])
		ifTrue: [^old]
		ifFalse: [^BertPropJoint make: bpj permissionsJoint
				with: bpj endorsementsJoint
				with: bpj isSensorWaiting not
				with: bpj isNotPartializable]!
*/
}
public PropJoint joinProp(PropJoint old, PropJoint a, PropJoint b, Prop prop) {
	BertPropJoint bpj;
	BertPropJoint one;
	BertPropJoint two;
	BertProp p;
	bpj = (BertPropJoint) old;
	one = (BertPropJoint) a;
	two = (BertPropJoint) b;
	p = (BertProp) prop;
	if (bpj.isSensorWaiting() == (one.isSensorWaiting() || (two.isSensorWaiting() || (p.isSensorWaiting())))) {
		return old;
	}
	else {
		return BertPropJoint.make(bpj.permissionsJoint(), bpj.endorsementsJoint(), ! bpj.isSensorWaiting(), bpj.isNotPartializable());
	}
/*
udanax-top.st:38893:DetectorWaitingChange methodsFor: 'smalltalk: suspended'!
{PropJoint} joinProp: old {PropJoint} with: a {PropJoint} with: b {PropJoint} with: prop {Prop}
	| bpj {BertPropJoint wimpy} one {BertPropJoint wimpy} two {BertPropJoint wimpy} p {BertProp wimpy} |
	bpj _ old cast: BertPropJoint.
	one _ a cast: BertPropJoint.
	two _ b cast: BertPropJoint.
	p _ prop cast: BertProp.
	bpj isSensorWaiting == (one isSensorWaiting
			or: [two isSensorWaiting
			or: [p isSensorWaiting]])
		ifTrue: [^old]
		ifFalse: [^BertPropJoint make: bpj permissionsJoint
				with: bpj endorsementsJoint
				with: bpj isSensorWaiting not
				with: bpj isNotPartializable]!
*/
}
public DetectorWaitingChange(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:38911:DetectorWaitingChange methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:38914:DetectorWaitingChange methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public DetectorWaitingChange() {
/*

Generated during transformation
*/
}
}
