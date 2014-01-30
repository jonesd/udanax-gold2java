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
import info.dgjones.abora.gold.props.CannotPartializeChange;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * The "Cannot Partialize" property is a Bert Canopy property to remember that a Stamp is
 * actively being viewed (by a session level Orgl) and therefore cannot be poured-out (made
 * more partial).  Should probably not be a Prop(erty), by rather a NOCOPY session level bit
 * in the BertCrums.
 */
public class CannotPartializeChange extends PropChange {

/*
udanax-top.st:38699:
PropChange subclass: #CannotPartializeChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-props'!
*/
/*
udanax-top.st:38703:
CannotPartializeChange comment:
'The "Cannot Partialize" property is a Bert Canopy property to remember that a Stamp is actively being viewed (by a session level Orgl) and therefore cannot be poured-out (made more partial).  Should probably not be a Prop(erty), by rather a NOCOPY session level bit in the BertCrums.'!
*/
/*
udanax-top.st:38705:
(CannotPartializeChange getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CannotPartializeChange.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * compare the changed parts of two Props
 */
public boolean areEqualProps(Prop a, Prop b) {
	return ((BertProp) a).isNotPartializable() == ((BertProp) b).isNotPartializable();
/*
udanax-top.st:38710:CannotPartializeChange methodsFor: 'accessing'!
{BooleanVar} areEqualProps: a {Prop} with: b {Prop}
	"compare the changed parts of two Props"
	^(a cast: BertProp) isNotPartializable == (b cast: BertProp) isNotPartializable!
*/
}
public Prop changed(Prop old, Prop a) {
	BertProp bp;
	BertProp abp;
	bp = (BertProp) old;
	abp = (BertProp) a;
	if (bp.isNotPartializable() == abp.isNotPartializable()) {
		return old;
	}
	return BertProp.make(bp.permissions(), bp.endorsements(), bp.isSensorWaiting(), abp.isNotPartializable());
/*
udanax-top.st:38714:CannotPartializeChange methodsFor: 'accessing'!
{Prop} changed: old {Prop} with: a {Prop}
	| bp {BertProp wimpy} abp {BertProp wimpy} |
	bp _ old cast: BertProp.
	abp _ a cast: BertProp.
	bp isNotPartializable == abp isNotPartializable ifTrue: [^old].
	^BertProp make: bp permissions
		with: bp endorsements
		with: bp isSensorWaiting
		with: abp isNotPartializable!
*/
}
public PropFinder fetchFinder(Prop before, Prop after, BeRangeElement element, PropFinder oldFinder) {
	return null;
/*
udanax-top.st:38725:CannotPartializeChange methodsFor: 'accessing'!
{PropFinder | NULL} fetchFinder: before {Prop unused}
	with: after {Prop unused}
	with: element {BeRangeElement unused}
	with: oldFinder {PropFinder unused | NULL}
	^NULL!
*/
}
/**
 * whether this is a complete change of props
 */
public boolean isFull() {
	return false;
/*
udanax-top.st:38732:CannotPartializeChange methodsFor: 'accessing'!
{BooleanVar} isFull
	"whether this is a complete change of props"
	^false!
*/
}
public Prop with(Prop old, Prop a) {
	BertProp bp;
	BertProp abp;
	bp = (BertProp) old;
	abp = (BertProp) a;
	if (bp.isNotPartializable() || ( ! abp.isNotPartializable())) {
		return old;
	}
	else {
		return BertProp.make(bp.permissions(), bp.endorsements(), bp.isSensorWaiting(), abp.isNotPartializable());
	}
/*
udanax-top.st:38736:CannotPartializeChange methodsFor: 'accessing'!
{Prop} with: old {Prop} with: a {Prop}
	| bp {BertProp wimpy} abp {BertProp wimpy} |
	bp _ old cast: BertProp.
	abp _ a cast: BertProp.
	(bp isNotPartializable or: [abp isNotPartializable not])
		ifTrue: [^old]
		ifFalse: [^BertProp make: bp permissions
			with: bp endorsements
			with: bp isSensorWaiting
			with: abp isNotPartializable]!
*/
}
/**
 * compare the changed parts of two PropJoints
 */
public boolean areEqualPropJoints(PropJoint a, PropJoint b) {
	return ((BertPropJoint) a).isNotPartializable() == ((BertPropJoint) b).isNotPartializable();
/*
udanax-top.st:38750:CannotPartializeChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} areEqualPropJoints: a {PropJoint} with: b {PropJoint}
	"compare the changed parts of two PropJoints"
	^(a cast: BertPropJoint) isNotPartializable == (b cast: BertPropJoint) isNotPartializable!
*/
}
public PropJoint changedJoint(PropJoint old, PropJoint a) {
	BertPropJoint bpj;
	BertPropJoint one;
	bpj = (BertPropJoint) old;
	one = (BertPropJoint) a;
	if (bpj.isNotPartializable() == one.isNotPartializable()) {
		return old;
	}
	else {
		return BertPropJoint.make(bpj.permissionsJoint(), bpj.endorsementsJoint(), bpj.isSensorWaiting(), ! bpj.isNotPartializable());
	}
/*
udanax-top.st:38754:CannotPartializeChange methodsFor: 'smalltalk: suspended'!
{PropJoint} changedJoint: old {PropJoint} with: a {PropJoint}
	
	| bpj {BertPropJoint wimpy} one {BertPropJoint wimpy} |
	bpj _ old cast: BertPropJoint.
	one _ a cast: BertPropJoint.
	bpj isNotPartializable == one isNotPartializable
		ifTrue: [^old]
		ifFalse: [^BertPropJoint make: bpj permissionsJoint
				with: bpj endorsementsJoint
				with: bpj isSensorWaiting
				with: bpj isNotPartializable not]!
*/
}
/**
 * compare the changed parts of a PropJoint and a Prop
 */
public boolean isEqualToJointOf(PropJoint a, Prop b) {
	return ((BertPropJoint) a).isNotPartializable() == ((BertProp) b).isNotPartializable();
/*
udanax-top.st:38766:CannotPartializeChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} isEqualToJointOf: a {PropJoint} with: b {Prop}
	"compare the changed parts of a PropJoint and a Prop"
	^(a cast: BertPropJoint) isNotPartializable == (b cast: BertProp) isNotPartializable!
*/
}
/**
 * combine two PropJoints with minimum effort, given the previous result
 */
public PropJoint join(PropJoint old, PropJoint a, PropJoint b) {
	BertPropJoint bpj;
	BertPropJoint one;
	BertPropJoint two;
	bpj = (BertPropJoint) old;
	one = (BertPropJoint) a;
	two = (BertPropJoint) b;
	if (bpj.isNotPartializable() == (one.isNotPartializable() || (two.isNotPartializable()))) {
		return old;
	}
	else {
		return BertPropJoint.make(bpj.permissionsJoint(), bpj.endorsementsJoint(), bpj.isSensorWaiting(), ! bpj.isNotPartializable());
	}
/*
udanax-top.st:38770:CannotPartializeChange methodsFor: 'smalltalk: suspended'!
{PropJoint} join: old {PropJoint} with: a {PropJoint} with: b {PropJoint}
	"combine two PropJoints with minimum effort, given the previous result"
	| bpj {BertPropJoint wimpy} one {BertPropJoint wimpy} two {BertPropJoint wimpy} |
	bpj _ old cast: BertPropJoint.
	one _ a cast: BertPropJoint.
	two _ b cast: BertPropJoint.
	bpj isNotPartializable == (one isNotPartializable
			or: [two isNotPartializable])
		ifTrue: [^old]
		ifFalse: [^BertPropJoint make: bpj permissionsJoint
				with: bpj endorsementsJoint
				with: bpj isSensorWaiting
				with: bpj isNotPartializable not]!
*/
}
/**
 * combine two PropJoints and a prop with minimum effort, given the previous result
 */
public PropJoint joinProp(PropJoint old, PropJoint a, PropJoint b, Prop prop) {
	BertPropJoint bpj;
	BertPropJoint one;
	BertPropJoint two;
	BertProp p;
	bpj = (BertPropJoint) old;
	one = (BertPropJoint) a;
	two = (BertPropJoint) b;
	p = (BertProp) prop;
	if (bpj.isNotPartializable() == (one.isNotPartializable() || (two.isNotPartializable() || (p.isNotPartializable())))) {
		return old;
	}
	else {
		return BertPropJoint.make(bpj.permissionsJoint(), bpj.endorsementsJoint(), bpj.isSensorWaiting(), ! bpj.isNotPartializable());
	}
/*
udanax-top.st:38784:CannotPartializeChange methodsFor: 'smalltalk: suspended'!
{PropJoint} joinProp: old {PropJoint} with: a {PropJoint} with: b {PropJoint} with: prop {Prop}
	"combine two PropJoints and a prop with minimum effort, given the previous result"
	| bpj {BertPropJoint wimpy} one {BertPropJoint wimpy} two {BertPropJoint wimpy} p {BertProp wimpy} |
	bpj _ old cast: BertPropJoint.
	one _ a cast: BertPropJoint.
	two _ b cast: BertPropJoint.
	p _ prop cast: BertProp.
	bpj isNotPartializable == (one isNotPartializable
			or: [two isNotPartializable
			or: [p isNotPartializable]])
		ifTrue: [^old]
		ifFalse: [^BertPropJoint make: bpj permissionsJoint
				with: bpj endorsementsJoint
				with: bpj isSensorWaiting
				with: bpj isNotPartializable not]!
*/
}
public CannotPartializeChange(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:38802:CannotPartializeChange methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:38805:CannotPartializeChange methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public CannotPartializeChange() {
/*

Generated during transformation
*/
}
}
