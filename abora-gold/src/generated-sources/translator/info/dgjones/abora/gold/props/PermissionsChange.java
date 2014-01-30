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
import info.dgjones.abora.gold.be.canopy.AnyRecorderPFinder;
import info.dgjones.abora.gold.be.canopy.CumulativeRecorderFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.ResultRecorderPFinder;
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.filter.RegionDelta;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.BertPropJoint;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.PermissionsChange;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Used when the Permissions part of a BertProp changed
 */
public class PermissionsChange extends PropChange {

/*
udanax-top.st:39184:
PropChange subclass: #PermissionsChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-props'!
*/
/*
udanax-top.st:39188:
PermissionsChange comment:
'Used when the Permissions part of a BertProp changed'!
*/
/*
udanax-top.st:39190:
(PermissionsChange getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PermissionsChange.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * compare the changed parts of two Props
 */
public boolean areEqualProps(Prop a, Prop b) {
	return ((BertProp) a).permissions().isEqual(((BertProp) b).permissions());
/*
udanax-top.st:39195:PermissionsChange methodsFor: 'accessing'!
{BooleanVar} areEqualProps: a {Prop} with: b {Prop}
	"compare the changed parts of two Props"
	^(a cast: BertProp) permissions isEqual: (b cast: BertProp) permissions!
*/
}
public Prop changed(Prop old, Prop a) {
	BertProp bp;
	bp = (BertProp) old;
	return BertProp.make(((BertProp) a).permissions(), bp.endorsements(), bp.isSensorWaiting(), bp.isNotPartializable());
/*
udanax-top.st:39199:PermissionsChange methodsFor: 'accessing'!
{Prop} changed: old {Prop} with: a {Prop}
	| bp {BertProp wimpy} |
	bp _ old cast: BertProp.
	^BertProp make: (a cast: BertProp) permissions
		with: bp endorsements
		with: bp isSensorWaiting
		with: bp isNotPartializable!
*/
}
public PropFinder fetchFinder(Prop before, Prop after, BeRangeElement element, PropFinder oldFinder) {
	if (before instanceof BertProp) {
		BertProp b = (BertProp) before;
		if (after instanceof BertProp) {
			BertProp a = (BertProp) after;
			/* TODO variable may not be initialized before being used */
			PropFinder result = null;
			RegionDelta delta;
			PropFinder any;
			ImmuSet anys;
			PropFinder simple;
			ImmuSet simples;
			delta = RegionDelta.make(b.permissions(), a.permissions());
			if (delta.isSame()) {
				return null;
			}
			any = AnyRecorderPFinder.make(delta);
			if (any.isEmpty()) {
				anys = ImmuSet.make();
			}
			else {
				anys = ImmuSet.newWith(any);
			}
			simple = ResultRecorderPFinder.make(element, delta, a.endorsements());
			if (simple.isEmpty()) {
				simples = ImmuSet.make();
			}
			else {
				simples = ImmuSet.newWith(simple);
			}
			if (oldFinder == null) {
				result = CumulativeRecorderFinder.make(anys, simples, ImmuSet.make());
			}
			else {
				if (oldFinder instanceof CumulativeRecorderFinder) {
					CumulativeRecorderFinder crf = (CumulativeRecorderFinder) oldFinder;
					result = CumulativeRecorderFinder.make(anys, simples, (crf.current().unionWith(crf.others())));
				}
			}
			if (result.isEmpty()) {
				return null;
			}
			else {
				return result;
			}
		}
	}
	return null;
/*
udanax-top.st:39208:PermissionsChange methodsFor: 'accessing'!
{PropFinder | NULL} fetchFinder: before {Prop}
	with: after {Prop}
	with: element {BeRangeElement}
	with: oldFinder {PropFinder | NULL}
	
	before cast: BertProp into: [ :b |
	after cast: BertProp into: [ :a |
		| result {PropFinder} delta {RegionDelta}
		  any {PropFinder} anys {ImmuSet}
		  simple {PropFinder} simples {ImmuSet} |
		delta := RegionDelta make: b permissions with: a permissions.
		delta isSame ifTrue:
			[^NULL].
		any := AnyRecorderPFinder make: delta.
		any isEmpty
			ifTrue: [anys := ImmuSet make]
			ifFalse: [anys := ImmuSet newWith: any].
		simple :=ResultRecorderPFinder make: element
			with: delta
			with: a endorsements.
		simple isEmpty
			ifTrue: [simples := ImmuSet make]
			ifFalse: [simples := ImmuSet newWith: simple].
		oldFinder == NULL ifTrue:
			[result := CumulativeRecorderFinder
				make: anys
				with: simples
				with: ImmuSet make]
		ifFalse:
			[oldFinder cast: CumulativeRecorderFinder into: [ :crf |
				result := CumulativeRecorderFinder
					make: anys
					with: simples
					with: (crf current unionWith: crf others)]].
		result isEmpty
			ifTrue: [^NULL]
			ifFalse: [^result]]].
	^NULL "fodder"!
*/
}
/**
 * whether this is a complete change of props
 */
public boolean isFull() {
	return false;
/*
udanax-top.st:39247:PermissionsChange methodsFor: 'accessing'!
{BooleanVar} isFull
	"whether this is a complete change of props"
	^false!
*/
}
public Prop with(Prop old, Prop a) {
	BertProp bp;
	bp = (BertProp) old;
	return BertProp.make((((BertProp) a).permissions().unionWith(bp.permissions())), bp.endorsements(), bp.isSensorWaiting(), bp.isNotPartializable());
/*
udanax-top.st:39251:PermissionsChange methodsFor: 'accessing'!
{Prop} with: old {Prop} with: a {Prop}
	| bp {BertProp wimpy} |
	bp _ old cast: BertProp.
	^BertProp make: ((a cast: BertProp) permissions unionWith: bp permissions)
		with: bp endorsements
		with: bp isSensorWaiting
		with: bp isNotPartializable!
*/
}
/**
 * compare the changed parts of two PropJoints
 */
public boolean areEqualPropJoints(PropJoint a, PropJoint b) {
	return ((BertPropJoint) a).permissionsJoint().isEqual(((BertPropJoint) b).permissionsJoint());
/*
udanax-top.st:39262:PermissionsChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} areEqualPropJoints: a {PropJoint} with: b {PropJoint}
	"compare the changed parts of two PropJoints"
	^(a cast: BertPropJoint) permissionsJoint isEqual: (b cast: BertPropJoint) permissionsJoint!
*/
}
public PropJoint changedJoint(PropJoint old, PropJoint a) {
	BertPropJoint bp;
	bp = (BertPropJoint) old;
	return BertPropJoint.make(((BertPropJoint) a).permissionsJoint(), bp.endorsementsJoint(), bp.isSensorWaiting(), bp.isNotPartializable());
/*
udanax-top.st:39266:PermissionsChange methodsFor: 'smalltalk: suspended'!
{PropJoint} changedJoint: old {PropJoint} with: a {PropJoint}
	
	| bp {BertPropJoint wimpy} |
	bp _ old cast: BertPropJoint.
	^BertPropJoint make: (a cast: BertPropJoint) permissionsJoint
		with: bp endorsementsJoint
		with: bp isSensorWaiting
		with: bp isNotPartializable!
*/
}
/**
 * compare the changed parts of a PropJoint and a Prop
 */
public boolean isEqualToJointOf(PropJoint a, Prop b) {
	return ((BertPropJoint) a).permissionsJoint().unioned().isEqual(((BertProp) b).permissions());
/*
udanax-top.st:39275:PermissionsChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} isEqualToJointOf: a {PropJoint} with: b {Prop}
	"compare the changed parts of a PropJoint and a Prop"
	^(a cast: BertPropJoint) permissionsJoint unioned isEqual: (b cast: BertProp) permissions!
*/
}
/**
 * combine two PropJoints with minimum effort, given the previous result
 */
public PropJoint join(PropJoint old, PropJoint a, PropJoint b) {
	BertPropJoint bp;
	bp = (BertPropJoint) old;
	return BertPropJoint.make((((BertPropJoint) a).permissionsJoint().join(((BertPropJoint) b).permissionsJoint())), bp.endorsementsJoint(), bp.isSensorWaiting(), bp.isNotPartializable());
/*
udanax-top.st:39279:PermissionsChange methodsFor: 'smalltalk: suspended'!
{PropJoint} join: old {PropJoint} with: a {PropJoint} with: b {PropJoint}
	"combine two PropJoints with minimum effort, given the previous result"
	| bp {BertPropJoint wimpy} |
	bp _ old cast: BertPropJoint.
	^BertPropJoint make: ((a cast: BertPropJoint) permissionsJoint join: (b cast: BertPropJoint) permissionsJoint)
		with: bp endorsementsJoint
		with: bp isSensorWaiting
		with: bp isNotPartializable!
*/
}
/**
 * combine two PropJoints and a prop with minimum effort, given the previous result
 */
public PropJoint joinProp(PropJoint old, PropJoint a, PropJoint b, Prop prop) {
	BertPropJoint bpj;
	bpj = (BertPropJoint) old;
	return BertPropJoint.make(((((BertPropJoint) a).permissionsJoint().join(((BertPropJoint) b).permissionsJoint())).with(((BertProp) prop).permissions())), bpj.endorsementsJoint(), bpj.isSensorWaiting(), bpj.isNotPartializable());
/*
udanax-top.st:39288:PermissionsChange methodsFor: 'smalltalk: suspended'!
{PropJoint} joinProp: old {PropJoint} with: a {PropJoint} with: b {PropJoint} with: prop {Prop}
	"combine two PropJoints and a prop with minimum effort, given the previous result"
	| bpj {BertPropJoint wimpy} |
	bpj _ old cast: BertPropJoint.
	^BertPropJoint make: (((a cast: BertPropJoint) permissionsJoint
			join: (b cast: BertPropJoint) permissionsJoint)
			with: (prop cast: BertProp) permissions)
		with: bpj endorsementsJoint
		with: bpj isSensorWaiting
		with: bpj isNotPartializable!
*/
}
public PermissionsChange(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:39301:PermissionsChange methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:39304:PermissionsChange methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public PermissionsChange() {
/*

Generated during transformation
*/
}
}
