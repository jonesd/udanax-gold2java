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
import info.dgjones.abora.gold.be.canopy.AnyRecorderEFinder;
import info.dgjones.abora.gold.be.canopy.CumulativeRecorderFinder;
import info.dgjones.abora.gold.be.canopy.OriginalResultRecorderEFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.filter.RegionDelta;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.BertPropJoint;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.EndorsementsChange;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Used when the Endorsement part of a BertProp changed
 */
public class EndorsementsChange extends PropChange {

/*
udanax-top.st:38917:
PropChange subclass: #EndorsementsChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-props'!
*/
/*
udanax-top.st:38921:
EndorsementsChange comment:
'Used when the Endorsement part of a BertProp changed'!
*/
/*
udanax-top.st:38923:
(EndorsementsChange getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EndorsementsChange.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * compare the changed parts of two Props
 */
public boolean areEqualProps(Prop a, Prop b) {
	return ((BertProp) a).endorsements().isEqual(((BertProp) b).endorsements());
/*
udanax-top.st:38928:EndorsementsChange methodsFor: 'accessing'!
{BooleanVar} areEqualProps: a {Prop} with: b {Prop}
	"compare the changed parts of two Props"
	^(a cast: BertProp) endorsements isEqual: (b cast: BertProp) endorsements!
*/
}
public Prop changed(Prop old, Prop a) {
	BertProp bp;
	bp = (BertProp) old;
	return BertProp.make(bp.permissions(), ((BertProp) a).endorsements(), bp.isSensorWaiting(), bp.isNotPartializable());
/*
udanax-top.st:38932:EndorsementsChange methodsFor: 'accessing'!
{Prop} changed: old {Prop} with: a {Prop}
	| bp {BertProp wimpy} |
	bp _ old cast: BertProp.
	^BertProp make: bp permissions
		with: (a cast: BertProp) endorsements
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
			delta = RegionDelta.make(b.endorsements(), a.endorsements());
			if (delta.isSame()) {
				return null;
			}
			any = AnyRecorderEFinder.make(((IDRegion) a.permissions()), delta);
			if (any.isEmpty()) {
				anys = ImmuSet.make();
			}
			else {
				anys = ImmuSet.newWith(any);
			}
			simple = OriginalResultRecorderEFinder.make(element, ((IDRegion) a.permissions()), delta);
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
udanax-top.st:38941:EndorsementsChange methodsFor: 'accessing'!
{PropFinder | NULL} fetchFinder: before {Prop}
	with: after {Prop}
	with: element {BeRangeElement}
	with: oldFinder {PropFinder | NULL}
	
	before cast: BertProp into: [ :b |
	after cast: BertProp into: [ :a |
		| result {PropFinder} delta {RegionDelta}
		  any {PropFinder} anys {ImmuSet}
		  simple {PropFinder} simples {ImmuSet} |
		delta := RegionDelta make: b endorsements with: a endorsements.
		delta isSame ifTrue:
			[^NULL].
		any := AnyRecorderEFinder make: (a permissions cast: IDRegion) with: delta.
		any isEmpty
			ifTrue: [anys := ImmuSet make]
			ifFalse: [anys := ImmuSet newWith: any].
		simple := OriginalResultRecorderEFinder make: element
			with: (a permissions cast: IDRegion)
			with: delta.
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
udanax-top.st:38980:EndorsementsChange methodsFor: 'accessing'!
{BooleanVar} isFull
	"whether this is a complete change of props"
	^false!
*/
}
public Prop with(Prop old, Prop a) {
	BertProp bp;
	bp = (BertProp) old;
	return BertProp.make(bp.permissions(), (((BertProp) a).endorsements().unionWith(bp.endorsements())), bp.isSensorWaiting(), bp.isNotPartializable());
/*
udanax-top.st:38984:EndorsementsChange methodsFor: 'accessing'!
{Prop} with: old {Prop} with: a {Prop}
	| bp {BertProp wimpy} |
	bp _ old cast: BertProp.
	^BertProp make: bp permissions
		with: ((a cast: BertProp) endorsements unionWith: bp endorsements)
		with: bp isSensorWaiting
		with: bp isNotPartializable!
*/
}
/**
 * compare the changed parts of two PropJoints
 */
public boolean areEqualPropJoints(PropJoint a, PropJoint b) {
	return ((BertPropJoint) a).endorsementsJoint().isEqual(((BertPropJoint) b).endorsementsJoint());
/*
udanax-top.st:38995:EndorsementsChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} areEqualPropJoints: a {PropJoint} with: b {PropJoint}
	"compare the changed parts of two PropJoints"
	^(a cast: BertPropJoint) endorsementsJoint isEqual: (b cast: BertPropJoint) endorsementsJoint!
*/
}
public PropJoint changedJoint(PropJoint old, PropJoint a) {
	BertPropJoint bertprop;
	bertprop = (BertPropJoint) old;
	return BertPropJoint.make(bertprop.permissionsJoint(), ((BertPropJoint) a).endorsementsJoint(), bertprop.isSensorWaiting(), bertprop.isNotPartializable());
/*
udanax-top.st:38999:EndorsementsChange methodsFor: 'smalltalk: suspended'!
{PropJoint} changedJoint: old {PropJoint} with: a {PropJoint}
	
	| bertprop {BertPropJoint wimpy} |
	bertprop _ old cast: BertPropJoint.
	^BertPropJoint make: bertprop  permissionsJoint
		with: (a cast: BertPropJoint) endorsementsJoint
		with: bertprop isSensorWaiting
		with: bertprop isNotPartializable!
*/
}
/**
 * compare the changed parts of a PropJoint and a Prop
 */
public boolean isEqualToJointOf(PropJoint a, Prop b) {
	return ((BertPropJoint) a).endorsementsJoint().unioned().isEqual(((BertProp) b).endorsements());
/*
udanax-top.st:39008:EndorsementsChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} isEqualToJointOf: a {PropJoint} with: b {Prop}
	"compare the changed parts of a PropJoint and a Prop"
	^(a cast: BertPropJoint) endorsementsJoint unioned isEqual: (b cast: BertProp) endorsements!
*/
}
/**
 * combine two PropJoints with minimum effort, given the previous result
 */
public PropJoint join(PropJoint old, PropJoint a, PropJoint b) {
	BertPropJoint bertprop;
	bertprop = (BertPropJoint) old;
	return BertPropJoint.make(bertprop.permissionsJoint(), (((BertPropJoint) a).endorsementsJoint().join(((BertPropJoint) b).endorsementsJoint())), bertprop.isSensorWaiting(), bertprop.isNotPartializable());
/*
udanax-top.st:39012:EndorsementsChange methodsFor: 'smalltalk: suspended'!
{PropJoint} join: old {PropJoint} with: a {PropJoint} with: b {PropJoint}
	"combine two PropJoints with minimum effort, given the previous result"
	| bertprop {BertPropJoint wimpy} |
	bertprop _ old cast: BertPropJoint.
	^BertPropJoint make: bertprop  permissionsJoint
		with: ((a cast: BertPropJoint) endorsementsJoint join: (b cast: BertPropJoint) endorsementsJoint)
		with: bertprop isSensorWaiting
		with: bertprop isNotPartializable!
*/
}
/**
 * combine two PropJoints and a prop with minimum effort, given the previous result
 */
public PropJoint joinProp(PropJoint old, PropJoint a, PropJoint b, Prop prop) {
	BertPropJoint bpj;
	bpj = (BertPropJoint) old;
	return BertPropJoint.make(bpj.permissionsJoint(), ((((BertPropJoint) a).endorsementsJoint().join(((BertPropJoint) b).endorsementsJoint())).with(((BertProp) prop).endorsements())), bpj.isSensorWaiting(), bpj.isNotPartializable());
/*
udanax-top.st:39021:EndorsementsChange methodsFor: 'smalltalk: suspended'!
{PropJoint} joinProp: old {PropJoint} with: a {PropJoint} with: b {PropJoint} with: prop {Prop}
	"combine two PropJoints and a prop with minimum effort, given the previous result"
	| bpj {BertPropJoint wimpy} |
	bpj _ old cast: BertPropJoint.
	^BertPropJoint make: bpj  permissionsJoint
		with: (((a cast: BertPropJoint) endorsementsJoint
			join: (b cast: BertPropJoint) endorsementsJoint)
			with: (prop cast: BertProp) endorsements)
		with: bpj isSensorWaiting
		with: bpj isNotPartializable!
*/
}
public EndorsementsChange(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:39034:EndorsementsChange methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:39037:EndorsementsChange methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public EndorsementsChange() {
/*

Generated during transformation
*/
}
}
