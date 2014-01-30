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
import info.dgjones.abora.gold.be.canopy.CumulativeRecorderFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.BertPropJoint;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.BertPropChange;
import info.dgjones.abora.gold.props.FullPropChange;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Use when it is fine to consider that all aspects of the BertProp may have changed
 */
public class BertPropChange extends FullPropChange {

/*
udanax-top.st:39096:
FullPropChange subclass: #BertPropChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-props'!
*/
/*
udanax-top.st:39100:
BertPropChange comment:
'Use when it is fine to consider that all aspects of the BertProp may have changed'!
*/
/*
udanax-top.st:39102:
(BertPropChange getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BertPropChange.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public PropFinder fetchFinder(Prop before, Prop after, BeRangeElement element, PropFinder oldFinder) {
	PropFinder p;
	PropFinder e;
	p = PropChange.permissionsChange().fetchFinder(before, after, element, oldFinder);
	e = PropChange.endorsementsChange().fetchFinder(before, after, element, oldFinder);
	if (p == null) {
		return e;
	}
	if (e == null) {
		return p;
	}
	if (p instanceof CumulativeRecorderFinder) {
		CumulativeRecorderFinder pcrf = (CumulativeRecorderFinder) p;
		if (e instanceof CumulativeRecorderFinder) {
			CumulativeRecorderFinder ecrf = (CumulativeRecorderFinder) e;
			return CumulativeRecorderFinder.make((pcrf.generators().unionWith(ecrf.generators())), (pcrf.current().unionWith(ecrf.current())), (pcrf.others().unionWith(ecrf.others())));
		}
	}
	return null;
/*
udanax-top.st:39107:BertPropChange methodsFor: 'accessing'!
{PropFinder | NULL} fetchFinder: before {Prop}
	with: after {Prop}
	with: element {BeRangeElement}
	with: oldFinder {PropFinder | NULL}
	
	| p {PropFinder} e {PropFinder} |
	p := PropChange permissionsChange
			fetchFinder: before with: after with: element with: oldFinder.
	e := PropChange endorsementsChange
			fetchFinder: before with: after with: element with: oldFinder.
	p == NULL ifTrue: [^e].
	e == NULL ifTrue: [^p].
	p cast: CumulativeRecorderFinder into: [ :pcrf |
	e cast: CumulativeRecorderFinder into: [ :ecrf |
		^CumulativeRecorderFinder
			make: (pcrf generators unionWith: ecrf generators)
			with: (pcrf current unionWith: ecrf current)
			with: (pcrf others unionWith: ecrf others)]].
	^NULL "fodder"!
*/
}
/**
 * compare the changed parts of a PropJoint and a Prop
 */
public boolean isEqualToJointOf(PropJoint a, Prop b) {
	BertPropJoint bpj;
	BertProp bp;
	bpj = (BertPropJoint) a;
	bp = (BertProp) b;
	return (bpj.endorsementsJoint().unioned().isEqual(bp.endorsements())) && ((bpj.permissionsJoint().unioned().isEqual(bp.permissions())) && (bpj.isSensorWaiting() == bp.isSensorWaiting() && (bpj.isNotPartializable() == bp.isNotPartializable())));
/*
udanax-top.st:39129:BertPropChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} isEqualToJointOf: a {PropJoint} with: b {Prop}
	"compare the changed parts of a PropJoint and a Prop"
	| bpj {BertPropJoint wimpy} bp {BertProp wimpy} |
	bpj _ a cast: BertPropJoint.
	bp _ b cast: BertProp.
	^(bpj endorsementsJoint unioned isEqual: bp endorsements)
		and: [(bpj permissionsJoint unioned isEqual: bp permissions)
		and: [bpj isSensorWaiting == bp isSensorWaiting
		and: [bpj isNotPartializable == bp isNotPartializable]]]!
*/
}
public BertPropChange(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:39141:BertPropChange methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:39144:BertPropChange methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public BertPropChange() {
/*

Generated during transformation
*/
}
}
