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
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.FullPropChange;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Use this to indicate that all aspects of the Prop may have changed.
 */
public class FullPropChange extends PropChange {

/*
udanax-top.st:39040:
PropChange subclass: #FullPropChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-props'!
*/
/*
udanax-top.st:39044:
FullPropChange comment:
'Use this to indicate that all aspects of the Prop may have changed.'!
*/
/*
udanax-top.st:39046:
(FullPropChange getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FullPropChange.class).setAttributes( new Set().add("NOTATYPE").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * compare the changed parts of two Props
 */
public boolean areEqualProps(Prop a, Prop b) {
	return a.isEqual(b);
/*
udanax-top.st:39051:FullPropChange methodsFor: 'accessing'!
{BooleanVar} areEqualProps: a {Prop} with: b {Prop}
	"compare the changed parts of two Props"
	^a isEqual: b!
*/
}
public Prop changed(Prop old, Prop a) {
	return a;
/*
udanax-top.st:39055:FullPropChange methodsFor: 'accessing'!
{Prop} changed: old {Prop unused} with: a {Prop}
	^a!
*/
}
public PropFinder fetchFinder(Prop before, Prop after, BeRangeElement element, PropFinder oldFinder) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39059:FullPropChange methodsFor: 'accessing'!
{PropFinder | NULL} fetchFinder: before {Prop}
	with: after {Prop}
	with: element {BeRangeElement}
	with: oldFinder {PropFinder | NULL}
	
	self subclassResponsibility!
*/
}
/**
 * whether this is a complete change of props
 */
public boolean isFull() {
	return true;
/*
udanax-top.st:39066:FullPropChange methodsFor: 'accessing'!
{BooleanVar} isFull
	"whether this is a complete change of props"
	^true!
*/
}
public Prop with(Prop old, Prop a) {
	return old.with(a);
/*
udanax-top.st:39070:FullPropChange methodsFor: 'accessing'!
{Prop} with: old {Prop} with: a {Prop}
	^old with: a!
*/
}
/**
 * compare the changed parts of two PropJoints
 */
public boolean areEqualPropJoints(PropJoint a, PropJoint b) {
	return a.isEqual(b);
/*
udanax-top.st:39076:FullPropChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} areEqualPropJoints: a {PropJoint} with: b {PropJoint}
	"compare the changed parts of two PropJoints"
	^a isEqual: b!
*/
}
public PropJoint changedJoint(PropJoint old, PropJoint a) {
	return a;
/*
udanax-top.st:39080:FullPropChange methodsFor: 'smalltalk: suspended'!
{PropJoint} changedJoint: old {PropJoint unused} with: a {PropJoint}
	
	^a!
*/
}
/**
 * compare the changed parts of a PropJoint and a Prop
 */
public boolean isEqualToJointOf(PropJoint a, Prop b) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39084:FullPropChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} isEqualToJointOf: a {PropJoint} with: b {Prop}
	"compare the changed parts of a PropJoint and a Prop"
	self subclassResponsibility!
*/
}
/**
 * combine two PropJoints with minimum effort, given the previous result
 */
public PropJoint join(PropJoint old, PropJoint a, PropJoint b) {
	return a.join(b);
/*
udanax-top.st:39088:FullPropChange methodsFor: 'smalltalk: suspended'!
{PropJoint} join: old {PropJoint unused} with: a {PropJoint} with: b {PropJoint}
	"combine two PropJoints with minimum effort, given the previous result"
	^a join: b!
*/
}
/*
udanax-top.st:39092:FullPropChange methodsFor: 'smalltalk: suspended'!
{PropJoint} joinProp: old {PropJoint unused} with: a {PropJoint} with: b {PropJoint} with: prop {Prop}
	"combine two PropJoints and a prop with minimum effort, given the previous result"
	^(a join: b) with: prop!
*/
public FullPropChange() {
/*

Generated during transformation
*/
}
public FullPropChange(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
