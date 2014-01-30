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
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.BertPropChange;
import info.dgjones.abora.gold.props.CannotPartializeChange;
import info.dgjones.abora.gold.props.DetectorWaitingChange;
import info.dgjones.abora.gold.props.EndorsementsChange;
import info.dgjones.abora.gold.props.PermissionsChange;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.props.SensorPropChange;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Each concrete class has just one canonical instance and no state.  A PropChange is used to
 * represent which property aspect changed (such as permission vs endorsement vs both).
 */
public class PropChange extends Heaper {

	protected static PropChange TheBertPropChange;
	protected static PropChange TheCannotPartializeChange;
	protected static PropChange TheDetectorWaitingChange;
	protected static PropChange TheEndorsementsChange;
	protected static PropChange ThePermissionsChange;
	protected static PropChange TheSensorPropChange;
/*
udanax-top.st:38547:
Heaper subclass: #PropChange
	instanceVariableNames: ''
	classVariableNames: '
		TheBertPropChange {PropChange} 
		TheCannotPartializeChange {PropChange} 
		TheDetectorWaitingChange {PropChange} 
		TheEndorsementsChange {PropChange} 
		ThePermissionsChange {PropChange} 
		TheSensorPropChange {PropChange} '
	poolDictionaries: ''
	category: 'Xanadu-props'!
*/
/*
udanax-top.st:38557:
PropChange comment:
'Each concrete class has just one canonical instance and no state.  A PropChange is used to represent which property aspect changed (such as permission vs endorsement vs both).'!
*/
/*
udanax-top.st:38559:
(PropChange getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:38653:
PropChange class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:38656:
(PropChange getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PropChange.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * compare the changed parts of two Props
 */
public boolean areEqualProps(Prop a, Prop b) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38564:PropChange methodsFor: 'accessing'!
{BooleanVar} areEqualProps: a {Prop} with: b {Prop}
	"compare the changed parts of two Props"
	self subclassResponsibility!
*/
}
/**
 * Return a Prop which is the same as 'old' for aspects which I don't represent as changing,
 * and 'a' for aspects that I do represent as changing.
 * This is used to replace Props with minimum effort, given that the 'a' parameter has only
 * new props which are of the aspect this change replaces, while the 'old' parameter starts
 * as the original set of Props, perhaps including other aspects.
 * See also: with:with:, which unions rather than replacing.
 */
public Prop changed(Prop old, Prop a) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38568:PropChange methodsFor: 'accessing'!
{Prop} changed: old {Prop} with: a {Prop}
	"Return a Prop which is the same as 'old' for aspects which I don't represent as changing, and 'a' for aspects that I do represent as changing.
	
	 This is used to replace Props with minimum effort, given that the 'a' parameter has only new props which are of the aspect this change replaces, while the 'old' parameter starts as the original set of Props, perhaps including other aspects.
	 
	 See also: with:with:, which unions rather than replacing."
	
	self subclassResponsibility!
*/
}
/**
 * return a finder looking for this change from before to after, in addition to whatever
 * oldFinder is looking for (assumes this changes is a subset of oldFinder's change)
 */
public PropFinder fetchFinder(Prop before, Prop after, BeRangeElement element, PropFinder oldFinder) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38577:PropChange methodsFor: 'accessing'!
{PropFinder | NULL} fetchFinder: before {Prop}
	with: after {Prop}
	with: element {BeRangeElement}
	with: oldFinder {PropFinder | NULL}
	"return a finder looking for this change from before to after, in addition to whatever oldFinder is looking for (assumes this changes is a subset of oldFinder's change)"
	
	self subclassResponsibility!
*/
}
/**
 * whether this is a complete change of props
 */
public boolean isFull() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38585:PropChange methodsFor: 'accessing'!
{BooleanVar} isFull
	"whether this is a complete change of props"
	self subclassResponsibility!
*/
}
/**
 * Return a Prop which is the same as 'old' for aspects which I don't represent as changing,
 * and the union of 'old' and 'a' for aspects that I do represent as changing.
 * This is used to accumulate changes to Props with minimum effort, given that the 'a'
 * parameter has only new props which are of the aspect this change changes, while the 'old'
 * parameter starts as the original set of Props, perhaps including other aspects.
 * See also changed:with:, which replaces rather than unioning.
 */
public Prop with(Prop old, Prop a) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38589:PropChange methodsFor: 'accessing'!
{Prop} with: old {Prop} with: a {Prop}
	"Return a Prop which is the same as 'old' for aspects which I don't represent as changing, and the union of 'old' and 'a' for aspects that I do represent as changing.
	
	 This is used to accumulate changes to Props with minimum effort, given that the 'a' parameter has only new props which are of the aspect this change changes, while the 'old' parameter starts as the original set of Props, perhaps including other aspects.
	 
	 See also changed:with:, which replaces rather than unioning."
	
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return takeOop();
/*
udanax-top.st:38600:PropChange methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self takeOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:38603:PropChange methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^self == other!
*/
}
/**
 * @deprecated
 */
public PropFinder fetchFinder(Prop before, Prop after) {
	throw new PasseException();
/*
udanax-top.st:38608:PropChange methodsFor: 'smalltalk: passe'!
{PropFinder} fetchFinder: before {Prop} with: after {Prop}
	self passe "extra args"!
*/
}
/**
 * @deprecated
 */
public PropFinder finderPartFrom(PropFinder finder) {
	throw new PasseException();
/*
udanax-top.st:38612:PropChange methodsFor: 'smalltalk: passe'!
{PropFinder} finderPartFrom: finder {PropFinder}
	self passe!
*/
}
public PropFinder fetchFinder(Prop before, Prop after, BeRangeElement element) {
	return fetchFinder(before, after, element, null);
/*
udanax-top.st:38618:PropChange methodsFor: 'smalltalk: defaults'!
{PropFinder | NULL} fetchFinder: before {Prop}
	with: after {Prop}
	with: element {BeRangeElement}
	
	^self fetchFinder: before with: after with: element with: NULL!
*/
}
/**
 * compare the changed parts of two PropJoints
 */
public boolean areEqualPropJoints(PropJoint a, PropJoint b) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38626:PropChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} areEqualPropJoints: a {PropJoint} with: b {PropJoint}
	"compare the changed parts of two PropJoints"
	
	self subclassResponsibility!
*/
}
/**
 * Return a PropJoint which is the same as 'old' for aspects which I don't represent as
 * changing, and 'a' for aspects that I do represent as changing.
 * This is used to replace PropJoints with minimum effort, given that the 'a' parameter has
 * only new PropJoints which are of the aspect this change replaces, while the 'old'
 * parameter starts as the original set of PropJoints, perhaps including other aspects.
 * See also: change:with:, which does this for Props rather than PropJoints.
 */
public PropJoint changedJoint(PropJoint old, PropJoint a) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38631:PropChange methodsFor: 'smalltalk: suspended'!
{PropJoint} changedJoint: old {PropJoint} with: a {PropJoint}
	"Return a PropJoint which is the same as 'old' for aspects which I don't represent as changing, and 'a' for aspects that I do represent as changing.
	
	 This is used to replace PropJoints with minimum effort, given that the 'a' parameter has only new PropJoints which are of the aspect this change replaces, while the 'old' parameter starts as the original set of PropJoints, perhaps including other aspects.
	 
	 See also: change:with:, which does this for Props rather than PropJoints."
	
	self subclassResponsibility!
*/
}
/**
 * compare the changed parts of a PropJoint and a Prop
 */
public boolean isEqualToJointOf(PropJoint a, Prop b) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38640:PropChange methodsFor: 'smalltalk: suspended'!
{BooleanVar} isEqualToJointOf: a {PropJoint} with: b {Prop}
	"compare the changed parts of a PropJoint and a Prop"
	self subclassResponsibility!
*/
}
/**
 * combine two PropJoints with minimum effort, given the previous result
 */
public PropJoint join(PropJoint old, PropJoint a, PropJoint b) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38644:PropChange methodsFor: 'smalltalk: suspended'!
{PropJoint} join: old {PropJoint} with: a {PropJoint} with: b {PropJoint}
	"combine two PropJoints with minimum effort, given the previous result"
	self subclassResponsibility!
*/
}
/**
 * combine two PropJoints and a prop with minimum effort, given the previous result
 */
public PropJoint joinProp(PropJoint old, PropJoint a, PropJoint b, Prop prop) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38648:PropChange methodsFor: 'smalltalk: suspended'!
{PropJoint} joinProp: old {PropJoint} with: a {PropJoint} with: b {PropJoint} with: prop {Prop}
	"combine two PropJoints and a prop with minimum effort, given the previous result"
	self subclassResponsibility!
*/
}
public static PropChange bertPropChange() {
	return TheBertPropChange;
/*
udanax-top.st:38661:PropChange class methodsFor: 'pseudo constructors'!
{PropChange} bertPropChange
	^TheBertPropChange!
*/
}
public static PropChange cannotPartializeChange() {
	return TheCannotPartializeChange;
/*
udanax-top.st:38664:PropChange class methodsFor: 'pseudo constructors'!
{PropChange} cannotPartializeChange
	^TheCannotPartializeChange!
*/
}
public static PropChange detectorWaitingChange() {
	return TheDetectorWaitingChange;
/*
udanax-top.st:38667:PropChange class methodsFor: 'pseudo constructors'!
{PropChange} detectorWaitingChange
	^TheDetectorWaitingChange!
*/
}
public static PropChange endorsementsChange() {
	return TheEndorsementsChange;
/*
udanax-top.st:38670:PropChange class methodsFor: 'pseudo constructors'!
{PropChange} endorsementsChange
	^TheEndorsementsChange!
*/
}
public static PropChange permissionsChange() {
	return ThePermissionsChange;
/*
udanax-top.st:38673:PropChange class methodsFor: 'pseudo constructors'!
{PropChange} permissionsChange
	^ThePermissionsChange!
*/
}
/**
 * Returns the canonical PropChange object for propagating the properties that result from
 * installing a recorder (permissions and endorsement filters).  A better name would be
 * recorderPropChange
 */
public static PropChange sensorPropChange() {
	return TheSensorPropChange;
/*
udanax-top.st:38676:PropChange class methodsFor: 'pseudo constructors'!
{PropChange} sensorPropChange
	"Returns the canonical PropChange object for propagating the properties that result from installing a recorder (permissions and endorsement filters).  A better name would be recorderPropChange"
	
	^TheSensorPropChange!
*/
}
public static void initTimeNonInherited() {
	TheCannotPartializeChange = new CannotPartializeChange();
	TheDetectorWaitingChange = new DetectorWaitingChange();
	TheEndorsementsChange = new EndorsementsChange();
	TheBertPropChange = new BertPropChange();
	TheSensorPropChange = new SensorPropChange();
	ThePermissionsChange = new PermissionsChange();
/*
udanax-top.st:38683:PropChange class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	TheCannotPartializeChange _ CannotPartializeChange create.
	TheDetectorWaitingChange _ DetectorWaitingChange create.
	TheEndorsementsChange _ EndorsementsChange create.
	TheBertPropChange _ BertPropChange create.
	TheSensorPropChange _ SensorPropChange create.
	ThePermissionsChange _ PermissionsChange create.!
*/
}
public static void linkTimeNonInherited() {
	TheCannotPartializeChange = null;
	TheDetectorWaitingChange = null;
	TheEndorsementsChange = null;
	TheBertPropChange = null;
	TheSensorPropChange = null;
	ThePermissionsChange = null;
/*
udanax-top.st:38691:PropChange class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	TheCannotPartializeChange _ NULL.
	TheDetectorWaitingChange _ NULL.
	TheEndorsementsChange _ NULL.
	TheBertPropChange _ NULL.
	TheSensorPropChange _ NULL.
	ThePermissionsChange _ NULL!
*/
}
public PropChange() {
/*

Generated during transformation
*/
}
public PropChange(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
