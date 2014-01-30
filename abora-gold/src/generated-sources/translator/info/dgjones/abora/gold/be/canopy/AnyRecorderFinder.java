/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.canopy.AbstractRecorderFinder;
import info.dgjones.abora.gold.be.canopy.AnyRecorderFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * NOT.A.TYPE A general superclass for finders that looks for all recorders, and all elements
 * they might find, resulting from a given change.
 */
public class AnyRecorderFinder extends AbstractRecorderFinder {

/*
udanax-top.st:39949:
AbstractRecorderFinder subclass: #AnyRecorderFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39953:
AnyRecorderFinder comment:
'NOT.A.TYPE A general superclass for finders that looks for all recorders, and all elements they might find, resulting from a given change.'!
*/
/*
udanax-top.st:39955:
(AnyRecorderFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(AnyRecorderFinder.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public AnyRecorderFinder() {
	super();
	/* for generated code */
/*
udanax-top.st:39960:AnyRecorderFinder methodsFor: 'create'!
create
	super create "for generated code"!
*/
}
public AnyRecorderFinder(int flags) {
	super(flags);
/*
udanax-top.st:39963:AnyRecorderFinder methodsFor: 'create'!
create: flags {UInt32}
	super create: flags!
*/
}
/**
 * do nothing
 */
public void checkRecorder(ResultRecorder recorder, RecorderFossil fossil) {
/*
udanax-top.st:39969:AnyRecorderFinder methodsFor: 'recording'!
{void} checkRecorder: recorder {ResultRecorder}
	with: fossil {RecorderFossil}
	"do nothing"!
*/
}
public PropFinder findPast(BeEdition stamp) {
	return this;
/*
udanax-top.st:39976:AnyRecorderFinder methodsFor: 'accessing'!
{PropFinder} findPast: stamp {BeEdition unused}
	
	^self!
*/
}
public boolean match(Prop prop) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39980:AnyRecorderFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	self subclassResponsibility!
*/
}
/**
 * An additional finder to use below the given Edition
 */
public PropFinder nextFinder(BeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39984:AnyRecorderFinder methodsFor: 'accessing'!
{PropFinder} nextFinder: edition {BeEdition}
	"An additional finder to use below the given Edition"
	
	self subclassResponsibility!
*/
}
public PropFinder oldPass(PropJoint parent) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39991:AnyRecorderFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	self subclassResponsibility!
*/
}
public AnyRecorderFinder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
