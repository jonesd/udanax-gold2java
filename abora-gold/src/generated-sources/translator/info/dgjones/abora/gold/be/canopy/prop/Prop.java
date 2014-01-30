/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy.prop;

import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A collection of properties which are to be found by navigating a Canopy.  PropJoints are
 * the union/intersection style abstraction of the properties which provide for such
 * navigation.
 */
public class Prop extends Heaper {

/*
udanax-top.st:38185:
Heaper subclass: #Prop
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy-Prop'!
*/
/*
udanax-top.st:38189:
Prop comment:
'A collection of properties which are to be found by navigating a Canopy.  PropJoints are the union/intersection style abstraction of the properties which provide for such navigation.'!
*/
/*
udanax-top.st:38191:
(Prop getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Prop.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The flags used in the Canopy to tag this prop
 */
public int flags() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38196:Prop methodsFor: 'accessing'!
{UInt32} flags
	"The flags used in the Canopy to tag this prop"
	
	self subclassResponsibility!
*/
}
public Prop with(Prop other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38201:Prop methodsFor: 'accessing'!
{Prop} with: other {Prop}
	self subclassResponsibility!
*/
}
/**
 * Returns the filtering information from this one prop as a PropJoint.
 */
public PropJoint joint() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:38206:Prop methodsFor: 'smalltalk: suspended'!
{PropJoint} joint
	"Returns the filtering information from this one prop as a PropJoint. "
	
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:38214:Prop methodsFor: 'tesing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public Prop() {
/*

Generated during transformation
*/
}
public Prop(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
