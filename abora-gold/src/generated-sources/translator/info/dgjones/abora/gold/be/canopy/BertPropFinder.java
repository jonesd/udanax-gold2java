/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.canopy.BertPropFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Used to filter by the bert canopy
 */
public class BertPropFinder extends PropFinder {

/*
udanax-top.st:39444:
PropFinder subclass: #BertPropFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39448:
BertPropFinder comment:
'Used to filter by the bert canopy'!
*/
/*
udanax-top.st:39450:
(BertPropFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BertPropFinder.class).setAttributes( new Set().add("NOTATYPE").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public BertPropFinder() {
	super();
	/* for comm */
/*
udanax-top.st:39455:BertPropFinder methodsFor: 'create'!
create
	super create "for comm"!
*/
}
public BertPropFinder(int flags) {
	super(flags);
/*
udanax-top.st:39458:BertPropFinder methodsFor: 'create'!
create: flags {UInt32}
	super create: flags!
*/
}
public PropFinder findPast(BeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39464:BertPropFinder methodsFor: 'accessing'!
{PropFinder} findPast: edition {BeEdition}
	
	self subclassResponsibility!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39468:BertPropFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	"tell whether a prop matches this filter"
	self subclassResponsibility!
*/
}
/**
 * return a simple enough finder for looking at the children
 */
public PropFinder oldPass(PropJoint parent) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39474:BertPropFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	"return a simple enough finder for looking at the children"
	self subclassResponsibility!
*/
}
public BertPropFinder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
