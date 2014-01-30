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
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.BertPropFinder;
import info.dgjones.abora.gold.be.canopy.CannotPartializeFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.BertPropJoint;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Used to figure out which Stamps have Orgls on them so that the archiver can knw that they
 * cannot be partialized.  Will go away because the state described is session level state
 * and therefore should be store in NOCOPY variables instead of in the Canopy''s Props.
 */
public class CannotPartializeFinder extends BertPropFinder {

/*
udanax-top.st:39643:
BertPropFinder subclass: #CannotPartializeFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39647:
CannotPartializeFinder comment:
'Used to figure out which Stamps have Orgls on them so that the archiver can knw that they cannot be partialized.  Will go away because the state described is session level state and therefore should be store in NOCOPY variables instead of in the Canopy''s Props.'!
*/
/*
udanax-top.st:39649:
(CannotPartializeFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CannotPartializeFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public CannotPartializeFinder() {
	super(BertCrum.isNotPartializableFlag());
/*
udanax-top.st:39654:CannotPartializeFinder methodsFor: 'create'!
create
	super create: BertCrum isNotPartializableFlag!
*/
}
public PropFinder findPast(BeEdition edition) {
	return this;
/*
udanax-top.st:39660:CannotPartializeFinder methodsFor: 'accessing'!
{PropFinder} findPast: edition {BeEdition unused}
	
	^self "inability to partialize is transitive"!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	return ((BertProp) prop).isNotPartializable();
/*
udanax-top.st:39664:CannotPartializeFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	"tell whether a prop matches this filter"
	^(prop cast: BertProp) isNotPartializable!
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual();
/*
udanax-top.st:39670:CannotPartializeFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	return other instanceof CannotPartializeFinder;
/*
udanax-top.st:39674:CannotPartializeFinder methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^other isKindOf: CannotPartializeFinder!
*/
}
/**
 * return a simple enough finder for looking at the children
 */
public PropFinder oldPass(PropJoint parent) {
	if (((BertPropJoint) parent).isNotPartializable()) {
		return this;
	}
	else {
		return PropFinder.closedPropFinder();
	}
/*
udanax-top.st:39680:CannotPartializeFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: parent {PropJoint}
	"return a simple enough finder for looking at the children"
	(parent cast: BertPropJoint) isNotPartializable
		ifTrue: [^self]
		ifFalse: [^PropFinder closedPropFinder]!
*/
}
public CannotPartializeFinder(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:39688:CannotPartializeFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:39691:CannotPartializeFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
}
