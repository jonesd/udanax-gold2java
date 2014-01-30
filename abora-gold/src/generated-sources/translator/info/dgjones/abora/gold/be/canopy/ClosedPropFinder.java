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
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.ClosedPropFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * The finder which matches nothing.  Used to indicate that this subtree is known to be
 * useless (no matches possible below here).
 */
public class ClosedPropFinder extends PropFinder {

/*
udanax-top.st:39745:
PropFinder subclass: #ClosedPropFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39749:
ClosedPropFinder comment:
'The finder which matches nothing.  Used to indicate that this subtree is known to be useless (no matches possible below here).'!
*/
/*
udanax-top.st:39751:
(ClosedPropFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ClosedPropFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public PropFinder findPast(BeEdition stamp) {
	return this;
/*
udanax-top.st:39756:ClosedPropFinder methodsFor: 'accessing'!
{PropFinder} findPast: stamp {BeEdition unused}
	
	^self!
*/
}
/**
 * Overridden only here
 */
public boolean isEmpty() {
	return true;
/*
udanax-top.st:39760:ClosedPropFinder methodsFor: 'accessing'!
{BooleanVar} isEmpty
	"Overridden only here"
	^true!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	return false;
/*
udanax-top.st:39764:ClosedPropFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop unused}
	"tell whether a prop matches this filter"
	^false!
*/
}
public PropFinder pass(CanopyCrum crum) {
	return this;
/*
udanax-top.st:39768:ClosedPropFinder methodsFor: 'accessing'!
{PropFinder} pass: crum {CanopyCrum unused}
	^self!
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual();
/*
udanax-top.st:39774:ClosedPropFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	return other instanceof ClosedPropFinder;
/*
udanax-top.st:39778:ClosedPropFinder methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^other isKindOf: ClosedPropFinder!
*/
}
public ClosedPropFinder() {
	super(0);
/*
udanax-top.st:39784:ClosedPropFinder methodsFor: 'create'!
create
	super create: UInt32Zero!
*/
}
public ClosedPropFinder(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:39790:ClosedPropFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:39793:ClosedPropFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
}
