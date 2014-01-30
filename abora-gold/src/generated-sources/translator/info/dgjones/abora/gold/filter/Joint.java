/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.filter;

import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.filter.Joint;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Joints are used to prune searches through trees of Regions. Each Joint summarizes the
 * Joints and Regions at its node and its children using their intersection and union. If you
 * maintain this information at each each node in the tree, then you can search for Regions
 * in the tree efficiently using Filter::pass() to adapt the search criteria to the contents
 * of the subtree. See also Filter::pass(Joint *).
 */
public class Joint extends Heaper {

	protected XnRegion myUnioned;
	protected XnRegion myIntersected;
/*
udanax-top.st:27751:
Heaper subclass: #Joint
	instanceVariableNames: '
		myUnioned {XnRegion}
		myIntersected {XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-filter'!
*/
/*
udanax-top.st:27757:
Joint comment:
'Joints are used to prune searches through trees of Regions. Each Joint summarizes the Joints and Regions at its node and its children using their intersection and union. If you maintain this information at each each node in the tree, then you can search for Regions in the tree efficiently using Filter::pass() to adapt the search criteria to the contents of the subtree. See also Filter::pass(Joint *).'!
*/
/*
udanax-top.st:27759:
(Joint getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:27823:
Joint class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:27826:
(Joint getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Joint.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Joint(XnRegion unioned, XnRegion intersected) {
	super();
	myUnioned = unioned;
	myIntersected = intersected;
/*
udanax-top.st:27764:Joint methodsFor: 'creation'!
create: unioned {XnRegion} with:  intersected{XnRegion}
	super create.
	myUnioned _ unioned.
	myIntersected _ intersected.!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(union: ");
	oo.print(myUnioned);
	oo.print("; intersected: ");
	oo.print(myIntersected);
	oo.print(")");
/*
udanax-top.st:27771:Joint methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(union: ' << myUnioned << '; intersected: ' << myIntersected << ')'!
*/
}
/**
 * The intersection of the regions at all child nodes in the tree.
 */
public XnRegion intersected() {
	return myIntersected;
/*
udanax-top.st:27776:Joint methodsFor: 'accessing'!
{XnRegion INLINE} intersected
	"The intersection of the regions at all child nodes in the tree."
	^myIntersected!
*/
}
/**
 * A Joint that is a parent of this Joint and the given one.
 */
public Joint join(Joint other) {
	return Joint.makeJoint(this, other);
/*
udanax-top.st:27781:Joint methodsFor: 'accessing'!
{Joint INLINE} join: other {Joint}
	"A Joint that is a parent of this Joint and the given one."
	
	^Joint make.Joint: self with: other!
*/
}
/**
 * The union of the regions at all child nodes in the tree.
 */
public XnRegion unioned() {
	return myUnioned;
/*
udanax-top.st:27786:Joint methodsFor: 'accessing'!
{XnRegion INLINE} unioned
	"The union of the regions at all child nodes in the tree."
	^myUnioned!
*/
}
/**
 * A Joint that is a parent of this one and the given region.
 */
public Joint with(XnRegion region) {
	return Joint.makeXnRegion((myUnioned.unionWith(region)), (myIntersected.intersect(region)));
/*
udanax-top.st:27791:Joint methodsFor: 'accessing'!
{Joint} with: region {XnRegion}
	"A Joint that is a parent of this one and the given region."
	
	^Joint make.XnRegion: (myUnioned unionWith: region)
		with: (myIntersected intersect: region)!
*/
}
public int actualHashForEqual() {
	return myUnioned.hashForEqual() + myIntersected.hashForEqual();
/*
udanax-top.st:27799:Joint methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myUnioned hashForEqual + myIntersected hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof Joint) {
		Joint o = (Joint) other;
		return (myUnioned.isEqual(o.unioned())) && (myIntersected.isEqual(o.intersected()));
	}
	else {
		return false;
	}
/*
udanax-top.st:27802:Joint methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	
	other cast: Joint into: [:o {Joint} |
			^(myUnioned isEqual: o unioned) and:
				[myIntersected isEqual: o intersected]]
		others: [^false].
	^false "fodder"!
*/
}
public Joint(Rcvr receiver) {
	super(receiver);
	myUnioned = (XnRegion) receiver.receiveHeaper();
	myIntersected = (XnRegion) receiver.receiveHeaper();
/*
udanax-top.st:27812:Joint methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myUnioned _ receiver receiveHeaper.
	myIntersected _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myUnioned);
	xmtr.sendHeaper(myIntersected);
/*
udanax-top.st:27817:Joint methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myUnioned.
	xmtr sendHeaper: myIntersected.!
*/
}
/**
 * An empty Joint in the given coordinate space.
 */
public static Joint makeCoordinateSpace(CoordinateSpace space) {
	return new Joint(space.emptyRegion(), space.fullRegion());
/*
udanax-top.st:27831:Joint class methodsFor: 'pseudo constructors'!
make.CoordinateSpace: space {CoordinateSpace}
	"An empty Joint in the given coordinate space."
	
	^Joint create: space emptyRegion with: space fullRegion!
*/
}
/**
 * A joint that is a parent of the two given Joints.
 */
public static Joint makeJoint(Joint left, Joint right) {
	return new Joint((left.unioned().unionWith(right.unioned())), (left.intersected().intersect(right.intersected())));
/*
udanax-top.st:27836:Joint class methodsFor: 'pseudo constructors'!
make.Joint: left {Joint} with: right {Joint}
	"A joint that is a parent of the two given Joints."
	
	^Joint create: (left unioned unionWith: right unioned)
		with: (left intersected intersect: right intersected)!
*/
}
/**
 * A Joint that is a parent of all of the Joints in the set.
 */
public static Joint makeScruSet(ScruSet subs) {
	XnRegion unioned;
	XnRegion intersected;
	Stepper subStepper;
	subStepper = subs.stepper();
	unioned = ((Joint) subStepper.get()).unioned();
	intersected = ((Joint) subStepper.fetch()).intersected();
	subStepper.step();
	Stepper stomper = subStepper;
	for (; stomper.hasValue(); stomper.step()) {
		Joint sub = (Joint) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		unioned = unioned.unionWith(sub.unioned());
		intersected = intersected.intersect(sub.intersected());
	}
	stomper.destroy();
	return new Joint(unioned, intersected);
/*
udanax-top.st:27842:Joint class methodsFor: 'pseudo constructors'!
make.ScruSet: subs {ScruSet of: Joint}
	"A Joint that is a parent of all of the Joints in the set."
	
	| unioned {XnRegion} intersected {XnRegion} subStepper {Stepper} |
	subStepper _ subs stepper.
	unioned _ (subStepper get cast: Joint) unioned.
	intersected _ (subStepper fetch cast: Joint) intersected.
	subStepper step.
	subStepper forEach: [ :sub {Joint} |
		unioned _ unioned unionWith: sub unioned.
		intersected _ intersected intersect: sub intersected].
	^Joint create: unioned with: intersected!
*/
}
/**
 * A Joint containing only the given region.
 */
public static Joint makeXnRegion(XnRegion both) {
	return new Joint(both, both);
/*
udanax-top.st:27855:Joint class methodsFor: 'pseudo constructors'!
make.XnRegion: both {XnRegion}
	"A Joint containing only the given region."
	
	^Joint create: both with: both!
*/
}
/**
 * A Joint with the given union and intersection regions.
 */
public static Joint makeXnRegion(XnRegion unioned, XnRegion intersected) {
	return new Joint(unioned, intersected);
/*
udanax-top.st:27860:Joint class methodsFor: 'pseudo constructors'!
make.XnRegion: unioned {XnRegion} with: intersected {XnRegion}
	"A Joint with the given union and intersection regions."
	
	^Joint create: unioned with: intersected!
*/
}
public static Joint make(Object something) {
	if (something instanceof XnRegion) {
		return makeXnRegion((XnRegion) something);
	}
	if (something instanceof CoordinateSpace) {
		return makeCoordinateSpace((CoordinateSpace) something);
	}
	return makeScruSet(((ScruSet) something));
/*
udanax-top.st:27867:Joint class methodsFor: 'smalltalk: smalltalk defaults'!
make: something
	(something isKindOf: XnRegion) ifTrue:
		[^self make.XnRegion: something].
	(something isKindOf: CoordinateSpace) ifTrue:
		[^self make.CoordinateSpace: something].
	^self make.ScruSet: (something cast: ScruSet)!
*/
}
public static Joint make(Object something, Object other) {
	if (something instanceof Joint) {
		return makeJoint((Joint) something, (Joint) other);
	}
	return makeXnRegion(((XnRegion) something), (XnRegion) other);
/*
udanax-top.st:27874:Joint class methodsFor: 'smalltalk: smalltalk defaults'!
make: something with: other
	(something isKindOf: Joint) ifTrue:
		[^self make.Joint: something with: other].
	^self make.XnRegion: (something cast: XnRegion) with: other!
*/
}
public Joint() {
/*

Generated during transformation
*/
}
}
