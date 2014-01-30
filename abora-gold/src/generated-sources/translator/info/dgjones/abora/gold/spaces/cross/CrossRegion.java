/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.cross;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A cross region is a distinction if 1) it is empty, 2) it is full, or 3) it is the
 * rectangular cross of full regions and one distinction. Note that case 3 actually subsumes
 * 1 and 2.  Since the simple regions of a space are the intersections of a finite number of
 * distinctions of a space, this implies that A cross region is simple if it is the
 * rectangular cross of simple regions.  In other words, a simple region is identical to the
 * cross of its projections.
 */
public class CrossRegion extends XnRegion {

/*
udanax-top.st:65511:
XnRegion subclass: #CrossRegion
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Cross'!
*/
/*
udanax-top.st:65515:
CrossRegion comment:
'A cross region is a distinction if 1) it is empty, 2) it is full, or 3) it is the rectangular cross of full regions and one distinction. Note that case 3 actually subsumes 1 and 2.  Since the simple regions of a space are the intersections of a finite number of distinctions of a space, this implies that A cross region is simple if it is the rectangular cross of simple regions.  In other words, a simple region is identical to the cross of its projections.'!
*/
/*
udanax-top.st:65517:
(CrossRegion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:65620:
CrossRegion class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:65623:
(CrossRegion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CrossRegion.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * To avoid overly burdensome canonicalization rules, my hash is calculated from the hash of
 * my projections
 */
public int actualHashForEqual() {
	return HashHelper.hashForEqual(this.getClass()) ^ projections().contentsHash();
/*
udanax-top.st:65522:CrossRegion methodsFor: 'testing'!
{UInt32} actualHashForEqual
	"To avoid overly burdensome canonicalization rules, my hash is calculated from the hash of my projections"
	
	^#cat.U.CrossRegion hashForEqual bitXor: self projections contentsHash.!
*/
}
public boolean hasMember(Position atPos) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65527:CrossRegion methodsFor: 'testing'!
{BooleanVar} hasMember: atPos {Position unused} 
	self subclassResponsibility!
*/
}
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65531:CrossRegion methodsFor: 'testing'!
{BooleanVar} isEmpty
	self subclassResponsibility!
*/
}
public boolean isEnumerable(OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65535:CrossRegion methodsFor: 'testing'!
{BooleanVar} isEnumerable: order {OrderSpec unused default: NULL}
	
	self subclassResponsibility!
*/
}
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65539:CrossRegion methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	
	self subclassResponsibility!
*/
}
public boolean isFinite() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65543:CrossRegion methodsFor: 'testing'!
{BooleanVar} isFinite
	self subclassResponsibility!
*/
}
public boolean isSimple() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65547:CrossRegion methodsFor: 'testing'!
{BooleanVar} isSimple
	
	self subclassResponsibility!
*/
}
/**
 * Essential. Divide this Region up into a disjoint sequence of boxes. A box is a region
 * which is the cross of its projections.
 */
public Stepper boxes() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65553:CrossRegion methodsFor: 'enumerating'!
{Stepper CLIENT of: CrossRegion} boxes
	"Essential. Divide this Region up into a disjoint sequence of boxes. A box is a region which is the cross of its projections."
	
	self subclassResponsibility!
*/
}
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65558:CrossRegion methodsFor: 'enumerating'!
{IntegerVar} count
	
	self subclassResponsibility!
*/
}
public ScruSet distinctions() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65562:CrossRegion methodsFor: 'enumerating'!
{ScruSet of: XnRegion} distinctions
	self subclassResponsibility!
*/
}
/**
 * Whether this Region is a box, i.e. is equal to the cross of its projections.
 */
public boolean isBox() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65566:CrossRegion methodsFor: 'enumerating'!
{BooleanVar CLIENT} isBox
	"Whether this Region is a box, i.e. is equal to the cross of its projections."
	
	self subclassResponsibility!
*/
}
public Stepper simpleRegions(OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65571:CrossRegion methodsFor: 'enumerating'!
{Stepper} simpleRegions: order {OrderSpec default: NULL} 
	self subclassResponsibility!
*/
}
public XnRegion asSimpleRegion() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65577:CrossRegion methodsFor: 'operations'!
{XnRegion} asSimpleRegion
	self subclassResponsibility!
*/
}
public XnRegion complement() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65581:CrossRegion methodsFor: 'operations'!
{XnRegion} complement
	self subclassResponsibility!
*/
}
public XnRegion intersect(XnRegion other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65585:CrossRegion methodsFor: 'operations'!
{XnRegion} intersect: other {XnRegion unused} 
	self subclassResponsibility!
*/
}
public XnRegion simpleUnion(XnRegion other) {
	return (unionWith(other)).asSimpleRegion();
/*
udanax-top.st:65589:CrossRegion methodsFor: 'operations'!
{XnRegion} simpleUnion: other {XnRegion} 
	
	^(self unionWith: other) asSimpleRegion!
*/
}
public XnRegion unionWith(XnRegion other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65593:CrossRegion methodsFor: 'operations'!
{XnRegion} unionWith: other {XnRegion unused} 
	self subclassResponsibility!
*/
}
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65599:CrossRegion methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	
	self subclassResponsibility!
*/
}
/**
 * The answer is the projection of this region into the specified dimension of the cross
 * space
 */
public XnRegion projection(int index) {
	return (XnRegion) (projections().fetch(index));
/*
udanax-top.st:65603:CrossRegion methodsFor: 'accessing'!
{XnRegion CLIENT} projection: index {Int32}
	"The answer is the projection of this region into the specified dimension of the cross space"
	
	^(self projections fetch: index) cast: XnRegion!
*/
}
/**
 * Essential.  The answer is the projection of this region into each dimension of the cross
 * space. Note that two regions which are different can have the same projections.
 */
public PtrArray projections() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65608:CrossRegion methodsFor: 'accessing'!
{PtrArray CLIENT of: XnRegion} projections
	"Essential.  The answer is the projection of this region into each dimension of the cross space. Note that two regions which are different can have the same projections."
	
	self subclassResponsibility!
*/
}
public Stepper actualStepper(OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65615:CrossRegion methodsFor: 'protected: enumerating'!
{Stepper of: Position} actualStepper: order {OrderSpec} 
	self subclassResponsibility!
*/
}
/**
 * {Stepper CLIENT of: CrossRegion} boxes
 * {BooleanVar CLIENT} isBox
 * {XuRegion CLIENT} projection: index {Int32}
 * {PtrArray CLIENT of: XuRegion} projections
 */
public static void infostProtocol() {
/*
udanax-top.st:65628:CrossRegion class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Stepper CLIENT of: CrossRegion} boxes
{BooleanVar CLIENT} isBox
{XuRegion CLIENT} projection: index {Int32}
{PtrArray CLIENT of: XuRegion} projections
"!
*/
}
public CrossRegion() {
/*

Generated during transformation
*/
}
public CrossRegion(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
