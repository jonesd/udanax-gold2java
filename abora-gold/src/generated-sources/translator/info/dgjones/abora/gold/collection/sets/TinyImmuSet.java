/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.sets;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.sets.TinyImmuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * This is an efficient implementation of ImmuSets for zero and one element sets.
 */
public class TinyImmuSet extends ImmuSet {

	protected Heaper elementInternal;
/*
udanax-top.st:45721:
ImmuSet subclass: #TinyImmuSet
	instanceVariableNames: 'elementInternal {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Sets'!
*/
/*
udanax-top.st:45725:
TinyImmuSet comment:
'This is an efficient implementation of ImmuSets for zero and one element sets.'!
*/
/*
udanax-top.st:45727:
(TinyImmuSet getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
/*
udanax-top.st:45812:
TinyImmuSet class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:45815:
(TinyImmuSet getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TinyImmuSet.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Initialize a singleton immuset
 */
public TinyImmuSet(Heaper only) {
	super();
	elementInternal = only;
/*
udanax-top.st:45732:TinyImmuSet methodsFor: 'protected: creation'!
create: only {Heaper}
	"Initialize a singleton immuset"
	super create.
	elementInternal _ only!
*/
}
public int count() {
	return 1;
/*
udanax-top.st:45739:TinyImmuSet methodsFor: 'enumerating'!
{IntegerVar} count
	^ 1!
*/
}
public Stepper stepper() {
	return Stepper.itemStepper(elementInternal);
/*
udanax-top.st:45742:TinyImmuSet methodsFor: 'enumerating'!
{Stepper} stepper
	^Stepper itemStepper: elementInternal!
*/
}
public Heaper theOne() {
	return elementInternal;
/*
udanax-top.st:45745:TinyImmuSet methodsFor: 'enumerating'!
{Heaper} theOne
	^ elementInternal!
*/
}
public ImmuSet with(Heaper anElement) {
	if (elementInternal.isEqual(anElement)) {
		return this;
	}
	else {
		MuSet nuSet;
		nuSet = MuSet.makeHeaper(anElement);
		nuSet.introduce(elementInternal);
		return ImmuSet.make(nuSet);
	}
/*
udanax-top.st:45750:TinyImmuSet methodsFor: 'adding-removing'!
{ImmuSet} with: anElement {Heaper} 
	(elementInternal isEqual: anElement)
		ifTrue: [^self]
		ifFalse: [| nuSet {MuSet} |
					nuSet _ MuSet make.Heaper: anElement.
					nuSet introduce: elementInternal.
					^ImmuSet make: nuSet]!
*/
}
public ImmuSet without(Heaper anElement) {
	if (elementInternal.isEqual(anElement)) {
		return ImmuSet.make();
	}
	return this;
/*
udanax-top.st:45758:TinyImmuSet methodsFor: 'adding-removing'!
{ImmuSet} without: anElement {Heaper}
	(elementInternal isEqual: anElement) ifTrue: [^ ImmuSet make].
	^ self!
*/
}
public boolean hasMember(Heaper someone) {
	return elementInternal.isEqual(someone);
/*
udanax-top.st:45764:TinyImmuSet methodsFor: 'accessing'!
{BooleanVar} hasMember: someone {Heaper}
	^ elementInternal isEqual: someone!
*/
}
public boolean isEmpty() {
	return false;
/*
udanax-top.st:45767:TinyImmuSet methodsFor: 'accessing'!
{BooleanVar} isEmpty
	^ false!
*/
}
public boolean isSubsetOf(ScruSet another) {
	return another.hasMember(elementInternal);
/*
udanax-top.st:45770:TinyImmuSet methodsFor: 'accessing'!
{BooleanVar} isSubsetOf: another {ScruSet}
	^ another hasMember: elementInternal!
*/
}
public ImmuSet intersect(ScruSet another) {
	if (another.hasMember(elementInternal)) {
		return this;
	}
	else {
		return ImmuSet.make();
	}
/*
udanax-top.st:45775:TinyImmuSet methodsFor: 'operations'!
{ImmuSet} intersect: another {ScruSet} 
	(another hasMember: elementInternal)
		ifTrue: [^ self]
		ifFalse: [^ ImmuSet make]!
*/
}
public ImmuSet minus(ScruSet another) {
	if (another.hasMember(elementInternal)) {
		return ImmuSet.make();
	}
	else {
		return this;
	}
/*
udanax-top.st:45780:TinyImmuSet methodsFor: 'operations'!
{ImmuSet} minus: another {ScruSet}
	(another hasMember: elementInternal) 
		ifTrue: [^ ImmuSet make]
		ifFalse: [^ self]!
*/
}
public ImmuSet unionWith(ScruSet another) {
	if (another.isEmpty()) {
		return this;
	}
	else {
		MuSet nuSet;
		if (another.hasMember(elementInternal)) {
			return another.asImmuSet();
		}
		if (another.count() > 5) {
			return another.asImmuSet().unionWith(this);
		}
		nuSet = MuSet.makeHeaper(elementInternal);
		nuSet.storeAll(another);
		return ImmuSet.make(nuSet);
	}
/*
udanax-top.st:45785:TinyImmuSet methodsFor: 'operations'!
{ImmuSet} unionWith: another {ScruSet}
	another isEmpty 
		ifTrue: [^ self]
		ifFalse:
			[| nuSet {MuSet} |
			(another hasMember: elementInternal) ifTrue: [ ^ another asImmuSet ].
			another count > 5 ifTrue: [^another asImmuSet unionWith: self].
			nuSet _ MuSet make.Heaper: elementInternal.
			nuSet storeAll: another.
			^ ImmuSet make: nuSet]!
*/
}
public MuSet asMuSet() {
	return MuSet.makeHeaper(elementInternal);
/*
udanax-top.st:45798:TinyImmuSet methodsFor: 'conversion'!
{MuSet} asMuSet
	^ MuSet make.Heaper: elementInternal!
*/
}
public TinyImmuSet(Rcvr receiver) {
	super(receiver);
	elementInternal = receiver.receiveHeaper();
/*
udanax-top.st:45803:TinyImmuSet methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	elementInternal _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(elementInternal);
/*
udanax-top.st:45807:TinyImmuSet methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: elementInternal.!
*/
}
public static ImmuSet make(Heaper aHeaper) {
	return new TinyImmuSet(aHeaper);
/*
udanax-top.st:45820:TinyImmuSet class methodsFor: 'create'!
{ImmuSet} make: aHeaper {Heaper}
	^ self create: aHeaper!
*/
}
public TinyImmuSet() {
/*

Generated during transformation
*/
}
}
