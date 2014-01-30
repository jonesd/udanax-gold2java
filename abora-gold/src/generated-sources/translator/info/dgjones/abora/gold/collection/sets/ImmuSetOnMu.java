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
import info.dgjones.abora.gold.collection.sets.ImmuSetOnMu;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ImmuSetOnMu extends ImmuSet {

	protected MuSet setInternal;
/*
udanax-top.st:45608:
ImmuSet subclass: #ImmuSetOnMu
	instanceVariableNames: 'setInternal {MuSet}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Sets'!
*/
/*
udanax-top.st:45612:
(ImmuSetOnMu getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
/*
udanax-top.st:45710:
ImmuSetOnMu class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:45713:
(ImmuSetOnMu getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ImmuSetOnMu.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public boolean hasMember(Heaper someone) {
	return setInternal.hasMember(someone);
/*
udanax-top.st:45617:ImmuSetOnMu methodsFor: 'accessing'!
{BooleanVar} hasMember: someone {Heaper}
	^ setInternal hasMember: someone!
*/
}
public boolean isEmpty() {
	return setInternal.isEmpty();
/*
udanax-top.st:45620:ImmuSetOnMu methodsFor: 'accessing'!
{BooleanVar} isEmpty
	^ setInternal isEmpty!
*/
}
public boolean isSubsetOf(ScruSet another) {
	return setInternal.isSubsetOf(another);
/*
udanax-top.st:45623:ImmuSetOnMu methodsFor: 'accessing'!
{BooleanVar} isSubsetOf: another {ScruSet}
	^ setInternal isSubsetOf: another!
*/
}
public int count() {
	return setInternal.count();
/*
udanax-top.st:45628:ImmuSetOnMu methodsFor: 'enumerating'!
{IntegerVar} count
	^ setInternal count!
*/
}
public Stepper stepper() {
	return setInternal.stepper();
/*
udanax-top.st:45631:ImmuSetOnMu methodsFor: 'enumerating'!
{Stepper} stepper
	^ setInternal stepper!
*/
}
public Heaper theOne() {
	return setInternal.theOne();
/*
udanax-top.st:45634:ImmuSetOnMu methodsFor: 'enumerating'!
{Heaper} theOne
	^ setInternal theOne!
*/
}
public ImmuSet intersect(ScruSet another) {
	if (another.isEmpty()) {
		return ImmuSet.make();
	}
	else {
		MuSet tmp;
		tmp = (MuSet) (setInternal.copy());
		tmp.restrictTo(another);
		return ImmuSet.from(tmp);
	}
/*
udanax-top.st:45639:ImmuSetOnMu methodsFor: 'operations'!
{ImmuSet} intersect: another {ScruSet} 
	another isEmpty
		ifTrue: [ ^ ImmuSet make ]
		ifFalse:
			[| tmp {MuSet} |
			tmp _ (setInternal copy) quickCast: MuSet.
			tmp restrictTo: another.
			^ ImmuSet from: tmp]!
*/
}
public ImmuSet minus(ScruSet another) {
	if (another.isEmpty()) {
		return this;
	}
	else {
		MuSet tmp;
		tmp = (MuSet) (setInternal.copy());
		tmp.wipeAll(another);
		return ImmuSet.from(tmp);
	}
/*
udanax-top.st:45648:ImmuSetOnMu methodsFor: 'operations'!
{ImmuSet} minus: another {ScruSet}
	another isEmpty
		ifTrue: [ ^ self ]
		ifFalse:
			[|tmp {MuSet} |
			tmp _ (setInternal copy) quickCast: MuSet.
			tmp wipeAll: another.
			^ ImmuSet from: tmp]!
*/
}
public ImmuSet unionWith(ScruSet another) {
	if (another.isEmpty()) {
		return this;
	}
	else {
		MuSet tmp;
		if (setInternal.count() < another.count()) {
			return another.asImmuSet().unionWith(setInternal);
		}
		tmp = (MuSet) setInternal.copy();
		tmp.storeAll(another);
		return ImmuSet.from(tmp);
	}
/*
udanax-top.st:45657:ImmuSetOnMu methodsFor: 'operations'!
{ImmuSet} unionWith: another {ScruSet}
	another isEmpty
		ifTrue: [ ^ self ]
		ifFalse:
			[| tmp {MuSet} |
			setInternal count < another count ifTrue: [^another asImmuSet unionWith: setInternal].
			tmp _ setInternal copy quickCast: MuSet.
			tmp storeAll: another.
			^ ImmuSet from: tmp]!
*/
}
public ImmuSet with(Heaper anElement) {
	MuSet tmp;
	tmp = asMuSet();
	tmp.store(anElement);
	return new ImmuSetOnMu(tmp);
/*
udanax-top.st:45669:ImmuSetOnMu methodsFor: 'adding-removing'!
{ImmuSet} with: anElement {Heaper}
	|tmp {MuSet} |
	tmp _ self asMuSet.
	tmp store: anElement.
	^ ImmuSetOnMu create.MuSet: tmp!
*/
}
public ImmuSet without(Heaper anElement) {
	MuSet tmp;
	tmp = (MuSet) (setInternal.copy());
	tmp.wipe(anElement);
	return ImmuSet.from(tmp);
/*
udanax-top.st:45675:ImmuSetOnMu methodsFor: 'adding-removing'!
{ImmuSet} without: anElement {Heaper}
	
	|tmp {MuSet} |
	tmp _ (setInternal copy) quickCast: MuSet.
	tmp wipe: anElement.
	^ ImmuSet from: tmp!
*/
}
public MuSet asMuSet() {
	return (MuSet) (setInternal.copy());
/*
udanax-top.st:45684:ImmuSetOnMu methodsFor: 'conversion'!
{MuSet} asMuSet
	^ (setInternal copy) cast: MuSet!
*/
}
/**
 * this set should be a copy for my own use
 */
public ImmuSetOnMu(MuSet fromSet) {
	super();
	/* the pseudo constructor enforces this */
	setInternal = fromSet;
/*
udanax-top.st:45689:ImmuSetOnMu methodsFor: 'protected: create'!
create.MuSet: fromSet {MuSet}
	"this set should be a copy for my own use"
	"the pseudo constructor enforces this"
	super create.
	setInternal _ fromSet!
*/
}
public void destruct() {
	setInternal.destroy();
	super.destruct();
/*
udanax-top.st:45695:ImmuSetOnMu methodsFor: 'protected: create'!
{void} destruct
	setInternal destroy.
	super destruct!
*/
}
public ImmuSetOnMu(Rcvr receiver) {
	super(receiver);
	setInternal = (MuSet) receiver.receiveHeaper();
/*
udanax-top.st:45701:ImmuSetOnMu methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	setInternal _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(setInternal);
/*
udanax-top.st:45705:ImmuSetOnMu methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: setInternal.!
*/
}
public static ImmuSet make(MuSet aSet) {
	return new ImmuSetOnMu(aSet);
/*
udanax-top.st:45718:ImmuSetOnMu class methodsFor: 'creation'!
{ImmuSet} make: aSet {MuSet}
	^ self create.MuSet: aSet!
*/
}
public ImmuSetOnMu() {
/*

Generated during transformation
*/
}
}
