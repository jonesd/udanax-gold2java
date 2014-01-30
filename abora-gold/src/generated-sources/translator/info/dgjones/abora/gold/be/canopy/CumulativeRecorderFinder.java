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
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.CumulativeRecorderFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.canopy.SimpleRecorderFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Propagates a change to all recorders which might be interested in it, and picking up all
 * elements which might newly be made visible by it. The generators make new finders as we
 * pass by additional Edition boundaries. Also holds onto a collection of simple finders
 * looking for recorders triggered by specific Works or Editions. The current set contains
 * those which might record the current edition, and are passed to all Recorders. The others
 * are only passed to Recorders with the directContainersOnly flag off.
 */
public class CumulativeRecorderFinder extends AbstractRecorderFinder {

	protected ImmuSet myGenerators;
	protected ImmuSet myCurrent;
	protected ImmuSet myOthers;
/*
udanax-top.st:40219:
AbstractRecorderFinder subclass: #CumulativeRecorderFinder
	instanceVariableNames: '
		myGenerators {ImmuSet of: AnyRecorderFinder}
		myCurrent {ImmuSet of: SimpleRecorderFinder}
		myOthers {ImmuSet of: SimpleRecorderFinder | AnyRecorderFinder}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:40226:
CumulativeRecorderFinder comment:
'Propagates a change to all recorders which might be interested in it, and picking up all elements which might newly be made visible by it. The generators make new finders as we pass by additional Edition boundaries. Also holds onto a collection of simple finders looking for recorders triggered by specific Works or Editions. The current set contains those which might record the current edition, and are passed to all Recorders. The others are only passed to Recorders with the directContainersOnly flag off.'!
*/
/*
udanax-top.st:40228:
(CumulativeRecorderFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:40349:
CumulativeRecorderFinder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:40352:
(CumulativeRecorderFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CumulativeRecorderFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void checkRecorder(ResultRecorder recorder, RecorderFossil fossil) {
	Stepper stomper = myCurrent.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		SimpleRecorderFinder current = (SimpleRecorderFinder) stomper.fetch();
		if (current == null) {
			continue ;
		}
		current.checkRecorder(recorder, fossil);
	}
	stomper.destroy();
	if ( ! (recorder.isDirectOnly())) {
		Stepper stomper2 = myOthers.stepper();
		for (; stomper2.hasValue(); stomper2.step()) {
			SimpleRecorderFinder other = (SimpleRecorderFinder) stomper2.fetch();
			if (other == null) {
				continue ;
			}
			other.checkRecorder(recorder, fossil);
		}
		stomper2.destroy();
	}
/*
udanax-top.st:40233:CumulativeRecorderFinder methodsFor: 'recording'!
{void} checkRecorder: recorder {ResultRecorder}
	with: fossil {RecorderFossil}
	myCurrent stepper forEach: [ :current {SimpleRecorderFinder} |
		current checkRecorder: recorder with: fossil].
	recorder isDirectOnly ifFalse:
		[myOthers stepper forEach: [ :other {SimpleRecorderFinder} |
			other checkRecorder: recorder with: fossil]]!
*/
}
public CumulativeRecorderFinder(int flags, ImmuSet generators, ImmuSet current, ImmuSet others) {
	super(flags);
	myGenerators = generators;
	myCurrent = current;
	myOthers = others;
/*
udanax-top.st:40244:CumulativeRecorderFinder methodsFor: 'create'!
create: flags {UInt32}
	with: generators {ImmuSet of: AnyRecorderFinder}
	with: current {ImmuSet of: SimpleRecorderFinder}
	with: others {ImmuSet of: SimpleRecorderFinder | AnyRecorderFinder}
	
	super create: flags.
	myGenerators := generators.
	myCurrent := current.
	myOthers := others.!
*/
}
public ImmuSet current() {
	return myCurrent;
/*
udanax-top.st:40256:CumulativeRecorderFinder methodsFor: 'accessing'!
{ImmuSet of: AnyRecorderFinder} current
	^myCurrent!
*/
}
public PropFinder findPast(BeEdition edition) {
	SetAccumulator newCurrent;
	newCurrent = SetAccumulator.make();
	Stepper stomper = myGenerators.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		AnyRecorderFinder gen = (AnyRecorderFinder) stomper.fetch();
		if (gen == null) {
			continue ;
		}
		PropFinder next;
		next = gen.nextFinder(edition);
		if ( ! (next.isEmpty())) {
			newCurrent.step(((SimpleRecorderFinder) next)
			/* cast will catch algorithm bugs in a place from which they are easier to fix */
			);
		}
	}
	stomper.destroy();
	return CumulativeRecorderFinder.make(myGenerators, ((ImmuSet) newCurrent.value()), (myOthers.unionWith(myCurrent)));
/*
udanax-top.st:40260:CumulativeRecorderFinder methodsFor: 'accessing'!
{PropFinder} findPast: edition {BeEdition}
	
	| newCurrent {SetAccumulator} |
	newCurrent := SetAccumulator make.
	myGenerators stepper forEach: [ :gen {AnyRecorderFinder} |
		| next {PropFinder} |
		next := gen nextFinder: edition.
		next isEmpty ifFalse:
			[newCurrent step: (next cast: SimpleRecorderFinder) "cast will catch algorithm bugs in a place from which they are easier to fix" ]].
	^CumulativeRecorderFinder
		make: myGenerators
		with: (newCurrent value cast: ImmuSet)
		with: (myOthers unionWith: myCurrent)!
*/
}
public ImmuSet generators() {
	return myGenerators;
/*
udanax-top.st:40274:CumulativeRecorderFinder methodsFor: 'accessing'!
{ImmuSet of: AnyRecorderFinder} generators
	^myGenerators!
*/
}
public boolean match(Prop prop) {
	Stepper stomper = myGenerators.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		PropFinder gen = (PropFinder) stomper.fetch();
		if (gen == null) {
			continue ;
		}
		if (gen.match(prop)) {
			return true;
		}
	}
	stomper.destroy();
	return false;
/*
udanax-top.st:40278:CumulativeRecorderFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	myGenerators stepper forEach: [ :gen {PropFinder} |
		(gen match: prop) ifTrue: [^true]].
	^false!
*/
}
public ImmuSet others() {
	return myOthers;
/*
udanax-top.st:40284:CumulativeRecorderFinder methodsFor: 'accessing'!
{ImmuSet of: SimpleRecorderFinder | AnyRecorderFinder} others
	^myOthers!
*/
}
public PropFinder pass(CanopyCrum parent) {
	if (parent instanceof SensorCrum) {
		SensorCrum p = (SensorCrum) parent;
		SetAccumulator newGenerators;
		SetAccumulator newCurrent;
		SetAccumulator newOthers;
		PropFinder past;
		newGenerators = SetAccumulator.make();
		Stepper stomper = myGenerators.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			PropFinder gen = (PropFinder) stomper.fetch();
			if (gen == null) {
				continue ;
			}
			past = gen.pass(p);
			if ( ! (past.isEmpty())) {
				newGenerators.step(past);
			}
		}
		stomper.destroy();
		if (((ImmuSet) newGenerators.value()).isEmpty()) {
			return PropFinder.closedPropFinder();
		}
		newCurrent = SetAccumulator.make();
		Stepper stomper2 = myCurrent.stepper();
		for (; stomper2.hasValue(); stomper2.step()) {
			PropFinder current = (PropFinder) stomper2.fetch();
			if (current == null) {
				continue ;
			}
			past = current.pass(p);
			if ( ! (past.isEmpty())) {
				newCurrent.step(past);
			}
		}
		stomper2.destroy();
		newOthers = SetAccumulator.make();
		Stepper stomper3 = myOthers.stepper();
		for (; stomper3.hasValue(); stomper3.step()) {
			PropFinder other = (PropFinder) stomper3.fetch();
			if (other == null) {
				continue ;
			}
			past = other.pass(p);
			if ( ! (past.isEmpty())) {
				newOthers.step(past);
			}
		}
		stomper3.destroy();
		return CumulativeRecorderFinder.make(((ImmuSet) newGenerators.value()), ((ImmuSet) newCurrent.value()), ((ImmuSet) newOthers.value()));
	}
	return null;
/*
udanax-top.st:40288:CumulativeRecorderFinder methodsFor: 'accessing'!
{PropFinder} pass: parent {CanopyCrum}
	parent cast: SensorCrum into: [ :p |
		| newGenerators {SetAccumulator} newCurrent {SetAccumulator}
		  newOthers {SetAccumulator} past {PropFinder} |
		newGenerators := SetAccumulator make.
		myGenerators stepper forEach: [ :gen {PropFinder} |
			past := gen pass: p.
			past isEmpty ifFalse:
				[newGenerators step: past]].
		(newGenerators value cast: ImmuSet) isEmpty ifTrue:
			[^PropFinder closedPropFinder].
		newCurrent := SetAccumulator make.
		myCurrent stepper forEach: [ :current {PropFinder} |
			past := current pass: p.
			past isEmpty ifFalse:
				[newCurrent step: past]].
		newOthers := SetAccumulator make.
		myOthers stepper forEach: [ :other {PropFinder} |
			past := other pass: p.
			past isEmpty ifFalse:
				[newOthers step: past]].
		^CumulativeRecorderFinder
			make: (newGenerators value cast: ImmuSet)
			with: (newCurrent value cast: ImmuSet)
			with: (newOthers value cast: ImmuSet)].
	^NULL "fodder"!
*/
}
public int actualHashForEqual() {
	return (myGenerators.hashForEqual() ^ myCurrent.hashForEqual()) ^ myOthers.hashForEqual();
/*
udanax-top.st:40318:CumulativeRecorderFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(myGenerators hashForEqual
		bitXor: myCurrent hashForEqual)
		bitXor: myOthers hashForEqual!
*/
}
public boolean isEqual(Heaper heaper) {
	if (heaper instanceof CumulativeRecorderFinder) {
		CumulativeRecorderFinder other = (CumulativeRecorderFinder) heaper;
		return (myGenerators.isEqual(other.generators())) && ((myCurrent.isEqual(other.current())) && (myOthers.isEqual(other.others())));
	}
	else {
		return false;
	}
/*
udanax-top.st:40324:CumulativeRecorderFinder methodsFor: 'testing'!
{BooleanVar} isEqual: heaper {Heaper}
	heaper cast: CumulativeRecorderFinder into: [ :other |
		^(myGenerators isEqual: other generators)
			and: [(myCurrent isEqual: other current)
			and: [myOthers isEqual: other others]]]
	others:
		[^false].
	^false "fodder"!
*/
}
public CumulativeRecorderFinder(Rcvr receiver) {
	super(receiver);
	myGenerators = (ImmuSet) receiver.receiveHeaper();
	myCurrent = (ImmuSet) receiver.receiveHeaper();
	myOthers = (ImmuSet) receiver.receiveHeaper();
/*
udanax-top.st:40336:CumulativeRecorderFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myGenerators _ receiver receiveHeaper.
	myCurrent _ receiver receiveHeaper.
	myOthers _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myGenerators);
	xmtr.sendHeaper(myCurrent);
	xmtr.sendHeaper(myOthers);
/*
udanax-top.st:40342:CumulativeRecorderFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myGenerators.
	xmtr sendHeaper: myCurrent.
	xmtr sendHeaper: myOthers.!
*/
}
public static PropFinder make(ImmuSet generators, ImmuSet current, ImmuSet others) {
	int f;
	if (generators.isEmpty()) {
		return PropFinder.closedPropFinder();
	}
	Ravi.thingToDo();
	/* since current & generators can have at most two elements, represent them explicitly as two OR(NULL) pointers? or make special SmallImmuSet class? */
	f = 0;
	Stepper stomper = generators.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		AnyRecorderFinder g = (AnyRecorderFinder) stomper.fetch();
		if (g == null) {
			continue ;
		}
		f = f | g.flags();
	}
	stomper.destroy();
	Stepper stomper2 = current.stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		SimpleRecorderFinder c = (SimpleRecorderFinder) stomper2.fetch();
		if (c == null) {
			continue ;
		}
		f = f | c.flags();
	}
	stomper2.destroy();
	Stepper stomper3 = others.stepper();
	for (; stomper3.hasValue(); stomper3.step()) {
		AbstractRecorderFinder o = (AbstractRecorderFinder) stomper3.fetch();
		if (o == null) {
			continue ;
		}
		f = f | o.flags();
	}
	stomper3.destroy();
	return new CumulativeRecorderFinder(f, generators, current, others);
/*
udanax-top.st:40357:CumulativeRecorderFinder class methodsFor: 'create'!
{PropFinder} make: generators {ImmuSet of: SimpleRecorderFinder}
	with: current {ImmuSet of: SimpleRecorderFinder}
	with: others {ImmuSet of: SimpleRecorderFinder}
	
	| f {UInt32} |
	generators isEmpty ifTrue:
		[^PropFinder closedPropFinder].
	Ravi thingToDo. "since current & generators can have at most two elements, represent them explicitly as two OR(NULL) pointers? or make special SmallImmuSet class?"
	f := UInt32Zero.
	generators stepper forEach: [ :g {AnyRecorderFinder} |
		f := f bitOr: g flags].
	current stepper forEach: [ :c {SimpleRecorderFinder} |
		f := f bitOr: c flags].
	others stepper forEach: [ :o {AbstractRecorderFinder} |
		f := f bitOr: o flags].
	^self create: f
		with: generators
		with: current
		with: others!
*/
}
public CumulativeRecorderFinder() {
/*

Generated during transformation
*/
}
}
