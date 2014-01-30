/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.wrapper;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.spaces.cross.Tuple;
import info.dgjones.abora.gold.wrapper.FeAbstractWrapperDef;
import info.dgjones.abora.gold.wrapper.FeAbstractWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeConcreteWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class FeAbstractWrapperSpec extends FeWrapperSpec {

	protected PtrArray myConcreteSpecs;
/*
udanax-top.st:26204:
FeWrapperSpec subclass: #FeAbstractWrapperSpec
	instanceVariableNames: 'myConcreteSpecs {PtrArray of: FeConcreteWrapperSpec}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:26208:
(FeAbstractWrapperSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:26279:
FeAbstractWrapperSpec class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:26282:
(FeAbstractWrapperSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeAbstractWrapperSpec.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean certify(FeEdition edition) {
	for (int i = 0; i < myConcreteSpecs.count(); i ++ ) {
		if (((FeConcreteWrapperSpec) (myConcreteSpecs.fetch(i))).certify(edition)) {
			return true;
		}
	}
	return false;
/*
udanax-top.st:26213:FeAbstractWrapperSpec methodsFor: 'accessing'!
{BooleanVar} certify: edition {FeEdition}
	Int32Zero almostTo: myConcreteSpecs count do: [ :i {Int32} |
		(((myConcreteSpecs fetch: i) cast: FeConcreteWrapperSpec)
				certify: edition) ifTrue:
			[^true]].
	^false!
*/
}
/**
 * Add a new concrete spec to the list, keeping it topologically sorted so that if A wraps B,
 * A precedes B
 */
public void setupConcreteSubSpec(FeConcreteWrapperSpec spec) {
	int pos;
	PtrArray copy;
	/* remember its endorsements */
	addToFilter(spec.endorsements());
	/* Look for the last wrapper in the array that can wrap this one */
	pos = myConcreteSpecs.count();
	while ( ! (pos <= 0 || (((FeConcreteWrapperSpec) (myConcreteSpecs.fetch(pos - 1))).wraps(spec)))) {
		pos = pos - 1;
	}
	/* Make a copy and insert it just after that one */
	copy = (PtrArray) (myConcreteSpecs.copyGrow(1));
	for (int j = copy.count() - 1; j >= pos + 1; j -= 1 ) {
		copy.store(j, (copy.fetch(j - 1)));
	}
	copy.store(pos, spec);
	myConcreteSpecs = copy;
	/* Recur upwards to add the spec to my parent */
	setup();
	if (fetchSuperSpec() != null) {
		fetchSuperSpec().setupConcreteSubSpec(spec);
	}
/*
udanax-top.st:26221:FeAbstractWrapperSpec methodsFor: 'accessing'!
{void} setupConcreteSubSpec: spec {FeConcreteWrapperSpec}
	"Add a new concrete spec to the list, keeping it topologically sorted so that if A wraps B, A precedes B"
	
	| pos {Int32} copy {PtrArray of: FeConcreteWrapperSpec} |
	"remember its endorsements"
	self addToFilter: spec endorsements.
	"Look for the last wrapper in the array that can wrap this one"
	pos := myConcreteSpecs count.
	[(pos <= Int32Zero or: [((myConcreteSpecs fetch: pos - 1) cast: FeConcreteWrapperSpec) wraps: spec]) not]
		whileTrue:
			[pos := pos - 1].
	"Make a copy and insert it just after that one"
	copy := (myConcreteSpecs copyGrow:1) cast: PtrArray.
	copy count - 1 downTo: pos + 1 do: [ :j {Int32} |
		copy at: j store: (copy fetch: j - 1)].
	copy at: pos store: spec.
	myConcreteSpecs := copy.
	"Recur upwards to add the spec to my parent"
	self setup.
	self fetchSuperSpec ~~ NULL ifTrue:
		[self fetchSuperSpec setupConcreteSubSpec: spec]!
*/
}
public FeAbstractWrapperSpec(FeAbstractWrapperDef def) {
	super(def);
	myConcreteSpecs = PtrArray.empty();
/*
udanax-top.st:26245:FeAbstractWrapperSpec methodsFor: 'create'!
create: def {FeAbstractWrapperDef}
	super create: def.
	myConcreteSpecs := PtrArray empty!
*/
}
public void endorse(FeEdition edition) {
	throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_CONCRETE_WRAPPER_SPEC);
/*
udanax-top.st:26251:FeAbstractWrapperSpec methodsFor: 'for wrappers only'!
{void} endorse: edition {FeEdition unused}
	Heaper BLAST: #MustBeConcreteWrapperSpec!
*/
}
public FeWrapper fetchWrap(FeEdition edition) {
	FeConcreteWrapperSpec sub;
	FeWrapper result;
	Ravi.thingToDo();
	/* BLAST if there is an ambiguity; right now the only possible one is between an empty Path and and an empty Text */
	/* If there are any endorsements that match mine, pick a concrete type that isn't wrapped by anything else */
	sub = null;
	Stepper stomper = (edition.endorsements().intersect(endorsements())).stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Tuple end = (Tuple) stomper.fetch();
		if (end == null) {
			continue ;
		}
		FeConcreteWrapperSpec other;
		other = (FeConcreteWrapperSpec) (FeWrapperSpec.getFromEndorsement(end));
		if (sub == null || (other.wraps(sub))) {
			sub = other;
		}
	}
	stomper.destroy();
	if (sub != null) {
		return sub.fetchWrap(edition);
	}
	for (int i = 
	/* There are no endorsements. Just walk through the topological sort until you hit one which works */
	0; i < myConcreteSpecs.count(); i ++ ) {
		Heaper cast1 = (myConcreteSpecs.fetch(i));
		if (cast1 instanceof FeConcreteWrapperSpec) {
			FeConcreteWrapperSpec spec = (FeConcreteWrapperSpec) cast1;
			result = spec.fetchWrap(edition);
			if (result != null) {
				return result;
			}
		}
	}
	return null;
/*
udanax-top.st:26257:FeAbstractWrapperSpec methodsFor: 'vulnerable'!
{FeWrapper | NULL} fetchWrap: edition {FeEdition}
	| sub {FeConcreteWrapperSpec} result {FeWrapper} |
	Ravi thingToDo. "BLAST if there is an ambiguity; right now the only possible one is between an empty Path and and an empty Text"
	"If there are any endorsements that match mine, pick a concrete type that isn't wrapped by anything else"
	sub := NULL.
	(edition endorsements intersect: self endorsements) stepper forEach:
		[ :end {Tuple} | | other {FeConcreteWrapperSpec} | 
		other := (FeWrapperSpec getFromEndorsement: end) cast: FeConcreteWrapperSpec.
		(sub == NULL or: [other wraps: sub]) ifTrue:
			[sub := other]].
	sub ~~ NULL ifTrue:
		[^sub fetchWrap: edition].
	"There are no endorsements. Just walk through the topological sort until you hit one which works"
	Int32Zero almostTo: myConcreteSpecs count do: [ :i {Int32} |
		(myConcreteSpecs fetch: i) cast: FeConcreteWrapperSpec into: [ :spec |
			result := spec fetchWrap: edition.
			result ~~ NULL ifTrue:
				[^result]]].
	^NULL!
*/
}
public static FeAbstractWrapperSpec make(FeAbstractWrapperDef def) {
	return new FeAbstractWrapperSpec(def);
/*
udanax-top.st:26287:FeAbstractWrapperSpec class methodsFor: 'pseudo constructors'!
make: def {FeAbstractWrapperDef}
	^self create: def!
*/
}
public FeAbstractWrapperSpec() {
/*

Generated during transformation
*/
}
public FeAbstractWrapperSpec(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
