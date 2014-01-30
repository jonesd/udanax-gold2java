/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeCarrier;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.brange3.BeEditionDetectorExecutor;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.grand.GrandHashSet;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Accumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeLabel;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.primtab.PrimSet;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.Matcher;
import info.dgjones.abora.gold.turtle.NorthRecorderChecker;
import info.dgjones.abora.gold.turtle.Sequencer;
import info.dgjones.abora.gold.turtle.SouthRecorderChecker;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class BeEdition extends BeRangeElement {

	protected OrglRoot myOrglRoot;
	protected MuSet myWorks;
	protected BertProp myOwnProp;
	protected BertProp myProp;
	protected PrimSet myDetectors;
/*
udanax-top.st:2504:
BeRangeElement subclass: #BeEdition
	instanceVariableNames: '
		myOrglRoot {OrglRoot}
		myWorks {MuSet of: BeWork}
		myOwnProp {BertProp}
		myProp {BertProp}
		myDetectors {(PrimSet NOCOPY of: FeFillRangeDetector)
	| NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:2514:
(BeEdition getOrMakeCxxClassDescription)
	friends:
'friend class Matcher;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:3325:
BeEdition class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:3328:
(BeEdition getOrMakeCxxClassDescription)
	friends:
'friend class Matcher;
';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeEdition.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * An Edition with the contents of both Editions; where they share keys, they must have the
 * same RangeElement.
 */
public BeEdition combine(BeEdition other) {
	if (other.isEmpty()) {
		return this;
	}
	if (isEmpty()) {
		return other;
	}
	/* Eventually trace coordinates should be delayed. */
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, (hCrum().hCut().newSuccessorAfter(other.hCrum().hCut())));
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			return BeEdition.make((myOrglRoot.combine(other.orglRoot())));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:2522:BeEdition methodsFor: 'operations'!
{BeEdition} combine: other {BeEdition}
	"An Edition with the contents of both Editions; where they share keys, they must have the same RangeElement."
	
	other isEmpty ifTrue: [^self].
	self isEmpty ifTrue: [^other].
	"Eventually trace coordinates should be delayed."
	[HistoryCrum] USES.
	[TracePosition] USES.
	[Ent] USES.
	CurrentTrace fluidBind: (self hCrum hCut newSuccessorAfter: other hCrum hCut)
		during: [CurrentBertCrum fluidBind: BertCrum make 
		during: [^BeEdition make: (myOrglRoot combine: other orglRoot)]]!
*/
}
/**
 * A new Edition with the domain restricted to the given set of keys.
 */
public BeEdition copy(XnRegion keys) {
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, hCrum().hCut().newSuccessor());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			return BeEdition.make((myOrglRoot.copy(keys)));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:2535:BeEdition methodsFor: 'operations'!
{BeEdition} copy: keys {XnRegion}
	"A new Edition with the domain restricted to the given set of keys."
	
	CurrentTrace fluidBind: self hCrum hCut newSuccessor 
		during: [CurrentBertCrum fluidBind: BertCrum make
		during: [^BeEdition make: (myOrglRoot copy: keys)]]!
*/
}
/**
 * An Edition with the contents of both Editions; where they share keys, use the contents of
 * the other Edition. Equivalent to
 * this->copy (other->domain ()->complement ())->combine (other)
 */
public BeEdition replace(BeEdition other) {
	Someone.thingToDo();
	/* This should be implemented directly. */
	return (copy(other.domain().complement())).combine(other);
/*
udanax-top.st:2542:BeEdition methodsFor: 'operations'!
{BeEdition} replace: other {BeEdition}
	"An Edition with the contents of both Editions; where they share keys, use the contents of the other Edition. Equivalent to
		this->copy (other->domain ()->complement ())->combine (other)"
	
	self thingToDo.  "This should be implemented directly."
	^(self copy: other domain complement) combine: other!
*/
}
/**
 * An Edition with the keys transformed according to the given Mapping. Where the Mapping
 * takes several keys in the domain to a single key in the range, this Edition must have the
 * same RangeElement at all the domain keys.
 */
public BeEdition transformedBy(Mapping mapping) {
	OrglRoot resultRoot;
	XnRegion domain;
	if (mapping instanceof Dsp) {
		Dsp dsp = (Dsp) mapping;
		if (dsp.isIdentity()) {
			return this;
		}
		Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, hCrum().hCut().newSuccessor());
		try {
			Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
			try {
				return BeEdition.make((myOrglRoot.transformedBy(dsp)));
			}
			finally {
				AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
		}
	}
	else {
		/* The rest of the method */
		;
	}
	Object currentTraceOldValue1 = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, hCrum().hCut().newSuccessor());
	try {
		Object currentBertCrumOldValue1 = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			domain = myOrglRoot.simpleDomain();
			resultRoot = OrglRoot.makeCoordinateSpace(mapping.rangeSpace());
			Stepper stomper = mapping.simpleMappings().stepper();
			for (; stomper.hasValue(); stomper.step()) {
				Mapping simple = (Mapping) stomper.fetch();
				if (simple == null) {
					continue ;
				}
				XnRegion common;
				common = domain.intersect(simple.domain());
				if ( ! (common.isEmpty())) {
					Dsp dsp1;
					if ((dsp1 = simple.fetchDsp()) != null) {
						resultRoot = resultRoot.combine(((myOrglRoot.copy(common)).transformedBy(dsp1)));
					}
					else {
						throw new UnimplementedException();
					}
				}
			}
			stomper.destroy();
			return BeEdition.make(resultRoot);
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue1);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue1);
	}
/*
udanax-top.st:2549:BeEdition methodsFor: 'operations'!
{BeEdition} transformedBy: mapping {Mapping}
	"An Edition with the keys transformed according to the given Mapping. Where the Mapping takes several keys in the domain to a single key in the range, this Edition must have the same RangeElement at all the domain keys."
	
	| resultRoot {OrglRoot} domain {XnRegion} |
	mapping 
		cast: Dsp into: 
			[:dsp | dsp isIdentity ifTrue: [^self].
			CurrentTrace fluidBind: self hCrum hCut newSuccessor during: 
			[CurrentBertCrum fluidBind: BertCrum make during: 
				[^BeEdition make: (myOrglRoot transformedBy: dsp)]]]
		others: ["The rest of the method"].
	CurrentTrace fluidBind: self hCrum hCut newSuccessor during: 
	[CurrentBertCrum fluidBind: BertCrum make during: 
		[domain _ myOrglRoot simpleDomain. 
		resultRoot _ OrglRoot make.CoordinateSpace: mapping rangeSpace.
		mapping simpleMappings stepper forEach:
			[:simple {Mapping} | 
			| common {XnRegion} |
			common _ domain intersect: simple domain.
			common isEmpty ifFalse:
				[ | dsp {Dsp} |
				(dsp _ simple fetchDsp) ~~ NULL 
					ifTrue:
						[resultRoot _ 
							resultRoot combine: ((myOrglRoot copy: common) transformedBy: dsp)]
					ifFalse: [self unimplemented]]].
		^BeEdition make: resultRoot]]!
*/
}
/**
 * A new Edition with a RangeElement at a specified key. The old value, if there is one, is
 * superceded. Equivalent to
 * this->replace (theServer ()->makeEditionWith (key, value))
 */
public BeEdition with(Position key, BeCarrier value) {
	return replace((((BeGrandMap) CurrentGrandMap.fluidGet()).newEditionWith(key, value)));
/*
udanax-top.st:2577:BeEdition methodsFor: 'operations'!
{BeEdition} with: key {Position} with: value {BeCarrier}
	"A new Edition with a RangeElement at a specified key. The old value, if there is one, is superceded. Equivalent to
		this->replace (theServer ()->makeEditionWith (key, value))"
		
	^self replace: (CurrentGrandMap fluidGet newEditionWith: key with: value)!
*/
}
/**
 * A new Edition with a RangeElement at a specified set of keys. The old values, if there are
 * any, are superceded. Equivalent to
 * this->replace (theServer ()->makeEditionWithAll (keys, value))
 */
public BeEdition withAll(XnRegion keys, BeCarrier value) {
	return replace((((BeGrandMap) CurrentGrandMap.fluidGet()).newEditionWithAll(keys, value)));
/*
udanax-top.st:2583:BeEdition methodsFor: 'operations'!
{BeEdition} withAll: keys {XnRegion} with: value {BeCarrier}
	"A new Edition with a RangeElement at a specified set of keys. The old values, if there are any, are superceded. Equivalent to
		this->replace (theServer ()->makeEditionWithAll (keys, value))"
		
	^self replace: (CurrentGrandMap fluidGet newEditionWithAll: keys with: value)!
*/
}
/**
 * A new Edition without any RangeElement at a specified key. The old value, if there is one,
 * is removed. Equivalent to
 * this->copy (key->asRegion ()->complement ())
 */
public BeEdition without(Position key) {
	return copy(key.asRegion().complement());
/*
udanax-top.st:2589:BeEdition methodsFor: 'operations'!
{BeEdition} without: key {Position}
	"A new Edition without any RangeElement at a specified key. The old value, if there is one, is removed. Equivalent to
		this->copy (key->asRegion ()->complement ())"
		
	^self copy: key asRegion complement!
*/
}
/**
 * A new Edition without any RangeElements at the specified keys. The old values, if there
 * are any, are removed. Equivalent to
 * this->copy (keys->complement ())
 */
public BeEdition withoutAll(XnRegion keys) {
	return copy(keys.complement());
/*
udanax-top.st:2595:BeEdition methodsFor: 'operations'!
{BeEdition} withoutAll: keys {XnRegion}
	"A new Edition without any RangeElements at the specified keys. The old values, if there are any, are removed. Equivalent to
		this->copy (keys->complement ())"
		
	^self copy: keys complement!
*/
}
/**
 * The space from which the keys of this Edition are taken. Equivalent to
 * this->domain ()->coordinateSpace ()
 */
public CoordinateSpace coordinateSpace() {
	return myOrglRoot.coordinateSpace();
/*
udanax-top.st:2603:BeEdition methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	"The space from which the keys of this Edition are taken. Equivalent to
		this->domain ()->coordinateSpace ()"
		
	^myOrglRoot coordinateSpace!
*/
}
/**
 * The number of keys in this Edition. Blasts if infinite. Equivalent to
 * this->domain ()->count ()
 */
public int count() {
	return myOrglRoot.count();
/*
udanax-top.st:2609:BeEdition methodsFor: 'accessing'!
{IntegerVar} count
	"The number of keys in this Edition. Blasts if infinite. Equivalent to
		this->domain ()->count ()"
	
	^myOrglRoot count!
*/
}
/**
 * All the keys in this Edition. May be infinite, or empty.
 */
public XnRegion domain() {
	return myOrglRoot.domain();
/*
udanax-top.st:2615:BeEdition methodsFor: 'accessing'!
{XnRegion} domain
	"All the keys in this Edition. May be infinite, or empty."
	
	^myOrglRoot domain!
*/
}
/**
 * Create a front end representation for what is at the given key.
 */
public FeRangeElement fetch(Position key) {
	return myOrglRoot.fetch(key, this);
/*
udanax-top.st:2620:BeEdition methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position}
	"Create a front end representation for what is at the given key."
	
	^myOrglRoot fetch: key with: self!
*/
}
/**
 * The value at the given key, or blast if there is no such key (i.e. if !! this->domain
 * ()->hasMember (key)).
 */
public FeRangeElement get(Position key) {
	FeRangeElement result;
	result = fetch(key);
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return result;
/*
udanax-top.st:2625:BeEdition methodsFor: 'accessing'!
{FeRangeElement} get: key {Position}
	"The value at the given key, or blast if there is no such key (i.e. if !! this->domain ()->hasMember (key))."
	
	| result {FeRangeElement | NULL} |
	result _ self fetch: key.
	result == NULL ifTrue: [Heaper BLAST: #NotInTable].
	^result!
*/
}
/**
 * Whether the given key is in the Edition. Equivalent to
 * this->domain ()->hasMember (key)
 */
public boolean includesKey(Position key) {
	return (myOrglRoot.fetch(key, this)) != null;
/*
udanax-top.st:2633:BeEdition methodsFor: 'accessing'!
{BooleanVar} includesKey: key {Position}
	"Whether the given key is in the Edition. Equivalent to
		this->domain ()->hasMember (key)"
		
	^(myOrglRoot fetch: key with: self) ~~ NULL!
*/
}
/**
 * Whether there are any keys in this Edition. Equivalent to
 * this->domain ()->isEmpty ()
 */
public boolean isEmpty() {
	return myOrglRoot.isEmpty();
/*
udanax-top.st:2639:BeEdition methodsFor: 'accessing'!
{BooleanVar} isEmpty
	"Whether there are any keys in this Edition. Equivalent to
		this->domain ()->isEmpty ()"
	
	^myOrglRoot isEmpty!
*/
}
/**
 * Whether there is a finite number of keys in this Edition. Equivalent to
 * this->domain ()->isFinite ()
 */
public boolean isFinite() {
	return myOrglRoot.simpleDomain().isFinite() || (myOrglRoot.domain().isFinite());
/*
udanax-top.st:2645:BeEdition methodsFor: 'accessing'!
{BooleanVar} isFinite
	"Whether there is a finite number of keys in this Edition. Equivalent to
		this->domain ()->isFinite ()"
	
	^myOrglRoot simpleDomain isFinite or: [myOrglRoot domain isFinite]!
*/
}
public boolean isPurgeable() {
	return super.isPurgeable() && (myDetectors == null);
/*
udanax-top.st:2651:BeEdition methodsFor: 'accessing'!
{BooleanVar} isPurgeable
	^super isPurgeable and: [myDetectors == NULL]!
*/
}
public FeRangeElement makeFe(BeLabel label) {
	return FeEdition.on(this, (FeLabel.on(label)));
/*
udanax-top.st:2654:BeEdition methodsFor: 'accessing'!
{FeRangeElement} makeFe: label {BeLabel | NULL}
	^FeEdition on: self with: (FeLabel on: label)!
*/
}
/**
 * The owners of all the RangeElements in the given Region, or in the entire
 * Edition if no Region is specified.
 */
public IDRegion rangeOwners(XnRegion positions) {
	return (IDRegion) (myOrglRoot.rangeOwners(positions));
/*
udanax-top.st:2658:BeEdition methodsFor: 'accessing'!
{IDRegion} rangeOwners: positions {XnRegion default: NULL} 
	"The owners of all the RangeElements in the given Region, or in the entire 
	Edition if no Region is specified."
	^(myOrglRoot rangeOwners: positions) cast: IDRegion!
*/
}
/**
 * Essential.  This is the fundamental retrieval operation.  Return a stepper of bundles.
 * Each bundle is an association between a region in the domain and the range elements
 * associated with that region.  Where the region is associated with data, for instance, the
 * bundle contains a PrimArray of the data elements.
 * If no Region is given, then reads out the whole thing.
 */
public Stepper retrieve(XnRegion region, OrderSpec order, int flags) {
	XnRegion theRegion;
	OrderSpec theOrder;
	Accumulator result;
	Someone.thingToDo();
	/* The above comment is horribly insufficient. */
	Someone.thingToDo();
	/* This desperately needs to splay the region. */
	if (region == null) {
		theRegion = myOrglRoot.simpleDomain();
	}
	else {
		theRegion = region;
	}
	if (theRegion.isEmpty()) {
		return Stepper.emptyStepper();
	}
	if (order == null) {
		theOrder = theRegion.coordinateSpace().getAscending();
	}
	else {
		theOrder = order;
	}
	/* generate everything at once to avoid problems with the data structures changing as the client steps */
	result = Accumulator.ptrArray();
	Stepper stomper = (myOrglRoot.bundleStepper(theRegion, theOrder));
	for (; stomper.hasValue(); stomper.step()) {
		Heaper bundle = (Heaper) stomper.fetch();
		if (bundle == null) {
			continue ;
		}
		result.step(bundle);
	}
	stomper.destroy();
	return TableStepper.ascending(((PtrArray) result.value()));
/*
udanax-top.st:2664:BeEdition methodsFor: 'accessing'!
{(Stepper of: Bundle) CLIENT} retrieve: region {XnRegion default: NULL}
	with: order {OrderSpec default: NULL}
	with: flags {Int32 default: Int32Zero}
	"Essential.  This is the fundamental retrieval operation.  Return a stepper of bundles.  Each bundle is an association between a region in the domain and the range elements associated with that region.  Where the region is associated with data, for instance, the bundle contains a PrimArray of the data elements.
	If no Region is given, then reads out the whole thing."
	
	| theRegion {XnRegion} theOrder {OrderSpec} result {Accumulator} |
	self thingToDo.  "The above comment is horribly insufficient."
	self thingToDo.  "This desperately needs to splay the region."
	region == NULL
		ifTrue: [theRegion _ myOrglRoot simpleDomain]
		ifFalse: [theRegion _ region].
	theRegion isEmpty ifTrue: [^Stepper emptyStepper].
	order == NULL
		ifTrue: [theOrder := theRegion coordinateSpace getAscending]
		ifFalse: [theOrder := order].
	"generate everything at once to avoid problems with the data structures changing as the client steps"
	result := Accumulator ptrArray.
	(myOrglRoot bundleStepper: theRegion with: theOrder)
		forEach: [:bundle {Heaper} | result step: bundle].
	^TableStepper ascending: (result value cast: PtrArray)!
*/
}
/**
 * If this Edition has a single key, then the value at that key; if not, blasts. Equivalent
 * to
 * this->get (this->domain ()->theOne ())
 */
public FeRangeElement theOne() {
	return get(domain().theOne());
/*
udanax-top.st:2686:BeEdition methodsFor: 'accessing'!
{FeRangeElement} theOne
	"If this Edition has a single key, then the value at that key; if not, blasts. Equivalent to
		this->get (this->domain ()->theOne ())"
	
	^self get: self domain theOne!
*/
}
/**
 * All of the endorsements on this Edition and all Works which the CurrentKeyMaster can read.
 */
public CrossRegion visibleEndorsements() {
	XnRegion result;
	result = myOwnProp.endorsements();
	Stepper stomper = myWorks.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		BeWork work = (BeWork) stomper.fetch();
		if (work == null) {
			continue ;
		}
		if (work.canBeReadBy(((FeKeyMaster) CurrentKeyMaster.fluidGet()))) {
			result = result.unionWith(work.endorsements());
		}
	}
	stomper.destroy();
	return (CrossRegion) result;
/*
udanax-top.st:2692:BeEdition methodsFor: 'accessing'!
{CrossRegion} visibleEndorsements
	"All of the endorsements on this Edition and all Works which the CurrentKeyMaster can read."
	
	| result {XnRegion} |
	result := myOwnProp endorsements.
	myWorks stepper forEach: [ :work {BeWork} |
		(work canBeReadBy: CurrentKeyMaster fluidGet) ifTrue:
			[result := result unionWith: work endorsements]].
	^result cast: CrossRegion!
*/
}
/**
 * Adds to the endorsements on this Edition. The set of endorsements must be a finite number
 * of (club ID, token ID) pairs.
 */
public void endorse(CrossRegion endorsements) {
	if (endorsements.isEmpty()) {
		return ;
	}
	AboraBlockSupport.enterConsistent(8);
	try {
		propChange(PropChange.endorsementsChange(), (BertProp.endorsementsProp((endorsements.unionWith(myProp.endorsements())))));
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:2704:BeEdition methodsFor: 'props'!
{void} endorse: endorsements {CrossRegion}
	"Adds to the endorsements on this Edition. The set of endorsements must be a finite number of (club ID, token ID) pairs."
	
	endorsements isEmpty
		ifTrue: [^VOID].
	DiskManager consistent: 8 with:
		[self 
			propChange: PropChange endorsementsChange 
			with: (BertProp endorsementsProp: (endorsements unionWith: myProp endorsements))]!
*/
}
/**
 * All of the endorsements on this Edition.
 */
public CrossRegion endorsements() {
	return (CrossRegion) myOwnProp.endorsements();
/*
udanax-top.st:2714:BeEdition methodsFor: 'props'!
{CrossRegion} endorsements
	"All of the endorsements on this Edition."
	
	^myOwnProp endorsements cast: CrossRegion!
*/
}
public BertProp prop() {
	return myProp;
/*
udanax-top.st:2719:BeEdition methodsFor: 'props'!
{BertProp} prop
	^myProp!
*/
}
public void propChange(PropChange change, Prop nw) {
	Prop old;
	old = myOwnProp;
	if ( ! (change.areEqualProps(old, nw))) {
		AboraBlockSupport.enterConsistent(6);
		try {
			myOwnProp = (BertProp) (change.changed(old, nw));
			diskUpdate();
			propChanged(change, old, nw);
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
/*
udanax-top.st:2723:BeEdition methodsFor: 'props'!
{void} propChange: change {PropChange} with: nw {Prop} 
	
	| old {Prop} |
	old _ myOwnProp.
	(change areEqualProps: old with: nw) not
		ifTrue: 
			[DiskManager consistent: 6 with:
				[myOwnProp _ (change changed: old with: nw) cast: BertProp.
				self diskUpdate.
				self propChanged: change with: old with: nw]]!
*/
}
/**
 * update props
 */
public void propChanged(PropChange change, Prop old, Prop nw, PropFinder oldFinder) {
	Prop newProp;
	/* Attempt to apply the change directly to the current set of properties.
	 If that removes some property
	 			look at all the berts to see if we get it from somewhere else.  (BIG and not currently log.)
	 If the new properties are different than the old ones we must change, so
	 		remember the current props
	 		In a consistent block
	 			change the props on the stamp
	 			change leaf of bert canopy and create an AgendaItem to propagate the chage through bert canopy
	 			fetch a finder to look for recorders rung by this change in props
	 			See if permissions decrease:
	 				If so, recorders can't be rung.  Don't bother with sensor canopy, just schedule bert canopy propagation.
	 				If not
	 					make an AgendaItem to check for recorders in the sensor canopy
	 					make and schedule a Sequencer to do the bert then the sensor canopy AgendaItems. */
	newProp = change.changed(myProp, myOwnProp);
	newProp = change.with(newProp, nw);
	if ( ! (change.areEqualProps(newProp, (change.with(newProp, old))))) {
		Stepper stomper = myWorks.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			BeWork work = (BeWork) stomper.fetch();
			if (work == null) {
				continue ;
			}
			Someone.thingToDo();
			/* Make it log. */
			newProp = change.with(newProp, work.localProp());
		}
		stomper.destroy();
	}
	if ( ! (change.areEqualProps(myProp, newProp))) {
		BertProp before;
		PropFinder finder;
		AgendaItem changer;
		AgendaItem checker;
		before = myProp;
		AboraBlockSupport.enterConsistent(9);
		try {
			myProp = ((BertProp) newProp);
			diskUpdate();
			changer = myOrglRoot.propChanger(change);
			finder = change.fetchFinder(before, myProp, this, oldFinder);
			if (finder == null) {
				changer.schedule();
			}
			else {
				checker = SouthRecorderChecker.make(myOrglRoot, finder, ((SensorCrum) myOrglRoot.sensorCrum().fetchParent()));
				if (oldFinder == null) {
					(Sequencer.make(changer, checker)).schedule();
				}
				else {
					AgendaItem workChecker;
					workChecker = NorthRecorderChecker.make(this, finder);
					/* the sequence of workChecker vs checker doesn't matter */
					(Sequencer.make(changer, (Sequencer.make(workChecker, checker)))).schedule();
				}
			}
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
/*
udanax-top.st:2734:BeEdition methodsFor: 'props'!
{void} propChanged: change {PropChange}
	with: old {Prop}
	with: nw {Prop}
	with: oldFinder {PropFinder default: NULL}
	"update props"
	
	| newProp {Prop} |
	
	"Attempt to apply the change directly to the current set of properties.
	 If that removes some property
	 			look at all the berts to see if we get it from somewhere else.  (BIG and not currently log.)
	 If the new properties are different than the old ones we must change, so
	 		remember the current props
	 		In a consistent block
	 			change the props on the stamp
	 			change leaf of bert canopy and create an AgendaItem to propagate the chage through bert canopy
	 			fetch a finder to look for recorders rung by this change in props
	 			See if permissions decrease:
	 				If so, recorders can't be rung.  Don't bother with sensor canopy, just schedule bert canopy propagation.
	 				If not
	 					make an AgendaItem to check for recorders in the sensor canopy
	 					make and schedule a Sequencer to do the bert then the sensor canopy AgendaItems."
	
	newProp _ change changed: myProp with: myOwnProp.
	newProp _ change with: newProp with: nw.
	(change areEqualProps: newProp with: (change with: newProp with: old)) not
		ifTrue: [myWorks stepper forEach: [:work {BeWork} | 
				self thingToDo.  "Make it log."
				newProp _ change with: newProp with: work localProp]].
	(change areEqualProps: myProp with: newProp)
		ifFalse: 
			[| before {BertProp} finder {PropFinder} 
			   changer {AgendaItem} checker {AgendaItem} |
			before _ myProp.
			DiskManager consistent: 9 with:
				[myProp _ (newProp cast: BertProp).
				self diskUpdate.
				changer _ myOrglRoot propChanger: change.
				finder _ change fetchFinder: before with: myProp with: self with: oldFinder.
				finder == NULL
					ifTrue: [changer schedule]
					ifFalse:
						[checker _ SouthRecorderChecker make: myOrglRoot 
								with: finder 
								with: (myOrglRoot sensorCrum fetchParent cast: SensorCrum).
						oldFinder == NULL ifTrue:
							[(Sequencer make: changer with: checker) schedule]
						ifFalse:
							[ | workChecker {AgendaItem} |
							workChecker := NorthRecorderChecker make: self with: finder.
							"the sequence of workChecker vs checker doesn't matter"
							(Sequencer make: changer
								with: (Sequencer make: workChecker with: checker)) schedule]]]]!
*/
}
/**
 * Removes endorsements from this Edition. Ignores all endorsements which you could have
 * removed, but which don't happen to be there right now.
 */
public void retract(CrossRegion endorsements) {
	if (endorsements.isEmpty()) {
		return ;
	}
	AboraBlockSupport.enterConsistent(4);
	try {
		propChange(PropChange.endorsementsChange(), (BertProp.endorsementsProp((myOwnProp.endorsements().minus(endorsements)))));
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:2788:BeEdition methodsFor: 'props'!
{void} retract: endorsements {CrossRegion}
	"Removes endorsements from this Edition. Ignores all endorsements which you could have removed, but which don't happen to be there right now."
	
	endorsements isEmpty
		ifTrue: [^VOID].
	DiskManager consistent: 4 with:
		[self 
			propChange: PropChange endorsementsChange 
			with: (BertProp endorsementsProp: (myOwnProp endorsements minus: endorsements))]!
*/
}
/**
 * All of the endorsements on this Edition and all Works directly on it
 */
public CrossRegion totalEndorsements() {
	XnRegion result;
	result = myOwnProp.endorsements();
	Stepper stomper = myWorks.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		BeWork work = (BeWork) stomper.fetch();
		if (work == null) {
			continue ;
		}
		result = result.unionWith(work.endorsements());
	}
	stomper.destroy();
	return (CrossRegion) result;
/*
udanax-top.st:2798:BeEdition methodsFor: 'props'!
{CrossRegion} totalEndorsements
	"All of the endorsements on this Edition and all Works directly on it"
	
	| result {XnRegion} |
	result := myOwnProp endorsements.
	myWorks stepper forEach: [ :work {BeWork} |
		result := result unionWith: work endorsements].
	^result cast: CrossRegion!
*/
}
/**
 * Add a detector which will be triggered with a FeEdition when a PlaceHolder becomes a
 * non-PlaceHolder
 */
public void addDetector(FeFillRangeDetector detect) {
	if (myDetectors == null) {
		myDetectors = PrimSet.weak(7, (BeEditionDetectorExecutor.make(this)));
		propChange(PropChange.detectorWaitingChange(), BertProp.detectorWaitingProp());
	}
	myDetectors.introduce(detect);
	myOrglRoot.triggerDetector(detect);
/*
udanax-top.st:2809:BeEdition methodsFor: 'becoming'!
{void} addDetector: detect {FeFillRangeDetector}
	"Add a detector which will be triggered with a FeEdition when a PlaceHolder becomes a non-PlaceHolder"
	
	myDetectors == NULL ifTrue:
		[myDetectors := PrimSet weak: 7 with: (BeEditionDetectorExecutor make: self).
		self propChange: PropChange detectorWaitingChange
			with: BertProp detectorWaitingProp].
	myDetectors introduce: detect.
	myOrglRoot triggerDetector: detect.!
*/
}
/**
 * Return the owner for the given position in the receiver.
 */
public ID ownerAt(Position key) {
	return myOrglRoot.ownerAt(key);
/*
udanax-top.st:2819:BeEdition methodsFor: 'becoming'!
{ID} ownerAt: key {Position}
	"Return the owner for the given position in the receiver."
		
	^myOrglRoot ownerAt: key!
*/
}
/**
 * Remove a previously added detector
 */
public void removeDetector(FeFillRangeDetector detect) {
	if (Heaper.isDestructed(myDetectors)) {
		return ;
	}
	if (myDetectors == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NEVER_ADDED_DETECTOR);
	}
	Ravi.knownBug();
	/* if we're in GC, we may be dealing with a partially unconstructed web of objects */
	myDetectors.remove(detect);
	if (myDetectors.isEmpty()) {
		myDetectors = null;
		propChange(PropChange.detectorWaitingChange(), BertProp.make());
	}
/*
udanax-top.st:2824:BeEdition methodsFor: 'becoming'!
{void} removeDetector: detect {FeFillRangeDetector}
	"Remove a previously added detector"
	
	(Heaper isDestructed: myDetectors) ifTrue:
		[^VOID].
	myDetectors == NULL ifTrue:
		[Heaper BLAST: #NeverAddedDetector].
	Ravi knownBug. "if we're in GC, we may be dealing with a partially unconstructed web of objects"
	myDetectors remove: detect.
	myDetectors isEmpty ifTrue:
		[myDetectors := NULL.
		self propChange: PropChange detectorWaitingChange
			with: BertProp make]!
*/
}
/**
 * Notify the edition that there are no remaining detectors on it.
 */
public void removeLastDetector() {
	myDetectors = null;
	propChange(PropChange.detectorWaitingChange(), BertProp.make());
/*
udanax-top.st:2838:BeEdition methodsFor: 'becoming'!
{void} removeLastDetector
	"Notify the edition that there are no remaining detectors on it."
	
	myDetectors := NULL.
	self propChange: PropChange detectorWaitingChange with: BertProp make!
*/
}
/**
 * Ring all my detectors with the given Edition as an argument
 */
public void ringDetectors(FeEdition newIdentities) {
	if (myDetectors != null) {
		Stepper stomper = myDetectors.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			FeFillRangeDetector det = (FeFillRangeDetector) stomper.fetch();
			if (det == null) {
				continue ;
			}
			det.rangeFilled(newIdentities);
		}
		stomper.destroy();
	}
/*
udanax-top.st:2844:BeEdition methodsFor: 'becoming'!
{void} ringDetectors: newIdentities {FeEdition}
	"Ring all my detectors with the given Edition as an argument"
	
	myDetectors ~~ NULL ifTrue:
		[myDetectors stepper forEach: [ :det {FeFillRangeDetector} |
			det rangeFilled: newIdentities]]!
*/
}
/**
 * Changes the owner of all RangeElements; requires the authority of the current owner.
 * Returns the subset of this Edition whose owners did not get changed because of lack of
 * authority.
 */
public BeEdition setRangeOwners(ID newOwner, XnRegion region) {
	Someone.knownBug();
	/* Must be a loop in ServerLoop. */
	Someone.thingToDo();
	/* propagate region down through the algorithm? */
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, hCrum().hCut().newSuccessor());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
		try {
			return BeEdition.make(((myOrglRoot.copy(region)).setAllOwners(newOwner)));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:2851:BeEdition methodsFor: 'becoming'!
{BeEdition} setRangeOwners: newOwner {ID} with: region {XnRegion}
	"Changes the owner of all RangeElements; requires the authority of the current owner.
	Returns the subset of this Edition whose owners did not get changed because of lack of authority."
	
	self knownBug.  "Must be a loop in ServerLoop."
	self thingToDo. "propagate region down through the algorithm?"
	CurrentTrace fluidBind: self hCrum hCut newSuccessor 
		during: [CurrentBertCrum fluidBind: BertCrum make
		during: [^BeEdition make: ((myOrglRoot copy: region) setAllOwners: newOwner)]]!
*/
}
/**
 * Change the identities of the RangeElements of this Edition to those at the same key in the
 * other Edition. The left piece of the result contains those object which are know to not be
 * able to become, because of
 * - lack of ownership authority
 * - different contents
 * - incompatible types
 * - no corresponding new identity
 * The right piece of the result is NULL if there is nothing more that might be done, or else
 * the remainder of the receiver on which we might be able to proceed. This material might
 * fail at a later time because of any of the reasons above; or it might succeed , even
 * though it failed this time because of
 * - synchronization problem
 * - just didn't feel like it
 * This is always required to make progress if it can, although it isn't required to make all
 * the progress that it might. Returns right=NULL when it can't make further progress.
 */
public Pair tryAllBecome(BeEdition newIdentities) {
	Dean.shouldImplement();
	return null;
/*
udanax-top.st:2861:BeEdition methodsFor: 'becoming'!
{Pair of: BeEdition} tryAllBecome: newIdentities {BeEdition}
	"Change the identities of the RangeElements of this Edition to those at the same key in the other Edition. The left piece of the result contains those object which are know to not be able to become, because of
		- lack of ownership authority
		- different contents
		- incompatible types
		- no corresponding new identity
	The right piece of the result is NULL if there is nothing more that might be done, or else the remainder of the receiver on which we might be able to proceed. This material might fail at a later time because of any of the reasons above; or it might succeed , even though it failed this time because of
		- synchronization problem
		- just didn't feel like it
	This is always required to make progress if it can, although it isn't required to make all the progress that it might. Returns right=NULL when it can't make further progress."
	
	Dean shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * The keys in this Edition at which there are Editions with the given label.
 */
public XnRegion keysLabelled(BeLabel label) {
	return myOrglRoot.keysLabelled(label);
/*
udanax-top.st:2878:BeEdition methodsFor: 'labelling'!
{XnRegion} keysLabelled: label {BeLabel}
	"The keys in this Edition at which there are Editions with the given label."
	
	^myOrglRoot keysLabelled: label!
*/
}
/**
 * Replace the Edition at the given key, leaving the Label the same. Equivalent to
 * this->store (key, edition->labelled (CAST(FeEdition,this->get (key))->label ()))
 */
public BeEdition rebind(Position key, BeEdition edition) {
	mightNotImplement();
	return null;
/*
udanax-top.st:2883:BeEdition methodsFor: 'labelling'!
{BeEdition} rebind: key {Position} with: edition {BeEdition}
	"Replace the Edition at the given key, leaving the Label the same. Equivalent to
		this->store (key, edition->labelled (CAST(FeEdition,this->get (key))->label ()))"
	self mightNotImplement.
	^NULL "fodder"!
*/
}
public void restartE(Rcvr rcvr) {
	myDetectors = null;
/*
udanax-top.st:2891:BeEdition methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartE: rcvr {Rcvr unused}
	myDetectors _ NULL!
*/
}
public OrglRoot orglRoot() {
	return myOrglRoot;
/*
udanax-top.st:2896:BeEdition methodsFor: 'protected:'!
{OrglRoot} orglRoot
	^myOrglRoot!
*/
}
/**
 * add oparent to the set of upward pointers.  Editions may
 * also have to propagate BertCrum change downward.
 */
public void addOParent(Loaf oparent) {
	BertCrum bCrum;
	BertCrum newBCrum;
	bCrum = hCrum().bertCrum();
	super.addOParent(oparent);
	newBCrum = hCrum().bertCrum();
	if ( ! (bCrum.isLE(newBCrum))) {
		myOrglRoot.updateBCrumTo(newBCrum);
	}
/*
udanax-top.st:2901:BeEdition methodsFor: 'be accessing'!
{void} addOParent: oparent {Loaf} 
	"add oparent to the set of upward pointers.  Editions may
	 also have to propagate BertCrum change downward."
	| bCrum {BertCrum} newBCrum {BertCrum} |
	[HistoryCrum] USES.
	bCrum _ self hCrum bertCrum.
	super addOParent: oparent.
	newBCrum _ self hCrum bertCrum.
	(bCrum isLE: newBCrum) 
		ifFalse: [myOrglRoot updateBCrumTo: newBCrum]!
*/
}
public boolean anyPasses(PropFinder finder) {
	PropFinder next;
	next = finder.findPast(this);
	return next.isFull() || (super.anyPasses(next));
/*
udanax-top.st:2913:BeEdition methodsFor: 'be accessing'!
{BooleanVar} anyPasses: finder {PropFinder}
	| next {PropFinder} |
	next := finder findPast: self.
	^next isFull or: [super anyPasses: next]!
*/
}
public void checkRecorders(PropFinder finder, SensorCrum scrum) {
	PropFinder newFinder;
	/* Get a new finder which remembers to check if recorders will newly find me */
	newFinder = finder.findPast(this);
	/* replace endorsements with those in the prop */
	if ( ! (newFinder.isEmpty())) {
		/* keep looking down, with my stamp as the new reference point */
		Someone.thingToDo();
		/* Use the new finder to check all recorders beneath me, checking whether they record all stamps from me all the way up to the stamp passed in as an argument */
		Ravi.knownBug();
		/* using scrum's parent records things twice */
		(SouthRecorderChecker.make(myOrglRoot, newFinder, ((SensorCrum) scrum.fetchParent()))).schedule();
	}
/*
udanax-top.st:2919:BeEdition methodsFor: 'be accessing'!
{void} checkRecorders: finder {PropFinder} 
	with: scrum {SensorCrum | NULL} 
	
	| newFinder {PropFinder} |
	"Get a new finder which remembers to check if recorders will newly find me"
	newFinder _ finder findPast: self.
	"replace endorsements with those in the prop"
	newFinder isEmpty
		ifFalse: 
			["keep looking down, with my stamp as the new reference point"
			self thingToDo.
			"Use the new finder to check all recorders beneath me, checking whether they record all stamps from me all the way up to the stamp passed in as an argument"
			Ravi knownBug. "using scrum's parent records things twice"
			(SouthRecorderChecker
				make: myOrglRoot
				with: newFinder
				with: (scrum fetchParent cast: SensorCrum)) schedule]!
*/
}
/**
 * The Works currently on this Edition
 */
public ImmuSet currentWorks() {
	return myWorks.asImmuSet();
/*
udanax-top.st:2937:BeEdition methodsFor: 'be accessing'!
{ImmuSet of: BeWork} currentWorks
	"The Works currently on this Edition"
	
	^myWorks asImmuSet!
*/
}
/**
 * An actual, non-virtual FE range element at that key. Used by become operation to get
 * something to pass into BeRangeElement::become ()
 */
public BeRangeElement getOrMakeBe(Position key) {
	return myOrglRoot.getBe(key);
/*
udanax-top.st:2942:BeEdition methodsFor: 'be accessing'!
{BeRangeElement} getOrMakeBe: key {Position}
	"An actual, non-virtual FE range element at that key. Used by become operation to get something to pass into BeRangeElement::become ()"
	
	^myOrglRoot getBe: key!
*/
}
/**
 * A Work has been newly revised to point at me.
 */
public void introduceWork(BeWork work) {
	AboraBlockSupport.enterConsistent();
	try {
		myWorks.introduce(work);
		diskUpdate();
		propChanged(PropChange.bertPropChange(), BertProp.make(), work.prop(), (PropChange.bertPropChange().fetchFinder(BertProp.make(), work.prop(), work, null)));
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	if (myWorks.count() >= 100 && ( ! (myWorks instanceof GrandHashSet))) {
		MuSet newWorks;
		newWorks = GrandHashSet.make();
		Stepper stomper = myWorks.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			BeWork b = (BeWork) stomper.fetch();
			if (b == null) {
				continue ;
			}
			newWorks.store(b);
		}
		stomper.destroy();
		AboraBlockSupport.enterConsistent(1);
		try {
			myWorks = newWorks;
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
/*
udanax-top.st:2947:BeEdition methodsFor: 'be accessing'!
{void} introduceWork: work {BeWork} 
	"A Work has been newly revised to point at me."
	DiskManager consistent: 
		[myWorks introduce: work.
		self diskUpdate.
		self propChanged: PropChange bertPropChange
			with: BertProp make
			with: work prop
			with: (PropChange bertPropChange
				fetchFinder: BertProp make
				with: work prop
				with: work
				with: NULL)].
	(myWorks count >= 100 and: [(myWorks isKindOf: GrandHashSet) not]) ifTrue: 
		[| newWorks {MuSet} |
		newWorks _ GrandHashSet make.
		myWorks stepper forEach: [:b {BeWork} | newWorks store: b].
		DiskManager consistent: 1 with:
			[myWorks _ newWorks.
			self diskUpdate]].!
*/
}
/**
 * The Work is no longer onto this Edition.  Remove the backpointer.
 */
public void removeWork(BeWork work) {
	AboraBlockSupport.enterConsistent();
	try {
		myWorks.remove(work);
		diskUpdate();
		propChanged(PropChange.bertPropChange(), work.prop(), BertProp.make());
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:2969:BeEdition methodsFor: 'be accessing'!
{void} removeWork: work {BeWork} 
	"The Work is no longer onto this Edition.  Remove the backpointer."
	DiskManager consistent:
		[myWorks remove: work.
		self diskUpdate.
		self propChanged: PropChange bertPropChange
			with: work prop
			with: BertProp make]!
*/
}
/**
 * My bertCrum must not be leafward of newBCrum.
 * Thus it must be LE to newCrum. Otherwise correct it and recur.
 */
public boolean updateBCrumTo(BertCrum newBCrum) {
	if (super.updateBCrumTo(newBCrum)) {
		myOrglRoot.updateBCrumTo(newBCrum);
		return true;
	}
	return false;
/*
udanax-top.st:2979:BeEdition methodsFor: 'be accessing'!
{BooleanVar} updateBCrumTo: newBCrum {BertCrum} 
	"My bertCrum must not be leafward of newBCrum. 
	Thus it must be LE to newCrum. Otherwise correct it and recur."
	(super updateBCrumTo: newBCrum) ifTrue: 
		[myOrglRoot updateBCrumTo: newBCrum.
		^true].
	^false!
*/
}
/**
 * All of the keys in this Edition at which the given RangeElement can be found. Equivalent
 * to
 * this->sharedRegion (theServer ()->makeEditionWith (some position, value))
 */
public XnRegion keysOf(FeRangeElement value) {
	return sharedRegion((((BeGrandMap) CurrentGrandMap.fluidGet()).newEditionWith(IntegerPos.zero(), value.carrier())));
/*
udanax-top.st:2990:BeEdition methodsFor: 'comparing'!
{XnRegion} keysOf: value {FeRangeElement}
	"All of the keys in this Edition at which the given RangeElement can be found. Equivalent to
		this->sharedRegion (theServer ()->makeEditionWith (some position, value))"
	
	[BeGrandMap] USES.
	^self sharedRegion: (CurrentGrandMap fluidGet
			newEditionWith: IntegerPos zero with: value carrier)!
*/
}
/**
 * A Mapping from each of the keys in this Edition to all of the keys in the other Edition
 * which have the same RangeElement.
 */
public Mapping mapSharedTo(BeEdition other) {
	return myOrglRoot.mapSharedTo(other.hCrum().hCut());
/*
udanax-top.st:2998:BeEdition methodsFor: 'comparing'!
{Mapping} mapSharedTo: other {BeEdition}
	"A Mapping from each of the keys in this Edition to all of the keys in the other Edition which have the same RangeElement."
	
	^myOrglRoot mapSharedTo: other hCrum hCut!
*/
}
/**
 * The subset of this Edition whose RangeElements are not in the other Edition. Equivalent to
 * this->copy (this->sharedRegion (other, flags)->complement ())
 */
public BeEdition notSharedWith(BeEdition other, int flags) {
	return copy((sharedRegion(other, flags)).complement());
/*
udanax-top.st:3003:BeEdition methodsFor: 'comparing'!
{BeEdition} notSharedWith: other {BeEdition}
	with: flags {Int32 default: Int32Zero}
	"The subset of this Edition whose RangeElements are not in the other Edition. Equivalent to
		this->copy (this->sharedRegion (other, flags)->complement ())"	
	
	^self copy: (self sharedRegion: other with: flags) complement!
*/
}
/**
 * The subset of the keys of this Edition which  have RangeElements that are in the other
 * Edition. If both flags are false, then equivalent to
 * this->mapSharedTo (other)->domain ()
 * If nestThis, then returns not only keys of RangeElements which are in the other, but also
 * keys of Editions which lead to RangeElements which are in the other.
 * If nestOther, then looks not only for RangeElements which are values of the other Edition,
 * but also those which are values of sub-Editions of the other Edition. (This option will
 * probably not be supported in version 1.0)
 */
public XnRegion sharedRegion(BeEdition other, int flags) {
	if (flags != 0) {
		throw new UnimplementedException();
	}
	return myOrglRoot.sharedRegion(other.hCrum().hCut());
/*
udanax-top.st:3010:BeEdition methodsFor: 'comparing'!
{XnRegion} sharedRegion: other {BeEdition}
	with: flags {Int32 default: Int32Zero}
	"The subset of the keys of this Edition which  have RangeElements that are in the other Edition. If both flags are false, then equivalent to
		this->mapSharedTo (other)->domain ()
	If nestThis, then returns not only keys of RangeElements which are in the other, but also keys of Editions which lead to RangeElements which are in the other.
	If nestOther, then looks not only for RangeElements which are values of the other Edition, but also those which are values of sub-Editions of the other Edition. (This option will probably not be supported in version 1.0)"
	
	flags ~= Int32Zero ifTrue: [self unimplemented].
	^myOrglRoot sharedRegion: other hCrum hCut!
*/
}
/**
 * The subset of this Edition whose RangeElements are in the other Edition. If the same
 * RangeElement is in this Edition at several different keys, all keys will be in the result
 * (provided the RangeElement is also in the other Edition). Equivalent to
 * this->copy (this->sharedRegion (other, flags))
 */
public BeEdition sharedWith(BeEdition other, int flags) {
	return copy((sharedRegion(other, flags)));
/*
udanax-top.st:3020:BeEdition methodsFor: 'comparing'!
{BeEdition} sharedWith: other {BeEdition}
	with: flags {Int32 default: Int32Zero}
	"The subset of this Edition whose RangeElements are in the other Edition. If the same RangeElement is in this Edition at several different keys, all keys will be in the result (provided the RangeElement is also in the other Edition). Equivalent to
		this->copy (this->sharedRegion (other, flags))"
	
	^self copy: (self sharedRegion: other with: flags)!
*/
}
public BeEdition works(IDRegion permissions, Filter endorsementsFilter, int flags) {
	Accumulator result;
	IDSpace iDSpace;
	XnRegion region;
	if ( ! (flags == (FeEdition.LOCALUPRESENTUONLY() | FeEdition.DIRECTUCONTAINERSUONLY()))) {
		return super.works(permissions, endorsementsFilter, flags);
	}
	result = Accumulator.ptrArray();
	Stepper stomper = myWorks.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		BeWork work = (BeWork) stomper.fetch();
		if (work == null) {
			continue ;
		}
		if (endorsementsFilter.match(work.endorsements())) {
			result.step((work.makeFe(null)));
		}
	}
	stomper.destroy();
	iDSpace = ((BeGrandMap) CurrentGrandMap.fluidGet()).newIDSpace();
	region = (iDSpace.newIDs((((PtrArray) result.value()).count())));
	return (((BeGrandMap) CurrentGrandMap.fluidGet()).newPlaceHolders(region.complement())).combine((((BeGrandMap) CurrentGrandMap.fluidGet()).newValueEdition(((PtrArray) result.value()), region, iDSpace.ascending())));
/*
udanax-top.st:3027:BeEdition methodsFor: 'comparing'!
{BeEdition} works: permissions {IDRegion}
	with: endorsementsFilter {Filter}
	with: flags {Int32}
	| result {Accumulator} iDSpace {IDSpace} region {XnRegion} |
	flags = (FeEdition LOCAL.U.PRESENT.U.ONLY bitOr: FeEdition DIRECT.U.CONTAINERS.U.ONLY) ifFalse:
		[^super works: permissions with: endorsementsFilter with: flags].
	result := Accumulator ptrArray.
	myWorks stepper forEach: [ :work {BeWork} |
		(endorsementsFilter match: work endorsements) ifTrue:
			[result step: (work makeFe: NULL)]].
	iDSpace := CurrentGrandMap fluidGet newIDSpace.
	region := (iDSpace newIDs: ((result value cast: PtrArray) count)).
	^(CurrentGrandMap fluidGet newPlaceHolders: region complement)
		combine:(CurrentGrandMap fluidGet
			newValueEdition: (result value cast: PtrArray)
			with: region
			with: iDSpace ascending)!
*/
}
public BeEdition(OrglRoot root) {
	super(root.sensorCrum());
	Dean.knownBug();
	/* this should not have the same SensorCrum as my OrglRoot */
	myOrglRoot = root;
	myWorks = MuSet.make();
	/* This should maybe just start out NULL. */
	myOwnProp = myProp = BertProp.make();
	myDetectors = null;
	AboraBlockSupport.enterConsistent(5);
	try {
		myOrglRoot.introduceEdition(this);
		newShepherd();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:3048:BeEdition methodsFor: 'creation'!
create: root {OrglRoot}
	super create: root sensorCrum.
	
	Dean knownBug. "this should not have the same SensorCrum as my OrglRoot"
	myOrglRoot _ root.
	myWorks _ MuSet make.  "This should maybe just start out NULL."
	myOwnProp _ myProp _ BertProp make.
	myDetectors _ NULL.
	DiskManager consistent: 5 with:
		[myOrglRoot introduceEdition: self.
		self newShepherd]!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(
	/* 2 with: (need to recalculate for adding propChange) */
	);
	try {
		propChange(PropChange.bertPropChange(), BertProp.make());
		if (Heaper.isConstructed(myOrglRoot)) {
			myOrglRoot.removeEdition(this);
		}
		myOrglRoot = null;
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:3060:BeEdition methodsFor: 'creation'!
{void} dismantle
	DiskManager consistent: "2 with: (need to recalculate for adding propChange)"
		[self propChange: PropChange bertPropChange with: BertProp make.
		(Heaper isConstructed: myOrglRoot) 
			ifTrue: [myOrglRoot removeEdition: self].
		myOrglRoot _ NULL.
		super dismantle]!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myOrglRoot);
	oo.print(")");
/*
udanax-top.st:3070:BeEdition methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myOrglRoot << ')'!
*/
}
/**
 * Attach the TrailBlazer to this Edition, and return the region of partiality it is attached
 * to
 */
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	return myOrglRoot.attachTrailBlazer(blazer);
/*
udanax-top.st:3075:BeEdition methodsFor: 'transclusions'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	"Attach the TrailBlazer to this Edition, and return the region of partiality it is attached to"
	
	^myOrglRoot attachTrailBlazer: blazer!
*/
}
public void fossilRelease(RecorderFossil oldGrabber) {
	MarkM.thingToDo();
	/* myGrabbersFossil == NULL ifTrue:
		[Heaper BLAST: #NotGrabbed]
	ifFalse: [myGrabbersFossil ~~ oldGrabber ifTrue:
		[Heaper BLAST: #WhoIsReleasingMe]
	ifFalse:
		[DiskManager consistent: 2 with:
			[myGrabbersFossil := NULL.
			oldGrabber extinguish: self.
			self diskUpdate]]] */
/*
udanax-top.st:3080:BeEdition methodsFor: 'transclusions'!
{void} fossilRelease: oldGrabber {RecorderFossil}
	MarkM thingToDo.
	"myGrabbersFossil == NULL ifTrue:
		[Heaper BLAST: #NotGrabbed]
	ifFalse: [myGrabbersFossil ~~ oldGrabber ifTrue:
		[Heaper BLAST: #WhoIsReleasingMe]
	ifFalse:
		[DiskManager consistent: 2 with:
			[myGrabbersFossil := NULL.
			oldGrabber extinguish: self.
			self diskUpdate]]]"!
*/
}
/**
 * Get or make a TrailBlazer for recording results into this Edition. Blast if there is
 * already more than one
 */
public TrailBlazer getOrMakeTrailBlazer() {
	TrailBlazer result;
	result = myOrglRoot.fetchTrailBlazer();
	if (result == null) {
		return TrailBlazer.make(this);
	}
	myOrglRoot.checkTrailBlazer(result);
	return result;
/*
udanax-top.st:3093:BeEdition methodsFor: 'transclusions'!
{TrailBlazer} getOrMakeTrailBlazer
	"Get or make a TrailBlazer for recording results into this Edition. Blast if there is already more than one"
	
	| result {TrailBlazer} |
	result := myOrglRoot fetchTrailBlazer.
	result == NULL
		ifTrue: [^TrailBlazer make: self].
	myOrglRoot checkTrailBlazer: result.
	^result!
*/
}
/**
 * See FeEdition
 */
public BeEdition rangeTranscluders(XnRegion region, Filter directFilter, Filter indirectFilter, int flags, BeEdition otherTrail) {
	RecorderFossil fossil;
	BeEdition result;
	/* Reject all the unimplemented cases.
	
	if a trail isn't given
		make a new one
	else
		use it as the result.
		
	Make a fossilized recorder 
		snapshotting the current login authority
		filtered by the endorsementsFilter
		for recording into the trail
	Set the transclusions request in motion
	Return the trail */
	if ((flags & ~ (FeEdition.DIRECTUCONTAINERSUONLY() | FeEdition.LOCALUPRESENTUONLY())) != 0) {
		throw new UnimplementedException();
	}
	if (otherTrail == null) {
		result = ((BeGrandMap) CurrentGrandMap.fluidGet()).newPlaceHolders(((BeGrandMap) CurrentGrandMap.fluidGet()).newIDSpace().fullRegion());
	}
	else {
		result = otherTrail;
	}
	fossil = RecorderFossil.transcluders((flags & FeEdition.DIRECTUCONTAINERSUONLY()) != 0, ((FeKeyMaster) CurrentKeyMaster.fluidFetch()).loginAuthority(), directFilter, indirectFilter, result.getOrMakeTrailBlazer());
	if ((flags & FeEdition.LOCALUPRESENTUONLY()) != 0) {
		scheduleImmediateBackfollow(fossil, region);
	}
	else {
		if ((flags & FeEdition.DIRECTUCONTAINERSUONLY()) != 0) {
			throw new UnimplementedException();
		}
		scheduleDelayedBackfollow(fossil, region);
	}
	return result;
/*
udanax-top.st:3103:BeEdition methodsFor: 'transclusions'!
{BeEdition} rangeTranscluders:  region {XnRegion | NULL}
	with: directFilter {Filter}
	with: indirectFilter {Filter}
	with: flags {Int32}
	with: otherTrail {BeEdition | NULL}
	"See FeEdition"
	
	| fossil {RecorderFossil} 
	  result {BeEdition} |
	
	"Reject all the unimplemented cases.
	
	if a trail isn't given
		make a new one
	else
		use it as the result.
		
	Make a fossilized recorder 
		snapshotting the current login authority
		filtered by the endorsementsFilter
		for recording into the trail
	Set the transclusions request in motion
	Return the trail"  
	  
	(flags bitAnd: (FeEdition DIRECT.U.CONTAINERS.U.ONLY
			bitOr: FeEdition LOCAL.U.PRESENT.U.ONLY) bitInvert) ~~ Int32Zero
		ifTrue: [self unimplemented].
	
	otherTrail == NULL 
		ifTrue: [result := CurrentGrandMap fluidGet 
				newPlaceHolders: CurrentGrandMap fluidGet newIDSpace fullRegion]
		ifFalse: [result := otherTrail].
		
	fossil := RecorderFossil 
		transcluders: (flags bitAnd: FeEdition DIRECT.U.CONTAINERS.U.ONLY) ~~ Int32Zero
		with: CurrentKeyMaster fluidFetch loginAuthority 
		with: directFilter
		with: indirectFilter
		with: result getOrMakeTrailBlazer.
	
	(flags bitAnd: FeEdition LOCAL.U.PRESENT.U.ONLY) ~~ Int32Zero
		ifTrue: [self scheduleImmediateBackfollow: fossil with: region]
		ifFalse: [(flags bitAnd: FeEdition DIRECT.U.CONTAINERS.U.ONLY) ~~ Int32Zero
				ifTrue: [self unimplemented].
			self scheduleDelayedBackfollow: fossil with: region].
	^result!
*/
}
/**
 * See FeEdition
 */
public BeEdition rangeWorks(XnRegion region, Filter filter, int flags, BeEdition otherTrail) {
	RecorderFossil fossil;
	BeEdition result;
	/* Reject all the unimplemented cases.
	
	if a trail isn't given
		make a new one
	else
		use it as the result.
		
	Make a fossilized recorder 
		snapshotting the current login authority
		filtered by the endorsementsFilter
		for recording into the trail
	Set the transclusions request in motion
	Return the trail */
	if ((flags & ~ (FeEdition.DIRECTUCONTAINERSUONLY() | FeEdition.LOCALUPRESENTUONLY())) != 0) {
		throw new UnimplementedException();
	}
	if (otherTrail == null) {
		result = ((BeGrandMap) CurrentGrandMap.fluidGet()).newPlaceHolders(((BeGrandMap) CurrentGrandMap.fluidGet()).newIDSpace().fullRegion());
	}
	else {
		result = otherTrail;
	}
	fossil = RecorderFossil.works((flags & FeEdition.DIRECTUCONTAINERSUONLY()) != 0, ((FeKeyMaster) CurrentKeyMaster.fluidGet()).loginAuthority(), filter, result.getOrMakeTrailBlazer());
	if ((flags & FeEdition.LOCALUPRESENTUONLY()) != 0) {
		scheduleImmediateBackfollow(fossil, region);
	}
	else {
		if ((flags & FeEdition.DIRECTUCONTAINERSUONLY()) != 0) {
			throw new UnimplementedException();
		}
		scheduleDelayedBackfollow(fossil, region);
	}
	return result;
/*
udanax-top.st:3150:BeEdition methodsFor: 'transclusions'!
{BeEdition} rangeWorks: region {XnRegion | NULL}
	with: filter {Filter}
	with: flags {Int32}
	with: otherTrail {BeEdition | NULL}
	"See FeEdition"
	
	| fossil {RecorderFossil} result {BeEdition} |
	
	"Reject all the unimplemented cases.
	
	if a trail isn't given
		make a new one
	else
		use it as the result.
		
	Make a fossilized recorder 
		snapshotting the current login authority
		filtered by the endorsementsFilter
		for recording into the trail
	Set the transclusions request in motion
	Return the trail"  
	  
	(flags bitAnd: (FeEdition DIRECT.U.CONTAINERS.U.ONLY
			bitOr: FeEdition LOCAL.U.PRESENT.U.ONLY) bitInvert) ~~ Int32Zero
		ifTrue: [self unimplemented].
	
	otherTrail == NULL 
		ifTrue: [result := CurrentGrandMap fluidGet 
				newPlaceHolders: CurrentGrandMap fluidGet newIDSpace fullRegion]
		ifFalse: [result := otherTrail].
		
	fossil := RecorderFossil 
		works: (flags bitAnd: FeEdition DIRECT.U.CONTAINERS.U.ONLY) ~~ Int32Zero
		with: CurrentKeyMaster fluidGet loginAuthority 
		with: filter
		with: result getOrMakeTrailBlazer.
	
	(flags bitAnd: FeEdition LOCAL.U.PRESENT.U.ONLY) ~~ Int32Zero
		ifTrue: [self scheduleImmediateBackfollow: fossil with: region]
		ifFalse: [(flags bitAnd: FeEdition DIRECT.U.CONTAINERS.U.ONLY) ~~ Int32Zero
				ifTrue: [self unimplemented].
			self scheduleDelayedBackfollow: fossil with: region].
	^result!
*/
}
/**
 * Walk down orgl's O-tree (onto range elements of interest) planting pointers to a Fossil of
 * BackfollowRecorder in the sensor canopy and collecting agenda items to propagate their
 * endorsement and permission filtering info rootward in the sensor canopy.
 * Create and schedule a structure of AgendaItems to:
 * - First:  Do the filtering info propagation.
 * - Second: Find and record any currently matching stamps.
 * This is done in this order so collection of the future part of recorder information is
 * completed before the present part is extracted, keeping significant information from
 * falling through the crack.
 */
public void scheduleDelayedBackfollow(RecorderFossil fossil, XnRegion region) {
	Agenda rAgents;
	AgendaItem matcher;
	OrglRoot oroot;
	/* Create an empty Agenda.
	Do the walk and collect PropChangers in the new Agenda.
	Reanimate the Fossil long enough to
		make a Matcher AgendaItem
			from the filtering information extracted from the Fossil
	Make and schedule a Sequencer that first runs the Agenda that propagates filtering info, then runs the Matcher. */
	if (fossil.isExtinct()) {
		return ;
	}
	rAgents = Agenda.make();
	if (region == null) {
		oroot = myOrglRoot;
	}
	else {
		Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, hCrum().hCut().newSuccessor());
		try {
			Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
			try {
				oroot = myOrglRoot.copy(region);
			}
			finally {
				AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
		}
	}
	oroot.storeRecordingAgents(fossil, rAgents);
	ResultRecorder recorder = AboraBlockSupport.enterRecorderFossilReanimate(fossil);
	try {
		matcher = Matcher.make(oroot, recorder.bertPropFinder(), fossil);
	}
	finally {
		AboraBlockSupport.exitRecorderFossilReanimate();
	}
	(Sequencer.make(rAgents, matcher)).schedule();
/*
udanax-top.st:3194:BeEdition methodsFor: 'transclusions'!
{void} scheduleDelayedBackfollow: fossil {RecorderFossil} with: region {XnRegion | NULL}
	"Walk down orgl's O-tree (onto range elements of interest) planting pointers to a Fossil of BackfollowRecorder in the sensor canopy and collecting agenda items to propagate their endorsement and permission filtering info rootward in the sensor canopy.
	Create and schedule a structure of AgendaItems to:
		- First:  Do the filtering info propagation.
		- Second: Find and record any currently matching stamps.
	
	This is done in this order so collection of the future part of recorder information is completed before the present part is extracted, keeping significant information from falling through the crack."
	  	
	| rAgents {Agenda} matcher {AgendaItem} oroot {OrglRoot} |
	  
	"Create an empty Agenda.
	Do the walk and collect PropChangers in the new Agenda.
	Reanimate the Fossil long enough to
		make a Matcher AgendaItem
			from the filtering information extracted from the Fossil
	Make and schedule a Sequencer that first runs the Agenda that propagates filtering info, then runs the Matcher."
	fossil isExtinct ifTrue:
		[^VOID].
	rAgents _ Agenda make.
	region == NULL ifTrue:
		[oroot := myOrglRoot]
	ifFalse:
		[CurrentTrace fluidBind: self hCrum hCut newSuccessor 
		during: [CurrentBertCrum fluidBind: BertCrum make
		during: [oroot := myOrglRoot copy: region]]].
	oroot storeRecordingAgents: fossil with: rAgents.
	fossil reanimate: [:recorder {ResultRecorder} |
		matcher _ Matcher make: oroot
			with: recorder bertPropFinder
			with: fossil].
	(Sequencer make: rAgents with: matcher) schedule!
*/
}
/**
 * Find and record any currently matching Editions.
 */
public void scheduleImmediateBackfollow(RecorderFossil fossil, XnRegion region) {
	OrglRoot oroot;
	MarkM.thingToDo();
	/* When we are actually leaving AgendaItems on the queue, make sure that all necessary canopy propagation is done before the Matcher excutes */
	if (region == null) {
		oroot = myOrglRoot;
	}
	else {
		Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, hCrum().hCut().newSuccessor());
		try {
			Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
			try {
				oroot = myOrglRoot.copy(region);
			}
			finally {
				AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
		}
	}
	ResultRecorder recorder = AboraBlockSupport.enterRecorderFossilReanimate(fossil);
	try {
		(Matcher.make(oroot, recorder.bertPropFinder(), fossil)).schedule();
	}
	finally {
		AboraBlockSupport.exitRecorderFossilReanimate();
	}
/*
udanax-top.st:3228:BeEdition methodsFor: 'transclusions'!
{void} scheduleImmediateBackfollow: fossil {RecorderFossil} with: region {XnRegion | NULL}
	"Find and record any currently matching Editions."
	| oroot {OrglRoot} |
	 
	MarkM thingToDo. "When we are actually leaving AgendaItems on the queue, make sure that all necessary canopy propagation is done before the Matcher excutes"
	
	region == NULL ifTrue:
		[oroot := myOrglRoot]
	ifFalse:
		[CurrentTrace fluidBind: self hCrum hCut newSuccessor 
		during: [CurrentBertCrum fluidBind: BertCrum make
		during: [oroot := myOrglRoot copy: region]]].
	fossil reanimate: [:recorder {ResultRecorder} |
		(Matcher make: oroot
			with: recorder bertPropFinder
			with: fossil) schedule]!
*/
}
public void propChanged(PropChange change, Prop old, Prop nw) {
	propChanged(change, old, nw, null);
/*
udanax-top.st:3250:BeEdition methodsFor: 'smalltalk: defaults'!
{void} propChanged: change {PropChange} with: old {Prop} with: nw {Prop} 
	self propChanged: change with: old with: nw with: NULL!
*/
}
public XnRegion sharedRegion(BeEdition other) {
	return sharedRegion(other, 0);
/*
udanax-top.st:3254:BeEdition methodsFor: 'smalltalk: defaults'!
{XnRegion} sharedRegion: other {BeEdition}
	^self sharedRegion: other with: 0!
*/
}
/**
 * @deprecated
 */
public MuSet detectors() {
	throw new PasseException();
/*
udanax-top.st:3260:BeEdition methodsFor: 'smalltalk: passe'!
{MuSet of: FeFillRangeDetector} detectors
	self passe!
*/
}
/**
 * An actual, non-virtual FE range element at that key. Used by become operation to get
 * something to pass into BeRangeElement::become ()
 * @deprecated
 */
public BeRangeElement fetchOrMakeBeRangeElement(Position key) {
	throw new PasseException();
/*
udanax-top.st:3264:BeEdition methodsFor: 'smalltalk: passe'!
{BeRangeElement | NULL} fetchOrMakeBeRangeElement: key {Position}
	"An actual, non-virtual FE range element at that key. Used by become operation to get something to pass into BeRangeElement::become ()"
	self passe "no implementation, senders, or polymorphs - /ravi/10/7/92/"!
*/
}
/**
 * @deprecated
 */
public BeEdition parcelAt(Position key) {
	throw new PasseException();
/*
udanax-top.st:3268:BeEdition methodsFor: 'smalltalk: passe'!
{BeEdition} parcelAt: key {Position}
	self passe!
*/
}
/**
 * @deprecated
 */
public BeEdition parcels() {
	throw new PasseException();
/*
udanax-top.st:3272:BeEdition methodsFor: 'smalltalk: passe'!
{BeEdition} parcels
	self passe!
*/
}
/**
 * Rearrange the keys of this Edition to lie in the given region, with the given ordering.
 * Equivalent to server->makeEdition (this->asArray (oldRegion, oldOrder), newRegion,
 * newOrder, NULL), except that it doesn't require everything to be in the same zone (and is
 * of course more efficient).
 * @deprecated
 */
public BeEdition reorganize(XnRegion oldRegion, OrderSpec oldOrder, XnRegion newRegion, OrderSpec newOrder) {
	throw new PasseException();
/*
udanax-top.st:3276:BeEdition methodsFor: 'smalltalk: passe'!
{BeEdition PROXY} reorganize: oldRegion {XnRegion | NULL}
	with: oldOrder {OrderSpec | NULL}
	with: newRegion {XnRegion | NULL}
	with: newOrder {OrderSpec | NULL}
	"Rearrange the keys of this Edition to lie in the given region, with the given ordering. Equivalent to server->makeEdition (this->asArray (oldRegion, oldOrder), newRegion, newOrder, NULL), except that it doesn't require everything to be in the same zone (and is of course more efficient)."
	self unimplemented!
*/
}
/**
 * @deprecated
 */
public void scheduleDelayedBackfollow(RecorderFossil fossil) {
	throw new PasseException();
/*
udanax-top.st:3283:BeEdition methodsFor: 'smalltalk: passe'!
{void} scheduleDelayedBackfollow: fossil {RecorderFossil}
	self passe!
*/
}
/**
 * @deprecated
 */
public void scheduleImmediateBackfollow(RecorderFossil fossil) {
	throw new PasseException();
/*
udanax-top.st:3287:BeEdition methodsFor: 'smalltalk: passe'!
{void} scheduleImmediateBackfollow: fossil {RecorderFossil}
	self passe!
*/
}
/**
 * @deprecated
 */
public BeEdition setAllOwners(ID newOwner) {
	throw new PasseException();
/*
udanax-top.st:3291:BeEdition methodsFor: 'smalltalk: passe'!
{BeEdition} setAllOwners: newOwner {ID} 
	self passe!
*/
}
/**
 * @deprecated
 */
public BeEdition setAllOwners(ID newOwner, XnRegion region) {
	throw new PasseException();
/*
udanax-top.st:3295:BeEdition methodsFor: 'smalltalk: passe'!
{BeEdition} setAllOwners: newOwner {ID} with: region {XnRegion}
	self passe "setRangeOwners"!
*/
}
/**
 * @deprecated
 */
public void unendorse(CrossRegion endorsements) {
	throw new PasseException();
/*
udanax-top.st:3299:BeEdition methodsFor: 'smalltalk: passe'!
{void} unendorse: endorsements {CrossRegion}
	self passe "retract"!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:3303:BeEdition methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor}
	
	self passe!
*/
}
public BeEdition(Rcvr receiver) {
	super(receiver);
	myOrglRoot = (OrglRoot) receiver.receiveHeaper();
	myWorks = (MuSet) receiver.receiveHeaper();
	myOwnProp = (BertProp) receiver.receiveHeaper();
	myProp = (BertProp) receiver.receiveHeaper();
	restartE(receiver);
/*
udanax-top.st:3309:BeEdition methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myOrglRoot _ receiver receiveHeaper.
	myWorks _ receiver receiveHeaper.
	myOwnProp _ receiver receiveHeaper.
	myProp _ receiver receiveHeaper.
	self restartE: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myOrglRoot);
	xmtr.sendHeaper(myWorks);
	xmtr.sendHeaper(myOwnProp);
	xmtr.sendHeaper(myProp);
/*
udanax-top.st:3317:BeEdition methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myOrglRoot.
	xmtr sendHeaper: myWorks.
	xmtr sendHeaper: myOwnProp.
	xmtr sendHeaper: myProp.!
*/
}
public static BeEdition make(OrglRoot oroot) {
	AboraBlockSupport.enterConsistent(5);
	try {
		return new BeEdition(oroot);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:3336:BeEdition class methodsFor: 'creation'!
make: oroot {OrglRoot}
	DiskManager consistent: 5 with:
		[^self create: oroot]!
*/
}
public BeEdition() {
/*

Generated during transformation
*/
}
public Stepper retrieve(XnRegion region, OrderSpec order) {
	return retrieve(region, order, 0);
/*

Generated during transformation: AddDefaultParameter
*/
}
public Stepper retrieve(XnRegion region) {
	return retrieve(region, null );
/*

Generated during transformation: AddDefaultParameter
*/
}
public Stepper retrieve() {
	return retrieve( null );
/*

Generated during transformation: AddDefaultParameter
*/
}
}
