/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.ents;

import info.dgjones.abora.gold.arrange.Arrangement;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BePlaceHolder;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.ents.ActualOrglRoot;
import info.dgjones.abora.gold.be.ents.HUpperCrum;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OExpandingLoaf;
import info.dgjones.abora.gold.be.ents.OPartialLoaf;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.be.ents.RegionLoaf;
import info.dgjones.abora.gold.be.ents.SplitLoaf;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FePlaceHolder;
import info.dgjones.abora.gold.nkernel.FePlaceHolderBundle;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class OPartialLoaf extends OExpandingLoaf {

	protected ID myOwner;
	protected TrailBlazer myTrailBlazer;
/*
udanax-top.st:8778:
OExpandingLoaf subclass: #OPartialLoaf
	instanceVariableNames: '
		myOwner {ID}
		myTrailBlazer {TrailBlazer | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:8784:
(OPartialLoaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #NOT.A.TYPE; add: #CONCRETE; add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #(MAY.BECOME RegionLoaf ); yourself)!
*/
/*
udanax-top.st:9057:
OPartialLoaf class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:9060:
(OPartialLoaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #NOT.A.TYPE; add: #CONCRETE; add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #(MAY.BECOME RegionLoaf ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OPartialLoaf.class).setAttributes( new Set().add("NOTATYPE").add("CONCRETE").add("COPY").add("SHEPHERDANCESTOR").add("LOCKED").add( new String[]
	{"MAYBECOME", "RegionLoaf"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Make a virtual PlaceHolder.
 */
public FeRangeElement fetch(Position key, BeEdition edition, Position globalKey) {
	if (domain().hasMember(key)) {
		return FePlaceHolder.fake(edition, globalKey);
	}
	else {
		return null;
	}
/*
udanax-top.st:8789:OPartialLoaf methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition} with: globalKey {Position}
	"Make a virtual PlaceHolder."
	(self domain hasMember: key) 
		ifTrue: [^FePlaceHolder fake: edition with: globalKey] 
		ifFalse: [^NULL]!
*/
}
/**
 * Get or make the BeRangeElement at the location.
 */
public BeRangeElement getBe(Position key) {
	/* My region had better be just onto the key.
	 become a RegionLoaf onto a new BePlaceHolder */
	BeRangeElement element;
	XnRegion domain;
	HUpperCrum hcrum;
	int hash;
	FlockInfo info;
	domain = key.asRegion();
	if ( ! (domain().isEqual(domain))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	hcrum = (HUpperCrum) hCrum();
	hash = hashForEqual();
	info = fetchInfo();
	AboraBlockSupport.enterConsistent();
	try {
		sensorCrum().removePointer(this);
		Object initialOwnerOldValue = AboraBlockSupport.enterFluidBindDuring(InitialOwner, owner());
		try {
			Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, hCrum().hCut());
			try {
				Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
				try {
					element = new BePlaceHolder(myTrailBlazer);
					if (myTrailBlazer != null) {
						myTrailBlazer.removeReference(this);
						myTrailBlazer = null;
					}
				}
				finally {
					AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
				}
			}
			finally {
				AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(InitialOwner, initialOwnerOldValue);
		}
		/* TODO newBecome */
		new RegionLoaf(domain, element, hcrum, hash, info);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return element;
/*
udanax-top.st:8796:OPartialLoaf methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"Get or make the BeRangeElement at the location."
	
	"My region had better be just onto the key.
	 become a RegionLoaf onto a new BePlaceHolder"
		
	| element {BeRangeElement}  domain {XnRegion}
	 hcrum {HUpperCrum} hash {UInt32} info {FlockInfo}|
	domain _ key asRegion.
	(self domain isEqual: domain) ifFalse: [Heaper BLAST: #NotInTable].
	hcrum _ self hCrum cast: HUpperCrum.
	hash _ self hashForEqual.
	info _ self fetchInfo.
	DiskManager consistent:
		[self sensorCrum removePointer: self.
		InitialOwner fluidBind: self owner
			during: [[Ent] USES. CurrentTrace fluidBind: self hCrum hCut
			during: [CurrentBertCrum fluidBind: BertCrum make
			during:
				[element _ BePlaceHolder create: myTrailBlazer.
				myTrailBlazer ~~ NULL ifTrue:
					[myTrailBlazer removeReference: self.
					myTrailBlazer := NULL]]]].
		(RegionLoaf new.Become: self)
			create: domain
			with: element 
			with: hcrum
			with: hash 
			with: info].
	^element!
*/
}
/**
 * Return the owner of the atoms represented by the receiver.
 */
public ID owner() {
	return myOwner;
/*
udanax-top.st:8827:OPartialLoaf methodsFor: 'accessing'!
{ID} owner
	"Return the owner of the atoms represented by the receiver."
	
	^myOwner!
*/
}
/**
 * Return the PrimSpec that describes the representation of the data.
 */
public PrimSpec spec() {
	throw new UnimplementedException();
/*
udanax-top.st:8832:OPartialLoaf methodsFor: 'accessing'!
{PrimSpec} spec
	"Return the PrimSpec that describes the representation of the data."
	
	self unimplemented.
	^PrimSpec pointer!
*/
}
public XnRegion usedDomain() {
	return domain().coordinateSpace().emptyRegion();
/*
udanax-top.st:8838:OPartialLoaf methodsFor: 'accessing'!
{XnRegion} usedDomain
	^self domain coordinateSpace emptyRegion!
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order, Dsp globalDsp) {
	XnRegion bundleRegion;
	bundleRegion = region.intersect((globalDsp.ofAll(domain())));
	if (bundleRegion.isEmpty()) {
		return Stepper.emptyStepper();
	}
	return Stepper.itemStepper((FePlaceHolderBundle.make(bundleRegion)));
/*
udanax-top.st:8843:OPartialLoaf methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec} with: globalDsp {Dsp}
	"Return a stepper of bundles according to the order."
	
	| bundleRegion {XnRegion} |
	bundleRegion _ region intersect: (globalDsp ofAll: self domain).
	bundleRegion isEmpty ifTrue: [^Stepper emptyStepper].
	^Stepper itemStepper: (FePlaceHolderBundle make: bundleRegion)!
*/
}
/**
 * Make an FeRangeElement for each position.
 */
public void fill(XnRegion keys, Arrangement toArrange, PrimArray toArray, Dsp dsp, BeEdition edition) {
	Stepper stomper = (keys.intersect(domain())).stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Position key = (Position) stomper.fetch();
		if (key == null) {
			continue ;
		}
		Position globalKey;
		globalKey = dsp.of(key);
		toArray.storeValue((toArrange.indexOf(globalKey)), (FePlaceHolder.fake(edition, globalKey)));
	}
	stomper.destroy();
/*
udanax-top.st:8851:OPartialLoaf methodsFor: 'operations'!
{void} fill: keys {XnRegion} with: toArrange {Arrangement} with: toArray {PrimArray} with: dsp {Dsp} with: edition {BeEdition} 
	"Make an FeRangeElement for each position."
	
	(keys intersect: self domain) stepper forEach: 
		[:key {Position} |
		| globalKey {Position} |
		globalKey _ dsp of: key.
		toArray at: (toArrange indexOf: globalKey) DOTasLong
			storeValue: (FePlaceHolder fake: edition with: globalKey)]!
*/
}
public void informTo(OrglRoot orgl) {
	throw new UnimplementedException();
/*
udanax-top.st:8861:OPartialLoaf methodsFor: 'operations'!
{void} informTo: orgl {OrglRoot unused}
	self unimplemented!
*/
}
/**
 * Partial crums are always partial.
 */
public boolean isPartial() {
	return true;
/*
udanax-top.st:8864:OPartialLoaf methodsFor: 'operations'!
{Boolean} isPartial
	"Partial crums are always partial."
	^true!
*/
}
/**
 * If the CurrentKeyMaster includes the owner of this loaf
 * then change the owner and return NULL
 * else just return self.
 */
public OrglRoot setAllOwners(ID owner) {
	if (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(myOwner)) {
		myOwner = owner;
		return OrglRoot.make(domain().coordinateSpace());
	}
	else {
		return ActualOrglRoot.make(this, domain());
	}
/*
udanax-top.st:8869:OPartialLoaf methodsFor: 'operations'!
{OrglRoot} setAllOwners: owner {ID} 
	"If the CurrentKeyMaster includes the owner of this loaf
		then change the owner and return NULL
		else just return self."
		
	(CurrentKeyMaster fluidGet hasAuthority: myOwner)
		ifTrue:
			[myOwner _ owner.
			^OrglRoot make: self domain coordinateSpace]
		ifFalse: [^ActualOrglRoot make: self with: self domain]!
*/
}
/**
 * Don't expand me in place.  Just move it closer to the top.
 */
public int actualSoftSplay(XnRegion region, XnRegion limitRegion) {
	return 2;
/*
udanax-top.st:8882:OPartialLoaf methodsFor: 'protected: splay'!
{Int8} actualSoftSplay: region {XnRegion} with: limitRegion {XnRegion unused} 
	"Don't expand me in place.  Just move it closer to the top."
	
	^2!
*/
}
/**
 * Expand my partial tree in place. The area in the region must go
 * into the leftCrum of my substitute, or the splay algorithm will fail!!
 */
public int actualSplay(XnRegion region, XnRegion limitRegion) {
	Pair crums;
	Loaf tmp1;
	Loaf tmp2;
	crums = sensorCrum().expand();
	AboraBlockSupport.enterConsistent(3);
	try {
		tmp1 = new OPartialLoaf((domain().intersect(region)), (HUpperCrum.make(((HUpperCrum) hCrum()))), ((SensorCrum) crums.left()), myOwner, myTrailBlazer);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	AboraBlockSupport.enterConsistent(3);
	try {
		tmp2 = new OPartialLoaf((domain().intersect(region.complement())), (HUpperCrum.make(((HUpperCrum) hCrum()))), ((SensorCrum) crums.right()), myOwner, myTrailBlazer);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	if (myTrailBlazer != null) {
		AboraBlockSupport.enterConsistent(1);
		try {
			myTrailBlazer.addReference(tmp1);
			myTrailBlazer.addReference(tmp2);
			myTrailBlazer.removeReference(this);
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	AboraBlockSupport.enterConsistent(5);
	try {
		HUpperCrum hcrum;
		int hash;
		FlockInfo info;
		CanopyCrum oldSensorCrum;
		hcrum = (HUpperCrum) hCrum();
		hash = hashForEqual();
		oldSensorCrum = sensorCrum();
		info = fetchInfo();
		/* TODO newBecome */
		new SplitLoaf(region, tmp1, tmp2, hcrum, hash, info);
		/* The new SplitLoaf will add itself. */
		oldSensorCrum.removePointer(this);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return 1;
/*
udanax-top.st:8887:OPartialLoaf methodsFor: 'protected: splay'!
{Int8} actualSplay: region {XnRegion} with: limitRegion {XnRegion unused} 
	"Expand my partial tree in place. The area in the region must go 
	into the leftCrum of my substitute, or the splay algorithm will fail!!"
	
	| crums {Pair of: SensorCrum} tmp1 {Loaf} tmp2 {Loaf} |
	 
	crums _ self sensorCrum expand.
	DiskManager consistent: 3 with: 
		[tmp1 _ OPartialLoaf
					create: (self domain intersect: region)
					with: (HUpperCrum make: (self hCrum cast: HUpperCrum))
					with: (crums left cast: SensorCrum)
					with: myOwner
					with: myTrailBlazer].
	DiskManager consistent: 3 with: 
		[tmp2 _ OPartialLoaf
					create: (self domain intersect: region complement)
					with: (HUpperCrum make: (self hCrum cast: HUpperCrum))
					with: (crums right cast: SensorCrum)
					with: myOwner
					with: myTrailBlazer].
	myTrailBlazer ~~ NULL ifTrue:
		[DiskManager consistent: 1 with:
			[myTrailBlazer addReference: tmp1.
			myTrailBlazer addReference: tmp2.
			myTrailBlazer removeReference: self]].
	DiskManager consistent: 5 with: 
		[| hcrum {HUpperCrum} 
		  hash {UInt32} 
		  info {FlockInfo} 
		  oldSensorCrum {CanopyCrum} |
		hcrum _ self hCrum cast: HUpperCrum.
		hash _ self hashForEqual.
		oldSensorCrum _ self sensorCrum.
		info _ self fetchInfo.
		(SplitLoaf new.Become: self)
				create: region
				with: tmp1
				with: tmp2
				with: hcrum
				with: hash
				with: info.
			"The new SplitLoaf will add itself."
			oldSensorCrum removePointer: self].
	^1!
*/
}
public OPartialLoaf(XnRegion region) {
	super(region);
	myOwner = ((ID) InitialOwner.fluidFetch());
	myTrailBlazer = null;
	newShepherd();
/*
udanax-top.st:8935:OPartialLoaf methodsFor: 'create'!
create: region {XnRegion}
	
	super create: region.
	myOwner _ InitialOwner fluidFetch.
	myTrailBlazer := NULL.
	self newShepherd!
*/
}
public OPartialLoaf(XnRegion region, HUpperCrum hcrum, SensorCrum scrum) {
	super(region, hcrum, scrum);
	myOwner = ((ID) InitialOwner.fluidFetch());
	myTrailBlazer = null;
	newShepherd();
/*
udanax-top.st:8942:OPartialLoaf methodsFor: 'create'!
create: region {XnRegion} with: hcrum {HUpperCrum} with: scrum {SensorCrum}
	super create: region with: hcrum with: scrum.
	myOwner _ InitialOwner fluidFetch.
	myTrailBlazer := NULL.
	self newShepherd!
*/
}
public OPartialLoaf(XnRegion region, HUpperCrum hcrum, SensorCrum scrum, ID owner, TrailBlazer blazer) {
	super(region, hcrum, scrum);
	myOwner = owner;
	myTrailBlazer = blazer;
	newShepherd();
/*
udanax-top.st:8948:OPartialLoaf methodsFor: 'create'!
create: region {XnRegion}
	with: hcrum {HUpperCrum}
	with: scrum {SensorCrum}
	with: owner {ID}
	with: blazer {TrailBlazer | NULL}
	super create: region with: hcrum with: scrum.
	myOwner := owner.
	myTrailBlazer := blazer.
	self newShepherd!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(4);
	try {
		if (Heaper.isConstructed(myTrailBlazer)) {
			myTrailBlazer.removeReference(this);
		}
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:8961:OPartialLoaf methodsFor: 'protected: delete'!
{void} dismantle
	
	DiskManager consistent: 4 with:
		[(Heaper isConstructed: myTrailBlazer) ifTrue:
			[myTrailBlazer removeReference: self].
		super dismantle]!
*/
}
/**
 * inform a piece of partiality
 * @deprecated
 */
public void inform(Position key, BeRangeElement element, TracePosition trace) {
	throw new PasseException();
/*
udanax-top.st:8970:OPartialLoaf methodsFor: 'smalltalk: passe'!
{void} inform: key {Position} with: element {BeRangeElement} with: trace {TracePosition}
	"inform a piece of partiality"
	self passe.
	[| in {XnRegion} impartial {Loaf} hcrum {HUpperCrum} hash {UInt32} info {FlockInfo} sensors {ImmuSet} |
	(self domain hasMember: key) ifFalse: [Heaper BLAST: #NotInTable].
	(self hCrum hCut isEqual: trace) ifFalse: [Heaper BLAST: #CantInform].
	in _ key asRegion.
	hcrum _ self hCrum cast: HUpperCrum.
	hash _ self hashForEqual.
	info _ self fetchInfo.
	Someone shouldImplement.
	self unimplemented.   "used to be detectors.  sensors _ mySensors."
	(in isEqual: self domain) 
		ifTrue:
			[impartial _ self.
			self sensorCrum removePointer: self.
			(RegionLoaf new.Become: self)
				create: in with: element with: (HUpperCrum make: hcrum) with: hash with: info]
		ifFalse:
			[ | partial {Loaf} |
			impartial _ Loaf make.Region: in with: (CurrentGrandMap fluidGet carrier: element).
			partial _ OPartialLoaf make: (self domain minus: in)
				with: (HUpperCrum make: hcrum)
				with: self sensorCrum.
			self sensorCrum removePointer: self.
			(SplitLoaf new.Become: self) create: in
				with: impartial
				with: partial
				with: hcrum
				with: hash
				with: info].
	"self flockInfo: info."
	Dean shouldImplement.
	"sensors stepper forEach: [ :sensor {XnSensor} |
		sensor ring: impartial]"] smalltalkOnly "so we can look at the old code"!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:9006:OPartialLoaf methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor} 
	
	self passe!
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	AboraBlockSupport.enterConsistent(2);
	try {
		if (myTrailBlazer != null) {
			if (myTrailBlazer.isAlive()) {
				throw new AboraRuntimeException(AboraRuntimeException.FATAL_ERROR);
			}
			else {
				myTrailBlazer.removeReference(this);
			}
		}
		myTrailBlazer = blazer;
		blazer.addReference(this);
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return domain();
/*
udanax-top.st:9012:OPartialLoaf methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	DiskManager consistent: 2 with:
		[myTrailBlazer ~~ NULL ifTrue:
			[myTrailBlazer isAlive
				ifTrue: [Heaper BLAST: #FatalError]
				ifFalse: [myTrailBlazer removeReference: self]].
		myTrailBlazer := blazer.
		blazer addReference: self.
		self diskUpdate].
	^self domain!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	if ( ! (myTrailBlazer != null && (myTrailBlazer.isEqual(blazer)))) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_TRAIL);
	}
/*
udanax-top.st:9024:OPartialLoaf methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	
	(myTrailBlazer ~~ NULL and: [myTrailBlazer isEqual: blazer]) ifFalse:
		[Heaper BLAST: #InvalidTrail].!
*/
}
public TrailBlazer fetchTrailBlazer() {
	if (myTrailBlazer == null || (myTrailBlazer.isAlive())) {
		return myTrailBlazer;
	}
	/* it was not successfully attached, so clean it up */
	AboraBlockSupport.enterConsistent(2);
	try {
		myTrailBlazer.removeReference(this);
		myTrailBlazer = null;
		diskUpdate();
		return null;
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:9029:OPartialLoaf methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	
	(myTrailBlazer == NULL or: [myTrailBlazer isAlive])
		ifTrue: [^myTrailBlazer].
	"it was not successfully attached, so clean it up"
	DiskManager consistent: 2 with:
		[myTrailBlazer removeReference: self.
		myTrailBlazer := NULL.
		self diskUpdate.
		^NULL]!
*/
}
/**
 * do nothing
 */
public void triggerDetector(FeFillRangeDetector detect) {
/*
udanax-top.st:9040:OPartialLoaf methodsFor: 'backfollow'!
{void} triggerDetector: detect {FeFillRangeDetector}
	
	"do nothing"!
*/
}
public OPartialLoaf(Rcvr receiver) {
	super(receiver);
	myOwner = (ID) receiver.receiveHeaper();
	myTrailBlazer = (TrailBlazer) receiver.receiveHeaper();
/*
udanax-top.st:9046:OPartialLoaf methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myOwner _ receiver receiveHeaper.
	myTrailBlazer _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myOwner);
	xmtr.sendHeaper(myTrailBlazer);
/*
udanax-top.st:9051:OPartialLoaf methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myOwner.
	xmtr sendHeaper: myTrailBlazer.!
*/
}
/**
 * @deprecated
 */
public static Loaf make(XnRegion region, HUpperCrum hcrum, SensorCrum scrum) {
	throw new PasseException();
/*
udanax-top.st:9065:OPartialLoaf class methodsFor: 'smalltalk: passe'!
{Loaf} make: region {XnRegion} 
	with: hcrum {HUpperCrum} 
	with: scrum {SensorCrum}
	
	self passe!
*/
}
public OPartialLoaf() {
/*

Generated during transformation
*/
}
}
