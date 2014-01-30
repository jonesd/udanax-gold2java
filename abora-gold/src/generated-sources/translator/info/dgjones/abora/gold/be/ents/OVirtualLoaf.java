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
import info.dgjones.abora.gold.be.basic.BeDataHolder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.ents.ActualOrglRoot;
import info.dgjones.abora.gold.be.ents.HUpperCrum;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OExpandingLoaf;
import info.dgjones.abora.gold.be.ents.OVirtualLoaf;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.be.ents.RegionLoaf;
import info.dgjones.abora.gold.be.ents.SharedData;
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
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeDataHolder;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class OVirtualLoaf extends OExpandingLoaf {

	protected ID myOwner;
	protected SharedData myData;
/*
udanax-top.st:9071:
OExpandingLoaf subclass: #OVirtualLoaf
	instanceVariableNames: '
		myOwner {ID}
		myData {SharedData}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:9077:
(OVirtualLoaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OVirtualLoaf.class).setAttributes( new Set().add("COPY").add("SHEPHERDANCESTOR").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Make a virtual DataHolder.
 */
public FeRangeElement fetch(Position key, BeEdition edition, Position globalKey) {
	if (domain().hasMember(key)) {
		return FeDataHolder.fake(((PrimValue) (myData.fetch(key))), globalKey, edition);
	}
	else {
		return null;
	}
/*
udanax-top.st:9082:OVirtualLoaf methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition} with: globalKey {Position}
	"Make a virtual DataHolder."
	(self domain hasMember: key) 
		ifTrue: 
			[^FeDataHolder fake: ((myData fetch: key) cast: PrimValue)
				with: globalKey
				with: edition] 
		ifFalse: [^NULL]!
*/
}
/**
 * Get or make the BeRangeElement at the location.
 */
public BeRangeElement getBe(Position key) {
	/* My region had better be just onto the key.
	 become a RegionLoaf onto a new BeDataHolder containing the 
	 data extracted from my SharedData object. */
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
		CanopyCrum oldSensorCrum;
		oldSensorCrum = sensorCrum();
		Object initialOwnerOldValue = AboraBlockSupport.enterFluidBindDuring(InitialOwner, owner());
		try {
			Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, hCrum().hCut());
			try {
				Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, BertCrum.make());
				try {
					element = new BeDataHolder(((PrimValue) (myData.fetch(key))));
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
		oldSensorCrum.removePointer(this);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return element;
/*
udanax-top.st:9092:OVirtualLoaf methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"Get or make the BeRangeElement at the location."
	
	"My region had better be just onto the key.
	 become a RegionLoaf onto a new BeDataHolder containing the 
	 data extracted from my SharedData object."
		
	| element {BeRangeElement}  domain {XnRegion}
	 hcrum {HUpperCrum} hash {UInt32} info {FlockInfo}|
	domain _ key asRegion.
	(self domain isEqual: domain) ifFalse: [Heaper BLAST: #NotInTable].
	hcrum _ self hCrum cast: HUpperCrum.
	hash _ self hashForEqual.
	info _ self fetchInfo.
	DiskManager consistent:
		[| oldSensorCrum {CanopyCrum} |
		oldSensorCrum _ self sensorCrum.
		[Ent] USES.
		InitialOwner fluidBind: self owner
			during: [CurrentTrace fluidBind: self hCrum hCut
			during: [CurrentBertCrum fluidBind: BertCrum make
			during:
				[element _ BeDataHolder create: ((myData fetch: key) cast: PrimValue)]]].
		(RegionLoaf new.Become: self)
			create: domain
			with: element 
			with: hcrum
			with: hash 
			with: info.
		oldSensorCrum removePointer: self].
	^element!
*/
}
/**
 * Return the owner of the atoms represented by the receiver.
 */
public ID owner() {
	return myOwner;
/*
udanax-top.st:9124:OVirtualLoaf methodsFor: 'accessing'!
{ID} owner
	"Return the owner of the atoms represented by the receiver."
	
	^myOwner!
*/
}
/**
 * Return the primSpec for my data.
 */
public PrimSpec spec() {
	return myData.spec();
/*
udanax-top.st:9129:OVirtualLoaf methodsFor: 'accessing'!
{PrimSpec} spec
	"Return the primSpec for my data."
	
	^myData spec!
*/
}
public XnRegion usedDomain() {
	return domain();
/*
udanax-top.st:9134:OVirtualLoaf methodsFor: 'accessing'!
{XnRegion} usedDomain
	^self domain!
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order, Dsp globalDsp) {
	XnRegion bundleRegion;
	PrimArray array;
	bundleRegion = region.intersect((globalDsp.ofAll(domain())));
	if (bundleRegion.isEmpty()) {
		return Stepper.emptyStepper();
	}
	array = myData.spec().array(bundleRegion.count());
	myData.fill(bundleRegion, (order.arrange(bundleRegion)), array, globalDsp);
	return Stepper.itemStepper((FeArrayBundle.make(bundleRegion, array, order)));
/*
udanax-top.st:9139:OVirtualLoaf methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec} with: globalDsp {Dsp} 
	"Return a stepper of bundles according to the order."
	| bundleRegion {XnRegion} array {PrimArray} |
	bundleRegion _ region intersect: (globalDsp ofAll: self domain).
	bundleRegion isEmpty ifTrue: [^Stepper emptyStepper].
	array _ myData spec array: bundleRegion count DOTasLong.
	myData fill: bundleRegion with: (order arrange: bundleRegion) with: array with: globalDsp.
	^Stepper itemStepper: 
		(FeArrayBundle 
			make: bundleRegion 
			with: array
			with: order)!
*/
}
public void fill(XnRegion keys, Arrangement toArrange, PrimArray toArray, Dsp dsp, BeEdition edition) {
	myData.fill((keys.intersect(domain())), toArrange, toArray, dsp);
/*
udanax-top.st:9153:OVirtualLoaf methodsFor: 'operations'!
{void} fill: keys {XnRegion} with: toArrange {Arrangement} with: toArray {PrimArray} with: dsp {Dsp} with: edition {BeEdition} 
	
	myData fill: (keys intersect: self domain) with: toArrange with: toArray with: dsp!
*/
}
public void informTo(OrglRoot orgl) {
	throw new UnimplementedException();
/*
udanax-top.st:9157:OVirtualLoaf methodsFor: 'operations'!
{void} informTo: orgl {OrglRoot unused}
	self unimplemented!
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
udanax-top.st:9160:OVirtualLoaf methodsFor: 'operations'!
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
public void printOn(PrintWriter aStream) {
	aStream.print(getAboraClass().name());
	aStream.print("(");
	aStream.print(
	/* (myData table subTable: self domain) << */
	", ");
	aStream.print(hCrum().hCut());
	aStream.print(")");
/*
udanax-top.st:9173:OVirtualLoaf methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << self getCategory name << '(' << "(myData table subTable: self domain) <<" ', ' << self hCrum hCut << ')'!
*/
}
/**
 * Don't expand my virtual tree in place.  Just move it closer to the top.
 */
public int actualSoftSplay(XnRegion region, XnRegion limitRegion) {
	return 2;
/*
udanax-top.st:9178:OVirtualLoaf methodsFor: 'protected: splay'!
{Int8} actualSoftSplay: region {XnRegion} with: limitRegion {XnRegion unused} 
	"Don't expand my virtual tree in place.  Just move it closer to the top."
	
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
	Object initialOwnerOldValue = AboraBlockSupport.enterFluidBindDuring(InitialOwner, owner());
	try {
		AboraBlockSupport.enterConsistent(3);
		try {
			tmp1 = new OVirtualLoaf((domain().intersect(region)), myData, (HUpperCrum.make(((HUpperCrum) hCrum()))), ((SensorCrum) crums.left()));
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
		AboraBlockSupport.enterConsistent(3);
		try {
			tmp2 = new OVirtualLoaf((domain().intersect(region.complement())), myData, (HUpperCrum.make(((HUpperCrum) hCrum()))), ((SensorCrum) crums.right()));
		}
		finally {
			AboraBlockSupport.exitConsistent();
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
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(InitialOwner, initialOwnerOldValue);
	}
	return 1;
/*
udanax-top.st:9183:OVirtualLoaf methodsFor: 'protected: splay'!
{Int8} actualSplay: region {XnRegion} with: limitRegion {XnRegion unused} 
	"Expand my partial tree in place. The area in the region must go 
	into the leftCrum of my substitute, or the splay algorithm will fail!!"
	
	| crums {Pair of: SensorCrum} tmp1 {Loaf} tmp2 {Loaf} |
	crums _ self sensorCrum expand.
	InitialOwner fluidBind: self owner during:
	[DiskManager consistent: 3 with:
		[tmp1 _ OVirtualLoaf
					create: (self domain intersect: region)
					with: myData  
					with: (HUpperCrum make: (self hCrum cast: HUpperCrum))
					with: (crums left cast: SensorCrum)].
	DiskManager consistent: 3 with:
		[tmp2 _ OVirtualLoaf
					create: (self domain intersect: region complement)
					with: myData 
					with: (HUpperCrum make: (self hCrum cast: HUpperCrum))
					with: (crums right cast: SensorCrum)].
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
		oldSensorCrum removePointer: self]].
	^1!
*/
}
public OVirtualLoaf(XnRegion region, SharedData data) {
	super(region);
	myData = data;
	myOwner = ((ID) InitialOwner.fluidFetch());
	newShepherd();
/*
udanax-top.st:9224:OVirtualLoaf methodsFor: 'create'!
create: region {XnRegion} with: data {SharedData}
	super create: region.
	myData _ data.
	myOwner _ InitialOwner fluidFetch.
	self newShepherd!
*/
}
public OVirtualLoaf(XnRegion region, SharedData data, HUpperCrum hcrum, SensorCrum scrum) {
	super(region, hcrum, scrum);
	myData = data;
	myOwner = ((ID) InitialOwner.fluidFetch());
	newShepherd();
/*
udanax-top.st:9230:OVirtualLoaf methodsFor: 'create'!
create: region {XnRegion} with: data {SharedData} with: hcrum {HUpperCrum} with: scrum {SensorCrum}
	super create: region with: hcrum with: scrum.
	myData _ data.
	myOwner _ InitialOwner fluidFetch.
	self newShepherd!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ myData.contentsHash();
/*
udanax-top.st:9238:OVirtualLoaf methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: myData contentsHash!
*/
}
public void showOn(PrintWriter oo) {
	oo.print(myData);
/*
udanax-top.st:9245:OVirtualLoaf methodsFor: 'smalltalk:'!
showOn: oo
	oo << myData!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:9250:OVirtualLoaf methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor}
	
	self passe!
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	return domain().coordinateSpace().emptyRegion();
/*
udanax-top.st:9256:OVirtualLoaf methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	^self domain coordinateSpace emptyRegion!
*/
}
/**
 * it's OK
 */
public void checkTrailBlazer(TrailBlazer blazer) {
/*
udanax-top.st:9260:OVirtualLoaf methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	
	"it's OK"!
*/
}
public TrailBlazer fetchTrailBlazer() {
	return null;
/*
udanax-top.st:9264:OVirtualLoaf methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	
	^NULL!
*/
}
public void triggerDetector(FeFillRangeDetector detect) {
	detect.rangeFilled(asFeEdition());
/*
udanax-top.st:9268:OVirtualLoaf methodsFor: 'backfollow'!
{void} triggerDetector: detect {FeFillRangeDetector}
	
	detect rangeFilled: self asFeEdition!
*/
}
public OVirtualLoaf(Rcvr receiver) {
	super(receiver);
	myOwner = (ID) receiver.receiveHeaper();
	myData = (SharedData) receiver.receiveHeaper();
/*
udanax-top.st:9274:OVirtualLoaf methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myOwner _ receiver receiveHeaper.
	myData _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myOwner);
	xmtr.sendHeaper(myData);
/*
udanax-top.st:9279:OVirtualLoaf methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myOwner.
	xmtr sendHeaper: myData.!
*/
}
public OVirtualLoaf() {
/*

Generated during transformation
*/
}
}
