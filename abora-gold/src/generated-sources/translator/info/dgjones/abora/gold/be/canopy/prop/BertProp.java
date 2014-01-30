/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy.prop;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.filter.Joint;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.BertPropJoint;
import info.dgjones.abora.gold.java.missing.PropJoint;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * The properties which are nevigable towards using the Bert Canopy.  All of these are
 * properties of the Stamps at the leaves of the Bert Canopy.
 */
public class BertProp extends Prop {

	protected XnRegion myPermissions;
	protected XnRegion myEndorsements;
	protected boolean mySensorWaitingFlag;
	protected boolean myCannotPartializeFlag;
	protected static BertProp TheIdentityBertProp;
/*
udanax-top.st:38217:
Prop subclass: #BertProp
	instanceVariableNames: '
		myPermissions {XnRegion of: ID}
		myEndorsements {XnRegion of: ID}
		mySensorWaitingFlag {BooleanVar}
		myCannotPartializeFlag {BooleanVar}'
	classVariableNames: 'TheIdentityBertProp {BertProp} '
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy-Prop'!
*/
/*
udanax-top.st:38225:
BertProp comment:
'The properties which are nevigable towards using the Bert Canopy.  All of these are properties of the Stamps at the leaves of the Bert Canopy.'!
*/
/*
udanax-top.st:38227:
(BertProp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:38327:
BertProp class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:38330:
(BertProp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BertProp.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public CrossRegion endorsements() {
	return (CrossRegion) myEndorsements;
/*
udanax-top.st:38232:BertProp methodsFor: 'accessing'!
{CrossRegion} endorsements
	^myEndorsements cast: CrossRegion!
*/
}
public int flags() {
	return BertCrum.flagsFor(((IDRegion) myPermissions), ((CrossRegion) myEndorsements), myCannotPartializeFlag, mySensorWaitingFlag);
/*
udanax-top.st:38235:BertProp methodsFor: 'accessing'!
{UInt32} flags
	^BertCrum flagsFor: (myPermissions cast: IDRegion)
		with: (myEndorsements cast: CrossRegion)
		with: myCannotPartializeFlag
		with: mySensorWaitingFlag!
*/
}
public boolean isNotPartializable() {
	return myCannotPartializeFlag;
/*
udanax-top.st:38242:BertProp methodsFor: 'accessing'!
{BooleanVar} isNotPartializable
	^myCannotPartializeFlag!
*/
}
public boolean isSensorWaiting() {
	return mySensorWaitingFlag;
/*
udanax-top.st:38245:BertProp methodsFor: 'accessing'!
{BooleanVar} isSensorWaiting
	^mySensorWaitingFlag!
*/
}
public XnRegion permissions() {
	return myPermissions;
/*
udanax-top.st:38248:BertProp methodsFor: 'accessing'!
{XnRegion of: ID} permissions
	^myPermissions!
*/
}
public Prop with(Prop other) {
	BertProp o;
	o = (BertProp) other;
	return BertProp.make((myPermissions.unionWith(o.permissions())), (myEndorsements.unionWith(o.endorsements())), (mySensorWaitingFlag || (o.isSensorWaiting())), (myCannotPartializeFlag || (o.isNotPartializable())));
/*
udanax-top.st:38251:BertProp methodsFor: 'accessing'!
{Prop} with: other {Prop}
	| o {BertProp} |
	o _ other cast: BertProp.
	^BertProp make: (myPermissions unionWith: o permissions)
		with: (myEndorsements unionWith: o endorsements)
		with: (mySensorWaitingFlag or: [o isSensorWaiting])
		with: (myCannotPartializeFlag or: [o isNotPartializable])!
*/
}
public BertProp(XnRegion permissions, XnRegion endorsements, boolean isSensorWaiting, boolean isNotPartializable) {
	super();
	myPermissions = permissions;
	myEndorsements = endorsements;
	mySensorWaitingFlag = isSensorWaiting;
	myCannotPartializeFlag = isNotPartializable;
/*
udanax-top.st:38261:BertProp methodsFor: 'creation'!
create: permissions {XnRegion of: ID} 
	with: endorsements {XnRegion of: ID} 
	with: isSensorWaiting {BooleanVar} 
	with: isNotPartializable {BooleanVar}
	super create.
	myPermissions _ permissions.
	myEndorsements _ endorsements.
	mySensorWaitingFlag _ isSensorWaiting.
	myCannotPartializeFlag _ isNotPartializable.!
*/
}
public int actualHashForEqual() {
	return myPermissions.hashForEqual() ^ myEndorsements.hashForEqual();
/*
udanax-top.st:38273:BertProp methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myPermissions hashForEqual bitXor: myEndorsements hashForEqual!
*/
}
/**
 * Does this do the right thing.
 */
public boolean isEmpty() {
	Someone.knownBug();
	return myEndorsements.isEmpty() && (myPermissions.isEmpty());
/*
udanax-top.st:38276:BertProp methodsFor: 'testing'!
{BooleanVar} isEmpty
	"Does this do the right thing."
	
	self knownBug.
	^myEndorsements isEmpty and: [myPermissions isEmpty]!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof BertProp) {
		BertProp b = (BertProp) other;
		return (b.endorsements().isEqual(myEndorsements)) && ((b.permissions().isEqual(myPermissions)) && (b.isSensorWaiting() == mySensorWaitingFlag && (b.isNotPartializable() == myCannotPartializeFlag)));
	}
	else {
		return false;
	}
/*
udanax-top.st:38282:BertProp methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: BertProp into: [:b |
			^(b endorsements isEqual: myEndorsements)
			 and: [(b permissions isEqual: myPermissions)
			 and: [b isSensorWaiting == mySensorWaitingFlag
			 and: [b isNotPartializable == myCannotPartializeFlag]]]]
		others: [^false].
	^ false "compiler fodder"!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(P: ");
	oo.print(myPermissions);
	oo.print("; E: ");
	oo.print(myEndorsements);
	if (mySensorWaitingFlag) {
		oo.print("; sensor");
	}
	if (myCannotPartializeFlag) {
		oo.print("; cannot partialize");
	}
	oo.print(")");
/*
udanax-top.st:38295:BertProp methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(P: ' << myPermissions << '; E: ' << myEndorsements.
	mySensorWaitingFlag ifTrue: [oo << '; sensor'].
	myCannotPartializeFlag ifTrue: [oo << '; cannot partialize'].
	oo << ')'!
*/
}
public PropJoint joint() {
	return BertPropJoint.make((Joint.make(myPermissions)), (Joint.make(myEndorsements)), mySensorWaitingFlag, myCannotPartializeFlag);
/*
udanax-top.st:38303:BertProp methodsFor: 'smalltalk: suspended'!
{PropJoint} joint
	^BertPropJoint
		make: (Joint make: myPermissions)
		with: (Joint make: myEndorsements)
		with: mySensorWaitingFlag
		with: myCannotPartializeFlag!
*/
}
public BertProp(Rcvr receiver) {
	super(receiver);
	myPermissions = (XnRegion) receiver.receiveHeaper();
	myEndorsements = (XnRegion) receiver.receiveHeaper();
	mySensorWaitingFlag = receiver.receiveBooleanVar();
	myCannotPartializeFlag = receiver.receiveBooleanVar();
/*
udanax-top.st:38312:BertProp methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myPermissions _ receiver receiveHeaper.
	myEndorsements _ receiver receiveHeaper.
	mySensorWaitingFlag _ receiver receiveBooleanVar.
	myCannotPartializeFlag _ receiver receiveBooleanVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myPermissions);
	xmtr.sendHeaper(myEndorsements);
	xmtr.sendBooleanVar(mySensorWaitingFlag);
	xmtr.sendBooleanVar(myCannotPartializeFlag);
/*
udanax-top.st:38319:BertProp methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myPermissions.
	xmtr sendHeaper: myEndorsements.
	xmtr sendBooleanVar: mySensorWaitingFlag.
	xmtr sendBooleanVar: myCannotPartializeFlag.!
*/
}
public static BertProp cannotPartializeProp() {
	return BertProp.make(IDSpace.global().emptyRegion(), ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementSpace().emptyRegion(), false, true);
/*
udanax-top.st:38335:BertProp class methodsFor: 'creation'!
{BertProp} cannotPartializeProp
	[BeGrandMap] USES.
	^BertProp make: IDSpace global emptyRegion
		with: CurrentGrandMap fluidGet endorsementSpace emptyRegion
		with: false
		with: true!
*/
}
public static BertProp detectorWaitingProp() {
	return BertProp.make(((BeGrandMap) CurrentGrandMap.fluidGet()).globalIDSpace().emptyRegion(), ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementSpace().emptyRegion(), true, false);
/*
udanax-top.st:38342:BertProp class methodsFor: 'creation'!
{BertProp} detectorWaitingProp
	^BertProp make: CurrentGrandMap fluidGet globalIDSpace emptyRegion
			with: CurrentGrandMap fluidGet endorsementSpace emptyRegion
			with: true
			with: false!
*/
}
public static BertProp endorsementsProp(XnRegion endorsements) {
	return BertProp.make(IDSpace.global().emptyRegion(), endorsements, false, false);
/*
udanax-top.st:38349:BertProp class methodsFor: 'creation'!
{BertProp} endorsementsProp: endorsements {XnRegion}
	^BertProp make: IDSpace global emptyRegion
		with: endorsements
		with: false
		with: false!
*/
}
public static BertProp make() {
	if (TheIdentityBertProp == null) {
		TheIdentityBertProp = BertProp.make(IDSpace.global().emptyRegion(), ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementSpace().emptyRegion(), false, false);
	}
	return TheIdentityBertProp;
/*
udanax-top.st:38356:BertProp class methodsFor: 'creation'!
{BertProp} make
	TheIdentityBertProp == NULL ifTrue:
		[TheIdentityBertProp := BertProp make: IDSpace global emptyRegion
			with: CurrentGrandMap fluidGet endorsementSpace emptyRegion
			with: false
			with: false].
	^TheIdentityBertProp!
*/
}
public static BertProp make(XnRegion permissions, XnRegion endorsements, boolean isSensorWaiting, boolean isNotPartializable) {
	return new BertProp(permissions, endorsements, isSensorWaiting, isNotPartializable);
/*
udanax-top.st:38365:BertProp class methodsFor: 'creation'!
make: permissions {XnRegion of: ID} 
	with: endorsements {XnRegion} 
	with: isSensorWaiting {BooleanVar} 
	with: isNotPartializable {BooleanVar} 
	^self
		create: permissions
		with: endorsements
		with: isSensorWaiting
		with: isNotPartializable!
*/
}
public static BertProp permissionsProp(XnRegion iDs) {
	return BertProp.make(iDs, ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementSpace().emptyRegion(), false, false);
/*
udanax-top.st:38375:BertProp class methodsFor: 'creation'!
{BertProp} permissionsProp: iDs {XnRegion of: ID}
	^BertProp make: iDs
		with: CurrentGrandMap fluidGet endorsementSpace emptyRegion
		with: false
		with: false!
*/
}
public static void linkTimeNonInherited() {
	TheIdentityBertProp = null;
/*
udanax-top.st:38383:BertProp class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	TheIdentityBertProp _ NULL.!
*/
}
/**
 * @deprecated
 */
public static BertProp sensorWaitingProp() {
	throw new PasseException();
/*
udanax-top.st:38388:BertProp class methodsFor: 'smalltalk: passe'!
{BertProp} sensorWaitingProp
	self passe!
*/
}
public BertProp() {
/*

Generated during transformation
*/
}
}
