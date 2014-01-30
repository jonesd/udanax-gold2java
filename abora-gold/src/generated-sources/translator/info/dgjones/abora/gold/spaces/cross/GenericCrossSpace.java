/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.cross;

import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.cross.CrossMapping;
import info.dgjones.abora.gold.cross.CrossOrderSpec;
import info.dgjones.abora.gold.cross.GenericCrossDsp;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.ActualTuple;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.cross.GenericCrossRegion;
import info.dgjones.abora.gold.spaces.cross.GenericCrossSpace;
import info.dgjones.abora.gold.spaces.cross.Tuple;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Default implementation of cross coordinate space.
 * was NOT.A.TYPE but that prevented compilation
 */
public class GenericCrossSpace extends CrossSpace {

/*
udanax-top.st:14769:
CrossSpace subclass: #GenericCrossSpace
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Cross'!
*/
/*
udanax-top.st:14773:
GenericCrossSpace comment:
'Default implementation of cross coordinate space.
was NOT.A.TYPE but that prevented compilation'!
*/
/*
udanax-top.st:14776:
(GenericCrossSpace getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #PSEUDO.COPY; yourself)!
*/
/*
udanax-top.st:14860:
GenericCrossSpace class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:14863:
(GenericCrossSpace getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #PSEUDO.COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GenericCrossSpace.class).setAttributes( new Set().add("CONCRETE").add("PSEUDOCOPY"));
/*

Generated during transformation: AddMethod
*/
}
public Mapping crossOfMappings(PtrArray subMappings) {
	if (subMappings == null) {
		return CrossMapping.make(this);
	}
	for (int i = 0; i < subMappings.count(); i ++ ) {
		Mapping subM;
		subM = (Mapping) (subMappings.fetch(i));
		if (subM != null && ( ! (subM instanceof Dsp))) {
			MarkM.shouldImplement();
		}
	}
	return CrossMapping.make(this, subMappings);
/*
udanax-top.st:14781:GenericCrossSpace methodsFor: 'making'!
{Mapping} crossOfMappings: subMappings {(PtrArray of: Mapping | NULL) default: NULL}
	subMappings == NULL ifTrue:
		[^CrossMapping make: self].
	Int32Zero almostTo: subMappings count do: [:i {Int32} |
		| subM {Mapping | NULL} |
		subM := (subMappings fetch: i) cast: Mapping.
		(subM ~~ NULL and: [(subM isKindOf: Dsp) not]) ifTrue:
			[MarkM shouldImplement]].
	^CrossMapping make: self with: subMappings!
*/
}
public CrossOrderSpec crossOfOrderSpecs(PtrArray subOrderings, PrimIntArray subSpaceOrdering) {
	return CrossOrderSpec.make(this, subOrderings, subSpaceOrdering);
/*
udanax-top.st:14792:GenericCrossSpace methodsFor: 'making'!
{CrossOrderSpec} crossOfOrderSpecs: subOrderings {(PtrArray of: OrderSpec | NULL) default: NULL}
	with: subSpaceOrdering {PrimIntArray default: NULL}
	
	^CrossOrderSpec make: self with: subOrderings with: subSpaceOrdering!
*/
}
public Tuple crossOfPositions(PtrArray coordinates) {
	return ActualTuple.make(coordinates);
/*
udanax-top.st:14797:GenericCrossSpace methodsFor: 'making'!
{Tuple} crossOfPositions: coordinates {PtrArray of: Position}
	
	^ActualTuple make: coordinates!
*/
}
public CrossRegion crossOfRegions(PtrArray subRegions) {
	PtrArray result;
	result = (PtrArray) subRegions.copy();
	for (int dimension = 0; dimension < result.count(); dimension ++ ) {
		if ((result.fetch(dimension)) == null) {
			result.store(dimension, (axis(dimension)).fullRegion());
		}
		else {
			if (((XnRegion) (result.fetch(dimension))).isEmpty()) {
				return (CrossRegion) emptyRegion();
			}
		}
	}
	return GenericCrossRegion.make(this, 1, result);
/*
udanax-top.st:14801:GenericCrossSpace methodsFor: 'making'!
{CrossRegion} crossOfRegions: subRegions {PtrArray of: XnRegion | NULL}
	
	| result {PtrArray of: XnRegion} |
	result := subRegions copy cast: PtrArray.
	Int32Zero almostTo: result count do: [ :dimension {Int32} |
		(result fetch: dimension) == NULL ifTrue:
			[result at: dimension
				store: (self axis: dimension) fullRegion]
		ifFalse: [((result fetch: dimension) cast: XnRegion) isEmpty ifTrue:
			[^self emptyRegion cast: CrossRegion]]].
	^GenericCrossRegion make: self with: 1 with: result!
*/
}
public CrossRegion extrusion(int dimension, XnRegion subRegion) {
	PtrArray projs;
	if (subRegion.isEmpty()) {
		return (CrossRegion) emptyRegion();
	}
	projs = PtrArray.nulls(mySubSpaces.count());
	for (int i = 0; i < mySubSpaces.count(); i ++ ) {
		if (i == dimension) {
			projs.store(i, subRegion);
		}
		else {
			projs.store(i, ((CoordinateSpace) (mySubSpaces.fetch(i))).fullRegion());
		}
	}
	return GenericCrossRegion.make(this, 1, projs);
/*
udanax-top.st:14813:GenericCrossSpace methodsFor: 'making'!
{CrossRegion} extrusion: dimension {Int32} with: subRegion {XnRegion}
	
	| projs {PtrArray of: XnRegion} |
	subRegion isEmpty ifTrue: [^self emptyRegion cast: CrossRegion].
	projs := PtrArray nulls: mySubSpaces count.
	Int32Zero almostTo: mySubSpaces count do: [ :i {Int32} |
		i = dimension ifTrue:
			[projs at: i store: subRegion]
		ifFalse:
			[projs at: i
				store: ((mySubSpaces fetch: i) cast: CoordinateSpace) fullRegion]].
	^GenericCrossRegion make: self with: 1 with: projs!
*/
}
public GenericCrossSpace(PtrArray subSpaces) {
	super(subSpaces);
	finishCreate((GenericCrossRegion.empty(this)), (GenericCrossRegion.full(this, subSpaces)), (GenericCrossDsp.identity(this, subSpaces)), (CrossOrderSpec.fetchAscending(this, subSpaces)), (CrossOrderSpec.fetchDescending(this, subSpaces)));
/*
udanax-top.st:14828:GenericCrossSpace methodsFor: 'private: creation'!
create: subSpaces {PtrArray of: CoordinateSpace}
	
	super create: subSpaces.
	self finishCreate: (GenericCrossRegion empty: self)
		with: (GenericCrossRegion full: self with: subSpaces)
		with: (GenericCrossDsp identity: self with: subSpaces)
		with: (CrossOrderSpec fetchAscending: self with: subSpaces)
		with: (CrossOrderSpec fetchDescending: self with: subSpaces).!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("<");
	for (int i = 0; i < mySubSpaces.count(); i ++ ) {
		if (i > 0) {
			oo.print(" x ");
		}
		oo.print((mySubSpaces.fetch(i)));
	}
	oo.print(">");
/*
udanax-top.st:14839:GenericCrossSpace methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << '<'.
	Int32Zero almostTo: mySubSpaces count do: [ :i {Int32} |
		i > Int32Zero ifTrue: [oo << ' x '].
		oo << (mySubSpaces fetch: i)].
	oo << '>'!
*/
}
public void sendGenericCrossSpaceTo(Xmtr xmtr) {
	xmtr.sendHeaper(mySubSpaces);
/*
udanax-top.st:14849:GenericCrossSpace methodsFor: 'hooks:'!
{void SEND.HOOK} sendGenericCrossSpaceTo: xmtr {Xmtr}
	xmtr sendHeaper: mySubSpaces.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	sendGenericCrossSpaceTo(xmtr);
/*
udanax-top.st:14855:GenericCrossSpace methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	
	self sendGenericCrossSpaceTo: xmtr.!
*/
}
public static Heaper makeRcvr(Rcvr rcvr) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:14868:GenericCrossSpace class methodsFor: 'rcvr pseudoconstructors'!
{Heaper} make.Rcvr: rcvr {Rcvr}
	^(GenericCrossSpace new.Become: ((rcvr cast: SpecialistRcvr) makeIbid: GenericCrossSpace))
		 create: (rcvr receiveHeaper cast: PtrArray)!
*/
}
public static CrossSpace make(PtrArray subSpaces) {
	return new GenericCrossSpace(subSpaces);
/*
udanax-top.st:14875:GenericCrossSpace class methodsFor: 'pseudoconstructors'!
{CrossSpace} make: subSpaces {PtrArray of: CoordinateSpace}
	^GenericCrossSpace create: subSpaces!
*/
}
public GenericCrossSpace() {
/*

Generated during transformation
*/
}
public GenericCrossSpace(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
