/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cross;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.cross.CrossMapping;
import info.dgjones.abora.gold.cross.GenericCrossDsp;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * All other crossed mappings must be gotten by factoring the non-dsp aspects out into the
 * generic non-dsp mapping objects.  This class represents what remains after the factoring.
 */
public class CrossMapping extends Dsp {

/*
udanax-top.st:29169:
Dsp subclass: #CrossMapping
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cross'!
*/
/*
udanax-top.st:29173:
CrossMapping comment:
'All other crossed mappings must be gotten by factoring the non-dsp aspects out into the generic non-dsp mapping objects.  This class represents what remains after the factoring.'!
*/
/*
udanax-top.st:29175:
(CrossMapping getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:29229:
CrossMapping class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:29232:
(CrossMapping getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CrossMapping.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public XnRegion ofAll(XnRegion reg) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29180:CrossMapping methodsFor: 'transforming'!
{XnRegion} ofAll: reg {XnRegion}
	
	self subclassResponsibility!
*/
}
public Dsp compose(Dsp other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29186:CrossMapping methodsFor: 'combining'!
{Dsp} compose: other {Dsp}
	
	self subclassResponsibility!
*/
}
public Mapping inverse() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29190:CrossMapping methodsFor: 'combining'!
{Mapping} inverse
	self subclassResponsibility!
*/
}
public Dsp minus(Dsp other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29194:CrossMapping methodsFor: 'combining'!
{Dsp} minus: other {Dsp}
	self subclassResponsibility!
*/
}
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29200:CrossMapping methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	
	self subclassResponsibility!
*/
}
public boolean isIdentity() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29204:CrossMapping methodsFor: 'accessing'!
{BooleanVar} isIdentity
	
	self subclassResponsibility!
*/
}
/**
 * The Dsp applied to Positions in the given subspace.
 */
public Dsp subMapping(int index) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29208:CrossMapping methodsFor: 'accessing'!
{Dsp CLIENT} subMapping: index {Int32}
	"The Dsp applied to Positions in the given subspace."
	
	self subclassResponsibility!
*/
}
/**
 * The Mappings applied to Positions in each of the subspaces. Each of these is already
 * simple enough that it is either the identityMapping or a visible subclass like
 * IntegerMapping.
 */
public PtrArray subMappings() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29213:CrossMapping methodsFor: 'accessing'!
{PtrArray CLIENT of: Dsp} subMappings
	"The Mappings applied to Positions in each of the subspaces. Each of these is already simple enough that it is either the identityMapping or a visible subclass like IntegerMapping."
	
	self subclassResponsibility!
*/
}
/**
 * @deprecated
 */
public Dsp subDsp(int index) {
	throw new PasseException();
/*
udanax-top.st:29220:CrossMapping methodsFor: 'smalltalk: passe'!
{Dsp} subDsp: index {Int32}
	self passe!
*/
}
/**
 * @deprecated
 */
public PtrArray subDsps() {
	throw new PasseException();
/*
udanax-top.st:29224:CrossMapping methodsFor: 'smalltalk: passe'!
{PtrArray of: Dsp} subDsps
	self passe "subMappings"!
*/
}
public static CrossMapping make(CrossSpace space, PtrArray subDsps) {
	PtrArray subDs;
	subDs = PtrArray.nulls(space.axisCount());
	for (int i = 0; i < subDs.count(); i ++ ) {
		subDs.store(i, (space.axis(i)).identityDsp());
	}
	if (subDsps != null) {
		for (int i1 = 0; i1 < subDs.count(); i1 ++ ) {
			Dsp subDsp;
			if ((subDsp = (Dsp) (subDsps.fetch(i1))) != null) {
				subDs.store(i1, subDsp);
			}
		}
	}
	return new GenericCrossDsp(space, subDs);
/*
udanax-top.st:29237:CrossMapping class methodsFor: 'pseudoconstructors'!
make: space {CrossSpace} with: subDsps {(PtrArray of: Dsp | NULL) default: NULL}
	| subDs {PtrArray of: Dsp} |
	subDs := PtrArray nulls: space axisCount.
	Int32Zero almostTo: subDs count do: [:i {Int32} |
		subDs at: i store: (space axis: i) identityDsp].
	
	subDsps ~~ NULL ifTrue:
		[Int32Zero almostTo: subDs count do: [:i {Int32} |
			| subDsp {Dsp | NULL} |
			(subDsp := (subDsps fetch: i) cast: Dsp) ~~ NULL ifTrue:
				[subDs at: i store: subDsp]]].
	
	^GenericCrossDsp create: space with: subDs!
*/
}
public static Mapping make(Object space) {
	return make(space, null);
/*
udanax-top.st:29254:CrossMapping class methodsFor: 'smalltalk: defaults'!
make: space
	^self make: space with: NULL!
*/
}
/**
 * {Dsp CLIENT} subMapping: index {Int32}
 * {PtrArray CLIENT of: Dsp} subMappings
 */
public static void infostProtocol() {
/*
udanax-top.st:29260:CrossMapping class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Dsp CLIENT} subMapping: index {Int32}
{PtrArray CLIENT of: Dsp} subMappings
"!
*/
}
public CrossMapping() {
/*

Generated during transformation
*/
}
public CrossMapping(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
