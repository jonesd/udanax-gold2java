/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.EmptyMapping;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class EmptyMapping extends Mapping {

	protected CoordinateSpace myCS;
	protected CoordinateSpace myRS;
	protected static Mapping LastEmptyMapping;
	protected static CoordinateSpace LastEmptyMappingCoordinateSpace;
	protected static CoordinateSpace LastEmptyMappingRangeSpace;
/*
udanax-top.st:30122:
Mapping subclass: #EmptyMapping
	instanceVariableNames: '
		myCS {CoordinateSpace}
		myRS {CoordinateSpace}'
	classVariableNames: '
		LastEmptyMapping {Mapping} 
		LastEmptyMappingCoordinateSpace {CoordinateSpace} 
		LastEmptyMappingRangeSpace {CoordinateSpace} '
	poolDictionaries: ''
	category: 'Xanadu-Spaces'!
*/
/*
udanax-top.st:30131:
(EmptyMapping getOrMakeCxxClassDescription)
	friends:
'/- friends for class EmptyMapping -/
friend SPTR(Mapping) emptyMapping (CoordinateSpace * cs, CoordinateSpace * rs);
';
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
/*
udanax-top.st:30263:
EmptyMapping class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:30266:
(EmptyMapping getOrMakeCxxClassDescription)
	friends:
'/- friends for class EmptyMapping -/
friend SPTR(Mapping) emptyMapping (CoordinateSpace * cs, CoordinateSpace * rs);
';
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EmptyMapping.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public CoordinateSpace coordinateSpace() {
	return myCS;
/*
udanax-top.st:30140:EmptyMapping methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^myCS!
*/
}
public XnRegion domain() {
	return coordinateSpace().emptyRegion();
/*
udanax-top.st:30144:EmptyMapping methodsFor: 'accessing'!
{XnRegion} domain
	^ self coordinateSpace emptyRegion.!
*/
}
public Dsp fetchDsp() {
	return null;
/*
udanax-top.st:30148:EmptyMapping methodsFor: 'accessing'!
{Dsp | NULL} fetchDsp
	^NULL!
*/
}
public boolean isComplete() {
	return true;
/*
udanax-top.st:30151:EmptyMapping methodsFor: 'accessing'!
{BooleanVar} isComplete
	^true!
*/
}
public boolean isIdentity() {
	return false;
/*
udanax-top.st:30155:EmptyMapping methodsFor: 'accessing'!
{BooleanVar} isIdentity
	^false!
*/
}
public XnRegion range() {
	return rangeSpace().emptyRegion();
/*
udanax-top.st:30159:EmptyMapping methodsFor: 'accessing'!
{XnRegion} range
	^ self rangeSpace emptyRegion.!
*/
}
public CoordinateSpace rangeSpace() {
	return myRS;
/*
udanax-top.st:30162:EmptyMapping methodsFor: 'accessing'!
{CoordinateSpace} rangeSpace
	^myRS!
*/
}
public ImmuSet simpleMappings() {
	return ImmuSet.make();
/*
udanax-top.st:30166:EmptyMapping methodsFor: 'accessing'!
{ImmuSet of: Mapping} simpleMappings
	^ ImmuSet make!
*/
}
public ImmuSet simpleRegionMappings() {
	return ImmuSet.make().with(this);
/*
udanax-top.st:30170:EmptyMapping methodsFor: 'accessing'!
{ImmuSet of: Mapping} simpleRegionMappings
	^ ImmuSet make with: self.!
*/
}
public Position inverseOf(Position pos) {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_RANGE);
/*
udanax-top.st:30176:EmptyMapping methodsFor: 'transforming'!
{Position} inverseOf: pos {Position unused}
	Heaper BLAST: #NotInRange.
	^ NULL!
*/
}
public XnRegion inverseOfAll(XnRegion reg) {
	return coordinateSpace().emptyRegion();
/*
udanax-top.st:30181:EmptyMapping methodsFor: 'transforming'!
{XnRegion} inverseOfAll: reg {XnRegion unused}
	^ self coordinateSpace emptyRegion.!
*/
}
public Position of(Position pos) {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_DOMAIN);
/*
udanax-top.st:30185:EmptyMapping methodsFor: 'transforming'!
{Position} of: pos {Position unused}
	Heaper BLAST: #NotInDomain.
	^ NULL!
*/
}
public XnRegion ofAll(XnRegion reg) {
	return rangeSpace().emptyRegion();
/*
udanax-top.st:30190:EmptyMapping methodsFor: 'transforming'!
{XnRegion} ofAll: reg {XnRegion unused}
	^self rangeSpace emptyRegion.!
*/
}
public EmptyMapping(CoordinateSpace cs, CoordinateSpace rs) {
	super();
	myCS = cs;
	myRS = rs;
/*
udanax-top.st:30196:EmptyMapping methodsFor: 'private: private creation'!
create: cs {CoordinateSpace} with: rs {CoordinateSpace}
	super create.
	myCS _ cs.
	myRS _ rs.!
*/
}
public void printOn(PrintWriter stream) {
	stream.print(getAboraClass().name());
	stream.print("()");
/*
udanax-top.st:30203:EmptyMapping methodsFor: 'printing'!
{void} printOn: stream {ostream reference}
	stream << self getCategory name.
	stream << '()'!
*/
}
public int actualHashForEqual() {
	return HashHelper.hashForEqual(this.getClass());
/*
udanax-top.st:30210:EmptyMapping methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^#cat.U.EmptyMapping hashForEqual!
*/
}
/**
 * This, and the CompositeMapping version, don't check CoordinateSpaces.  Should they?
 */
public boolean isEqual(Heaper other) {
	return (other instanceof EmptyMapping);
/*
udanax-top.st:30213:EmptyMapping methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	"This, and the CompositeMapping version, don't check CoordinateSpaces.  Should they?"
	^(other isKindOf: EmptyMapping)!
*/
}
public Mapping appliedAfter(Dsp dsp) {
	return this;
/*
udanax-top.st:30219:EmptyMapping methodsFor: 'operations'!
{Mapping} appliedAfter: dsp {Dsp unused}
	^self!
*/
}
public Mapping inverse() {
	return Mapping.makeCoordinateSpace(rangeSpace(), domainSpace());
/*
udanax-top.st:30223:EmptyMapping methodsFor: 'operations'!
{Mapping} inverse
	^Mapping make.CoordinateSpace: self rangeSpace
		with.CoordinateSpace: self domainSpace!
*/
}
public Mapping preCompose(Dsp dsp) {
	return this;
/*
udanax-top.st:30228:EmptyMapping methodsFor: 'operations'!
{Mapping} preCompose: dsp {Dsp unused}
	^ self!
*/
}
public Mapping restrict(XnRegion region) {
	return this;
/*
udanax-top.st:30232:EmptyMapping methodsFor: 'operations'!
{Mapping} restrict: region {XnRegion unused}
	^self!
*/
}
public Mapping restrictRange(XnRegion region) {
	return this;
/*
udanax-top.st:30236:EmptyMapping methodsFor: 'operations'!
{Mapping} restrictRange: region {XnRegion unused}
	^self!
*/
}
public Mapping transformedBy(Dsp dsp) {
	return this;
/*
udanax-top.st:30240:EmptyMapping methodsFor: 'operations'!
{Mapping} transformedBy: dsp {Dsp unused}
	^ self!
*/
}
public Mapping fetchCombine(Mapping mapping) {
	return mapping;
/*
udanax-top.st:30246:EmptyMapping methodsFor: 'protected: protected'!
{Mapping} fetchCombine: mapping {Mapping}
	^ mapping!
*/
}
public EmptyMapping(Rcvr receiver) {
	super(receiver);
	myCS = (CoordinateSpace) receiver.receiveHeaper();
	myRS = (CoordinateSpace) receiver.receiveHeaper();
/*
udanax-top.st:30252:EmptyMapping methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCS _ receiver receiveHeaper.
	myRS _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCS);
	xmtr.sendHeaper(myRS);
/*
udanax-top.st:30257:EmptyMapping methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCS.
	xmtr sendHeaper: myRS.!
*/
}
public static void linkTimeNonInherited() {
	LastEmptyMapping = null;
	LastEmptyMappingCoordinateSpace = null;
	LastEmptyMappingRangeSpace = null;
/*
udanax-top.st:30275:EmptyMapping class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	LastEmptyMapping _ NULL.
	LastEmptyMappingCoordinateSpace _ NULL.
	LastEmptyMappingRangeSpace _ NULL.!
*/
}
public static Mapping make(CoordinateSpace cs, CoordinateSpace rs) {
	if (LastEmptyMapping == null || ( ! (cs.isEqual(LastEmptyMappingCoordinateSpace)) || ( ! (rs.isEqual(LastEmptyMappingRangeSpace))))) {
		LastEmptyMappingCoordinateSpace = cs;
		LastEmptyMappingRangeSpace = rs;
		LastEmptyMapping = new EmptyMapping(cs, rs);
	}
	return LastEmptyMapping;
/*
udanax-top.st:30282:EmptyMapping class methodsFor: 'pseudoconstructor'!
{Mapping} make: cs {CoordinateSpace} with: rs {CoordinateSpace}
	(LastEmptyMapping == NULL
	 or: [(cs isEqual: LastEmptyMappingCoordinateSpace) not
	 or: [(rs isEqual: LastEmptyMappingRangeSpace) not]])
		ifTrue:
			[LastEmptyMappingCoordinateSpace _ cs.
			LastEmptyMappingRangeSpace _ rs.
			LastEmptyMapping _ EmptyMapping create: cs with: rs].
	^ LastEmptyMapping!
*/
}
public EmptyMapping() {
/*

Generated during transformation
*/
}
}
