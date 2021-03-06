/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.PackOBits;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.urdi.WriteVariableArrayStream;
import info.dgjones.abora.gold.xcvr.Binary2XcvrMaker;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Implementation note:
 * An ID exists within a particular IDSpace, and is generated by a particular Server. It
 * holds onto the space and the Server which created it, along with a number identifying the
 * ID uniquely. If mySpace is NULL, then the ID is in the global IDSpace. If myBackend is
 * NULL, then this ID was generated by the current Server (unless myNumber is negative, in
 * which case it is considered to have been generated by the "global" backend). If myBackend
 * is non-NULL, then myNumber must be non-negative.
 */
public class ID extends Position {

	protected IDSpace mySpace;
	protected Sequence myBackend;
	protected int myNumber;
/*
udanax-top.st:31611:
Position subclass: #ID
	instanceVariableNames: '
		mySpace {IDSpace | NULL}
		myBackend {Sequence | NULL}
		myNumber {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:31618:
ID comment:
'Implementation note:
An ID exists within a particular IDSpace, and is generated by a particular Server. It holds onto the space and the Server which created it, along with a number identifying the ID uniquely. If mySpace is NULL, then the ID is in the global IDSpace. If myBackend is NULL, then this ID was generated by the current Server (unless myNumber is negative, in which case it is considered to have been generated by the "global" backend). If myBackend is non-NULL, then myNumber must be non-negative.'!
*/
/*
udanax-top.st:31622:
(ID getOrMakeCxxClassDescription)
	friends:
'friend class IDRegion;
friend class IDStepper;
friend class IDUpOrder;
friend class IDTester;
friend class IDSpace;
';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
/*
udanax-top.st:31762:
ID class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:31765:
(ID getOrMakeCxxClassDescription)
	friends:
'friend class IDRegion;
friend class IDStepper;
friend class IDUpOrder;
friend class IDTester;
friend class IDSpace;
';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ID.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public XnRegion asRegion() {
	if (myBackend == null) {
		return IDRegion.make(mySpace, (IntegerRegion.make(myNumber)), null, false);
	}
	else {
		MuTable others;
		others = MuTable.make(SequenceSpace.make());
		others.introduce(myBackend, (IntegerRegion.make(myNumber)));
		return IDRegion.make(mySpace, IntegerRegion.make(), others.asImmuTable(), false);
	}
/*
udanax-top.st:31634:ID methodsFor: 'accessing'!
{XnRegion} asRegion
	myBackend == NULL ifTrue:
		[^IDRegion make: mySpace
			with: (IntegerRegion make: myNumber)
			with: NULL
			with: false]
	ifFalse:
		[ | others {MuTable of: Sequence and: IntegerRegion} |
		others := MuTable make: SequenceSpace make.
		others at: myBackend introduce: (IntegerRegion make: myNumber).
		^IDRegion make: mySpace
			with: IntegerRegion make
			with: others asImmuTable
			with: false].!
*/
}
public CoordinateSpace coordinateSpace() {
	if (mySpace == null) {
		return IDSpace.global();
	}
	return mySpace;
/*
udanax-top.st:31650:ID methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	mySpace == NULL ifTrue:
		[^IDSpace global].
	^mySpace!
*/
}
/**
 * Essential. Export this iD in a form which can be handed to Server::importID on any Server
 * to generate the same ID
 */
public UInt8Array export() {
	SpecialistXmtr xmtr;
	WriteVariableArrayStream result;
	result = WriteVariableArrayStream.make(200);
	xmtr = Binary2XcvrMaker.make().makeXmtr((TransferSpecialist.make(Cookbook.make())), result);
	ID.exportSequence(xmtr, ((IDSpace) coordinateSpace()).backend());
	xmtr.sendIntegerVar(((IDSpace) coordinateSpace()).spaceNumber());
	ID.exportSequence(xmtr, backend());
	xmtr.sendIntegerVar(number());
	return result.array();
/*
udanax-top.st:31656:ID methodsFor: 'accessing'!
{UInt8Array CLIENT} export
	"Essential. Export this iD in a form which can be handed to Server::importID on any Server to generate the same ID"
	
	| xmtr {SpecialistXmtr} result {WriteVariableArrayStream} |
	result := WriteVariableArrayStream make: 200.
	xmtr := Binary2XcvrMaker make
		makeXmtr: (TransferSpecialist make: Cookbook make)
		with: result.
	ID exportSequence: xmtr
		with: (self coordinateSpace cast: IDSpace) backend.
	xmtr  sendIntegerVar: (self coordinateSpace cast: IDSpace) spaceNumber.
	ID exportSequence: xmtr
		with: self backend.
	xmtr sendIntegerVar: self number.
	^result array!
*/
}
public int actualHashForEqual() {
	int result;
	result = getCategory().hashForEqual();
	if (mySpace != null) {
		result = result ^ mySpace.hashForEqual();
	}
	if (myBackend != null) {
		result = result ^ myBackend.hashForEqual();
	}
	return result ^ HashHelper.hashForEqual(myNumber);
/*
udanax-top.st:31674:ID methodsFor: 'comparing'!
{UInt32} actualHashForEqual
	| result {UInt32} |
	result := self getCategory hashForEqual.
	mySpace ~~ NULL ifTrue:
		[result := result bitXor: mySpace hashForEqual].
	myBackend ~~ NULL ifTrue:
		[result := result bitXor: myBackend hashForEqual].
	^result bitXor: myNumber DOThashForEqual!
*/
}
public boolean isEqual(Heaper heaper) {
	if (heaper instanceof ID) {
		ID other = (ID) heaper;
		if (mySpace == null) {
			if ( ! (other.fetchSpace() == null)) {
				return false;
			}
		}
		else {
			if ( ! (other.fetchSpace() != null && (mySpace.isEqual(other.fetchSpace())))) {
				return false;
			}
		}
		if (myBackend == null) {
			if ( ! (other.fetchBackend() == null)) {
				return false;
			}
		}
		else {
			if ( ! (other.fetchBackend() != null && (myBackend.isEqual(other.fetchBackend())))) {
				return false;
			}
		}
		return myNumber == other.number();
	}
	else {
		return false;
	}
/*
udanax-top.st:31684:ID methodsFor: 'comparing'!
{BooleanVar} isEqual: heaper {Heaper}
	heaper cast: ID into: [ :other |
		mySpace == NULL
				ifTrue: [other fetchSpace == NULL ifFalse: [^false]]
				ifFalse: [(other fetchSpace ~~ NULL
					and: [mySpace isEqual: other fetchSpace]) ifFalse: [^false]].
		myBackend == NULL
				ifTrue: [other fetchBackend == NULL ifFalse: [^false]]
				ifFalse: [(other fetchBackend ~~ NULL
					and: [myBackend isEqual: other fetchBackend]) ifFalse: [^false]].
		^ myNumber = other number]
	others:
		[^false].
	^false "fodder"!
*/
}
public ID(IDSpace space, Sequence backend, int number) {
	super();
	mySpace = space;
	myBackend = backend;
	myNumber = number;
/*
udanax-top.st:31702:ID methodsFor: 'protected: create'!
create: space {IDSpace | NULL} with: backend {Sequence | NULL} with: number {IntegerVar}
	super create.
	mySpace := space.
	myBackend := backend.
	myNumber := number.!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(((IDSpace) coordinateSpace()).identifier());
	oo.print(":");
	oo.print(identifier());
/*
udanax-top.st:31711:ID methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << (self coordinateSpace cast: IDSpace) identifier << ':' << self identifier!
*/
}
/**
 * Essential. A Sequence identifying the server on which this was created
 */
public Sequence backend() {
	if (myBackend == null) {
		if (myNumber < 0) {
			return Sequence.zero();
		}
		else {
			return FeServer.identifier();
		}
	}
	else {
		return myBackend;
	}
/*
udanax-top.st:31717:ID methodsFor: 'private:'!
{Sequence} backend
	"Essential. A Sequence identifying the server on which this was created"
	
	myBackend == NULL
		ifTrue: [myNumber < IntegerVarZero
			ifTrue: [^Sequence zero]
			ifFalse: [^FeServer identifier]]
		ifFalse: [^myBackend]!
*/
}
public Sequence fetchBackend() {
	return myBackend;
/*
udanax-top.st:31726:ID methodsFor: 'private:'!
{Sequence | NULL} fetchBackend
	^myBackend!
*/
}
public IDSpace fetchSpace() {
	return mySpace;
/*
udanax-top.st:31730:ID methodsFor: 'private:'!
{IDSpace | NULL} fetchSpace
	^mySpace!
*/
}
/**
 * Essential. The number identifying this ID from all others generated by the same Server in
 * the same IDSpace.
 */
public int number() {
	return myNumber;
/*
udanax-top.st:31734:ID methodsFor: 'private:'!
{IntegerVar} number
	"Essential. The number identifying this ID from all others generated by the same Server in the same IDSpace."
	
	^myNumber!
*/
}
/**
 * A sequence of numbers which uniquely identify this ID within its space
 */
public Sequence identifier() {
	Ravi.thingToDo();
	/* get rid of this, and clients */
	return backend().withLast(myNumber);
/*
udanax-top.st:31741:ID methodsFor: 'obsolete:'!
{Sequence} identifier
	"A sequence of numbers which uniquely identify this ID within its space"
	
	Ravi thingToDo. "get rid of this, and clients"
	^self backend withLast: myNumber!
*/
}
public ID(Rcvr receiver) {
	super(receiver);
	mySpace = (IDSpace) receiver.receiveHeaper();
	myBackend = (Sequence) receiver.receiveHeaper();
	myNumber = receiver.receiveIntegerVar();
/*
udanax-top.st:31749:ID methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	mySpace _ receiver receiveHeaper.
	myBackend _ receiver receiveHeaper.
	myNumber _ receiver receiveIntegerVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(mySpace);
	xmtr.sendHeaper(myBackend);
	xmtr.sendIntegerVar(myNumber);
/*
udanax-top.st:31755:ID methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: mySpace.
	xmtr sendHeaper: myBackend.
	xmtr sendIntegerVar: myNumber.!
*/
}
public static ID make(IDSpace space, Sequence backend, int number) {
	return new ID(space, backend, number);
/*
udanax-top.st:31777:ID class methodsFor: 'module private create'!
make: space {IDSpace | NULL} with: backend {Sequence | NULL} with: number {IntegerVar}
	^ self create: space with: backend with: number!
*/
}
/**
 * Special for IDStepper - checks whether it should make backend be NULL
 */
public static ID usingx(IDSpace space, Sequence backend, int number) {
	if ((backend == null || (backend.isEqual(Sequence.zero()))) || (backend.isEqual(FeServer.identifier()))) {
		return make(space, null, number);
	}
	else {
		return make(space, backend, number);
	}
/*
udanax-top.st:31782:ID class methodsFor: 'private: pseudo constructors'!
{ID} usingx: space {IDSpace | NULL} with: backend {Sequence | NULL} with: number {IntegerVar}
	"Special for IDStepper - checks whether it should make backend be NULL"
	[BeGrandMap] USES.
	((backend == NULL
			or: [backend isEqual: Sequence zero])
			or: [backend isEqual: FeServer identifier]) 
		ifTrue: [^self make: space with: NULL with: number]
		ifFalse: [^self make: space with: backend with: number]!
*/
}
/**
 * ID key: 'test'
 * @deprecated
 */
public static ID key(String string) {
	throw new PasseException();
/*
udanax-top.st:31793:ID class methodsFor: 'smalltalk: passe'!
{ID} key: string {char star}
	"ID key: 'test'"
	self passe.!
*/
}
/**
 * @deprecated
 */
public static ID make(PackOBits pakobits) {
	throw new PasseException();
/*
udanax-top.st:31797:ID class methodsFor: 'smalltalk: passe'!
make: pakobits {PackOBits}
	self passe.!
*/
}
/**
 * @deprecated
 */
public static ID make(int left, int right) {
	throw new PasseException();
/*
udanax-top.st:31801:ID class methodsFor: 'smalltalk: passe'!
make: left {IntegerVar} with: right {IntegerVar}
	self passe.!
*/
}
/**
 * {UInt8Array CLIENT} export
 */
public static void infostProtocol() {
/*
udanax-top.st:31806:ID class methodsFor: 'smalltalk: system'!
info.stProtocol
"{UInt8Array CLIENT} export
"!
*/
}
/**
 * Essential. Take some information describing an ID and create the ID it was exported from.
 */
public static ID importx(PrimIntArray data) {
	SpecialistRcvr rcvr;
	Sequence spaceBackend;
	int spaceNumber;
	Sequence iDBackend;
	int iDNumber;
	IDSpace space;
	rcvr = Binary2XcvrMaker.make().makeRcvr((TransferSpecialist.make(Cookbook.make())), (XnReadStream.make(((UInt8Array) data))));
	spaceBackend = importSequence(rcvr);
	spaceNumber = rcvr.receiveIntegerVar();
	iDBackend = importSequence(rcvr);
	iDNumber = rcvr.receiveIntegerVar();
	space = IDSpace.make(spaceBackend, spaceNumber);
	if (space.isEqual(((BeGrandMap) CurrentGrandMap.fluidGet()).globalIDSpace())) {
		space = null;
	}
	return ID.usingx(space, iDBackend, iDNumber);
/*
udanax-top.st:31812:ID class methodsFor: 'creation'!
{ID CLIENT login} import: data {PrimIntArray}
	"Essential. Take some information describing an ID and create the ID it was exported from."
	
	| rcvr {SpecialistRcvr} spaceBackend {Sequence} spaceNumber {IntegerVar}
	  iDBackend {Sequence} iDNumber {IntegerVar} space {IDSpace} |
	rcvr := Binary2XcvrMaker make
		makeRcvr: (TransferSpecialist make: Cookbook make)
		with: (XnReadStream make: (data cast: UInt8Array)).
	spaceBackend := self importSequence: rcvr.
	spaceNumber := rcvr receiveIntegerVar.
	iDBackend := self importSequence: rcvr.
	iDNumber := rcvr receiveIntegerVar.
	space := IDSpace make: spaceBackend with: spaceNumber.
	(space isEqual: CurrentGrandMap fluidGet globalIDSpace) ifTrue:
		[space := NULL].
	^ID usingx: space with: iDBackend with: iDNumber!
*/
}
/**
 * Write a IntegerRegion onto a stream
 */
public static void exportIntegerRegion(SpecialistXmtr xmtr, IntegerRegion integers) {
	xmtr.sendBooleanVar( ! integers.isBoundedBelow());
	xmtr.sendIntegerVar(integers.secretTransitions().count());
	for (int i = 0; i < integers.secretTransitions().count(); i ++ ) {
		xmtr.sendIntegerVar((integers.secretTransitions().integerAt(i)));
	}
/*
udanax-top.st:31831:ID class methodsFor: 'private: export/import for friends'!
{void} exportIntegerRegion: xmtr {SpecialistXmtr} with: integers {IntegerRegion}
	"Write a IntegerRegion onto a stream"
	
	xmtr sendIntegerVar: integers isBoundedBelow not.
	xmtr sendIntegerVar: integers secretTransitions count.
	Int32Zero almostTo: integers secretTransitions count do: [ :i {Int32} |
		xmtr sendIntegerVar: (integers secretTransitions integerAt: i)]!
*/
}
/**
 * Write a Sequence onto a stream
 */
public static void exportSequence(SpecialistXmtr xmtr, Sequence sequence) {
	if (sequence.isZero()) {
		xmtr.sendIntegerVar(0);
		return ;
	}
	xmtr.sendIntegerVar(sequence.lastIndex() - sequence.firstIndex() + 1);
	xmtr.sendIntegerVar(sequence.firstIndex());
	for (int i = sequence.firstIndex(); i <= sequence.lastIndex(); i ++ ) {
		xmtr.sendIntegerVar((sequence.integerAt(i)));
	}
/*
udanax-top.st:31839:ID class methodsFor: 'private: export/import for friends'!
{void} exportSequence: xmtr {SpecialistXmtr} with: sequence {Sequence}
	"Write a Sequence onto a stream"
	
	sequence isZero ifTrue:
		[xmtr sendIntegerVar: IntegerVarZero.
		^VOID].
	xmtr sendIntegerVar: sequence lastIndex - sequence firstIndex + 1.
	xmtr sendIntegerVar: sequence firstIndex.
	sequence firstIndex to: sequence lastIndex do: [ :i {IntegerVar} |
		xmtr sendIntegerVar: (sequence integerAt: i)].!
*/
}
/**
 * Read a IntegerRegion from a stream
 */
public static IntegerRegion importIntegerRegion(SpecialistRcvr rcvr) {
	boolean startsInside;
	int n;
	IntegerVarArray transitions;
	startsInside = rcvr.receiveBooleanVar();
	n = rcvr.receiveIntegerVar();
	transitions = IntegerVarArray.zeros(n);
	for (int i = 0; i < n; i ++ ) {
		transitions.storeInteger(i, rcvr.receiveIntegerVar());
	}
	return IntegerRegion.usingx(startsInside, n, transitions);
/*
udanax-top.st:31850:ID class methodsFor: 'private: export/import for friends'!
{IntegerRegion} importIntegerRegion: rcvr {SpecialistRcvr}
	"Read a IntegerRegion from a stream"
	
	| startsInside {BooleanVar} n {Int32} transitions {IntegerVarArray} |
	startsInside := rcvr receiveIntegerVar DOTasLong.
	n := rcvr receiveIntegerVar DOTasLong.
	transitions := IntegerVarArray zeros: n.
	Int32Zero almostTo: n do: [ :i {Int32} |
		transitions at: i storeInteger: rcvr receiveIntegerVar].
	^IntegerRegion usingx: startsInside with: n with: transitions!
*/
}
/**
 * Read a Sequence from a stream
 */
public static Sequence importSequence(SpecialistRcvr rcvr) {
	int count;
	int shift;
	IntegerVarArray numbers;
	count = rcvr.receiveIntegerVar();
	if (count == 0) {
		return Sequence.zero();
	}
	numbers = IntegerVarArray.zeros(count);
	shift = rcvr.receiveIntegerVar();
	for (int i = 0; i < count; i ++ ) {
		numbers.storeInteger(i, rcvr.receiveIntegerVar());
	}
	return SequenceSpace.make().position(numbers, shift);
/*
udanax-top.st:31861:ID class methodsFor: 'private: export/import for friends'!
{Sequence} importSequence: rcvr {SpecialistRcvr}
	"Read a Sequence from a stream"
	
	| count {IntegerVar} shift {IntegerVar} numbers {IntegerVarArray} |
	count := rcvr receiveIntegerVar.
	count == IntegerVarZero ifTrue:
		[^Sequence zero].
	numbers := IntegerVarArray zeros: count DOTasLong.
	shift := rcvr receiveIntegerVar.
	Int32Zero almostTo: count DOTasLong do: [ :i {Int32} |
		numbers at: i storeInteger: rcvr receiveIntegerVar].
	^SequenceSpace make position: numbers with: shift!
*/
}
public ID() {
/*

Generated during transformation
*/
}
}
