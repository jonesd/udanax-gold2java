/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraCharacterSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Binary2Rcvr;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;
import java.io.StringWriter;

public class Binary2Rcvr extends SpecialistRcvr {

	protected XnReadStream myStream;
	protected int myDepth;
	protected static InstanceCache SomeRcvrs;
/*
udanax-top.st:41240:
SpecialistRcvr subclass: #Binary2Rcvr
	instanceVariableNames: '
		myStream {XnReadStream}
		myDepth {IntegerVar}'
	classVariableNames: 'SomeRcvrs {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:41246:
(Binary2Rcvr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:41473:
Binary2Rcvr class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:41476:
(Binary2Rcvr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Binary2Rcvr.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean receiveBooleanVar() {
	int result;
	startThing();
	result = myStream.getByte();
	endThing();
	if (result == 1) {
		return true;
	}
	else {
		if (result == 0) {
			return false;
		}
		else {
			throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
		}
	}
/*
udanax-top.st:41251:Binary2Rcvr methodsFor: 'receiving'!
{BooleanVar} receiveBooleanVar
	| result {BooleanVar} |
	self startThing.
	result _ myStream getByte.
	self endThing.
	result == 1 ifTrue: [^true] ifFalse: [result == Int0 ifTrue: [^false] ifFalse: [Heaper BLAST: #InvalidRequest]].
	^false "fodder"!
*/
}
public Category receiveCategory() {
	int num;
	num = getIntegerVar();
	if (num == 0) {
		myDepth = myDepth - 1;
		endThing();
		return null;
	}
	else {
		return specialist().getCategoryFor(num-1);
	}
/*
udanax-top.st:41259:Binary2Rcvr methodsFor: 'receiving'!
{Category | NULL} receiveCategory
	| num {IntegerVar} |
	num _ self getIntegerVar.
	num == IntegerVar0
		ifTrue:
			[myDepth _ myDepth - 1.
			self endThing.
			^NULL]
		ifFalse: [^self specialist getCategoryFor: num-1]!
*/
}
/**
 * Fill the array with data from the stream.
 */
public void receiveData(UInt8Array array) {
	startThing();
	for (int i = 0; i <= array.count() - 1; i ++ ) {
		array.storeUInt(i, myStream.getByte());
	}
	endThing();
/*
udanax-top.st:41270:Binary2Rcvr methodsFor: 'receiving'!
{void} receiveData: array {UInt8Array}
	"Fill the array with data from the stream."
	
	self startThing.
	Int32Zero to: array count - 1 do: [ :i {Int32} |
		array at: i storeUInt: myStream getByte].
	self endThing!
*/
}
/**
 * | result {IEEEDoubleVar} |
 * self startThing.
 * result _ Double make: self getIntegerVar with: self getIntegerVar.
 * self endThing.
 * ^result
 */
public double receiveIEEEDoubleVar() {
	throw new UnimplementedException();
/*
udanax-top.st:41278:Binary2Rcvr methodsFor: 'receiving'!
{IEEEDoubleVar} receiveIEEEDoubleVar
	"| result {IEEEDoubleVar} |
	self startThing.
	result _ Double make: self getIntegerVar with: self getIntegerVar.
	self endThing.
	^result"
	
	self unimplemented.
	^NULL!
*/
}
public int receiveInt32() {
	int result;
	startThing();
	result = getIntegerVar();
	endThing();
	return result;
/*
udanax-top.st:41288:Binary2Rcvr methodsFor: 'receiving'!
{Int32} receiveInt32 
	| result {Int32} |
	self startThing.
	result _ self getIntegerVar DOTasLong.
	self endThing.
	^result!
*/
}
public int receiveInt8() {
	int result;
	startThing();
	result = myStream.getByte();
	AboraSupport.smalltalkOnly();
	{
		if (result > 127) {
			result = result - 256;
		}
	}
	endThing();
	return result;
/*
udanax-top.st:41295:Binary2Rcvr methodsFor: 'receiving'!
{Int8} receiveInt8
	| result {Int8} | 
	self startThing.
	result _ myStream getByte.
	[result > 127 ifTrue: [result _ result - 256]] smalltalkOnly.
	self endThing.
	^result!
*/
}
public int receiveIntegerVar() {
	int result;
	startThing();
	result = getIntegerVar();
	endThing();
	return result;
/*
udanax-top.st:41303:Binary2Rcvr methodsFor: 'receiving'!
{IntegerVar} receiveIntegerVar
	| result {IntegerVar} |
	self startThing. 
	result _ self getIntegerVar.
	self endThing.
	^result!
*/
}
public String receiveString() {
	int size;
	String result;
	size = getIntegerVar();
	startThing();
	StringWriter stringWriter = new StringWriter();
	PrintWriter stream = new PrintWriter(stringWriter);
	for (int ignore = 1; ignore <= size; ignore ++ ) {
		stream.print(((char) (myStream.getByte())));
	}
	result = stringWriter.toString();
	/* Removed translateOnly */
	endThing();
	return result;
/*
udanax-top.st:41310:Binary2Rcvr methodsFor: 'receiving'!
{char star} receiveString
	| size {Int32} result {char star} |
	size _ self getIntegerVar DOTasLong.
	self startThing.
	[result _ String streamContents: [ :stream |
		1 to: size do: [:ignore {IntegerVar} |
			stream nextPut: (Character char: myStream getByte)]]] smalltalkOnly.
	'
	result = new char[size+1];
	char c;
	int soFar = 0;
	for (; soFar < size ; soFar++) {
		c = myStream->getByte();
		result[soFar] = c;
	}
	result[soFar] = ''\0'';' translateOnly.
	self endThing.
	^result!
*/
}
public int receiveUInt32() {
	int result;
	startThing();
	result = getIntegerVar();
	endThing();
	return result;
/*
udanax-top.st:41330:Binary2Rcvr methodsFor: 'receiving'!
{UInt32} receiveUInt32
	| result {UInt32} | 
	self startThing.
	result _ self getIntegerVar DOTasLong.
	self endThing.
	^result!
*/
}
public int receiveUInt8() {
	int result;
	startThing();
	result = myStream.getByte();
	endThing();
	return result;
/*
udanax-top.st:41337:Binary2Rcvr methodsFor: 'receiving'!
{UInt8} receiveUInt8
	| result {UInt8} | 
	self startThing.
	result _ myStream getByte.
	self endThing.
	^result!
*/
}
public void endOfInstance() {
	myDepth = myDepth - 1;
	endThing();
/*
udanax-top.st:41346:Binary2Rcvr methodsFor: 'protected: specialist'!
{void} endOfInstance
	myDepth _ myDepth - 1.
	self endThing!
*/
}
public void endPacket() {
	/* Transform: Rewrote body */
	myStream.getByte();
/*
udanax-top.st:41350:Binary2Rcvr methodsFor: 'protected: specialist'!
{void} endPacket
	myStream getByte == $!!
*/
}
/*
udanax-top.st:41353:Binary2Rcvr methodsFor: 'protected: specialist'!
uint8 assert: 'End of packet marker required'.
	myStream getByte == $!!
*/
/*
udanax-top.st:41354:Binary2Rcvr methodsFor: 'protected: specialist'!
uint8 assert: 'End of packet marker required'.
	super endPacket!
*/
public Category fetchStartOfInstance() {
	startThing();
	myDepth = myDepth + 1;
	return receiveCategory();
/*
udanax-top.st:41355:Binary2Rcvr methodsFor: 'protected: specialist'!
{Category | NULL} fetchStartOfInstance
	self startThing.
	myDepth _ myDepth + 1.
	^self receiveCategory!
*/
}
/**
 * A new representation that requires less shifting (eventually).
 */
public int getIntegerVar() {
	/* 
7/1 		0<7>
14/2	10<6>		<8>
21/3	110<5>		<16>
28/4	1110<4>		<24>
35/5	11110<3>	<32>
42/6	111110<2>	<40>
49/7	1111110<1>	<48>
56/8	11111110 	<56>
?/?	11111111  <humber count>
 */
	int bytex;
	XnReadStream stream;
	int mask;
	int count;
	int num;
	/* count is bytes following first word or -1 if bignum meaning next byte is humber for actual count */
	stream = stream();
	bytex = stream.getByte();
	if (bytex <= 63) {
		return bytex;
	}
	if (bytex <= 127) {
		return bytex-128;
	}
	if (bytex <= 191) {
		mask = 63;
		count = 1;
	}
	else {
		if (bytex <= 223) {
			mask = 31;
			count = 2;
		}
		else {
			if (bytex <= 239) {
				mask = 15;
				count = 3;
			}
			else {
				if (bytex <= 247) {
					mask = 7;
					count = 4;
				}
				else {
					throw new UnimplementedException();
				}
			}
		}
	}
	bytex = bytex & mask;
	if ((bytex & (( ~ mask >> 1) & mask)) != 0) {
		bytex = bytex | ~ mask;
		num = -1;
	}
	else {
		num = 0;
		if ((count > 3) && ((bytex != (bytex & mask)))) {
			throw new UnimplementedException();
		}
	}
	num = (num << 8) + bytex;
	for (int i = 1; i <= count; i ++ ) {
		num = (num << 8) + stream.getByte();
	}
	return num;
/*
udanax-top.st:41361:Binary2Rcvr methodsFor: 'protected: specialist'!
{IntegerVar} getIntegerVar
	"A new representation that requires less shifting (eventually)."
"
7/1 		0<7>
14/2	10<6>		<8>
21/3	110<5>		<16>
28/4	1110<4>		<24>
35/5	11110<3>	<32>
42/6	111110<2>	<40>
49/7	1111110<1>	<48>
56/8	11111110 	<56>
?/?	11111111  <humber count>
"
	| byte {UInt8} stream {XnReadStream wimpy} mask {UInt8} count {Int32} num {Int32} |
	"count is bytes following first word or -1 if bignum meaning next byte is humber for actual count"
	stream _ self stream.
	byte _  stream getByte.
	byte <= 2r00111111 ifTrue: [^byte]. 
	byte <= 2r01111111 ifTrue: [^byte-128].
	byte <= 2r10111111
		ifTrue: [mask _ 2r00111111.  count _ 1]
		ifFalse: [byte <= 2r11011111
					ifTrue: [mask _ 2r00011111.  count _ 2]
					ifFalse: [byte <= 2r11101111
								ifTrue: [mask _ 2r00001111.  count _ 3]
								ifFalse: [byte <= 2r11110111
											ifTrue: [mask _ 2r00000111.  count _ 4]
											ifFalse: [self unimplemented]]]].
	byte _ byte bitAnd: mask.
	(byte bitAnd: ((mask bitInvert bitShiftRight: 1) bitAnd: mask)) ~= Int32Zero
		ifTrue:
			[byte _ byte bitOr: mask bitInvert.
			num _ -1]
		ifFalse: [num _ Int32Zero.
				((count > 3) and: [(byte ~= (byte bitAnd: mask))]) ifTrue: [self unimplemented]].
	num _ (num bitShift: 8) + byte.
	1 to: count do: [:i {Int32} | num _ (num bitShift: 8) + stream getByte].
	^ num!
*/
}
public void endThing() {
	if (myDepth == 0) {
		endPacket();
	}
/*
udanax-top.st:41402:Binary2Rcvr methodsFor: 'private:'!
{void} endThing
	myDepth == IntegerVar0 ifTrue: [self endPacket]!
*/
}
public void startThing() {
	if (myDepth == 0) {
		myStream.refill();
	}
/*
udanax-top.st:41405:Binary2Rcvr methodsFor: 'private:'!
{void} startThing
	myDepth == IntegerVar0 ifTrue: [myStream refill]!
*/
}
public Binary2Rcvr(TransferSpecialist specialist, XnReadStream stream) {
	super(specialist);
	myStream = stream;
	myDepth = 0;
/*
udanax-top.st:41410:Binary2Rcvr methodsFor: 'creation'!
create: specialist {TransferSpecialist} with: stream {XnReadStream}
	super create: specialist.
	myStream _ stream.
	myDepth _ IntegerVar0!
*/
}
public void destroy() {
	if ( ! (SomeRcvrs.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:41416:Binary2Rcvr methodsFor: 'creation'!
{void} destroy
	(SomeRcvrs store: self) ifFalse: [super destroy]!
*/
}
/**
 * get an identifier from the stream into a pre-allocated buffer
 */
public void getIdentifier(UInt8Array buf) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:41421:Binary2Rcvr methodsFor: 'smalltalk: deja vu'!
{void} getIdentifier: buf {UInt8Array}
	"get an identifier from the stream into a pre-allocated buffer"
	| c {char} nextPos {UInt32} limit {UInt32} |
	[limit _ buf count - 1    "For the NULL at the end."] translateOnly.
	[limit _ buf count] smalltalkOnly.
	nextPos _ UInt32Zero.
	c _ myStream getByte.
	[(Character isalnum: c) or: [c == $_]] whileTrue:
		[nextPos >= limit ifTrue: [TextyRcvr blast.IdentifierTooLong raise].
		buf at: nextPos store: c uint8.
		nextPos _ nextPos + 1.
		c _ Character char: myStream getByte].
	myStream putBack: c uint8.
	[buf at: nextPos store: Int32Zero] translateOnly.
	[buf become: (PrimSpec uInt8 arrayFromBuffer: nextPos with: buf)] smalltalkOnly.!
*/
}
public String receiveString(UInt8Array array) {
	startThing();
	StringWriter stringWriter = new StringWriter();
	PrintWriter stream = new PrintWriter(stringWriter);
	char c;
	getCharToken('"');
	while ('"' != (c = (char) myStream.getByte())) {
		if (c != '\\') {
			stream.print(c);
		}
		else {
			c = (char) myStream.getByte();
			if (c == '\'') {
				stream.print('\'');
			}
			else {
				if (c == '"') {
					stream.print('"');
				}
				else {
					if (c == 'n') {
						stream.print(AboraCharacterSupport.cr());
					}
					else {
						if (c == 't') {
							stream.print(AboraCharacterSupport.tab());
						}
						else {
							if (c == 'b') {
								stream.print(AboraCharacterSupport.backspace());
							}
							else {
								throw new AboraRuntimeException(AboraRuntimeException.INVALID_CHARACTER);
							}
						}
					}
				}
			}
		}
	}
	return stringWriter.toString();
/*
udanax-top.st:41438:Binary2Rcvr methodsFor: 'smalltalk: deja vu'!
{void} receiveString: array {UInt8Array}
	self startThing.
	[^String streamContents: [ :stream | | c {char} |
		self getCharToken: $".
		[$" ~~ (c _ myStream getByte)] whileTrue:
			[c ~~ $\ ifTrue:
				[stream nextPut: c]
			ifFalse:
				[c _ myStream getByte.
				c == $' ifTrue:
					[stream nextPut: $']
				ifFalse: [c == $" ifTrue:
					[stream nextPut: $"]
				ifFalse: [c == $n ifTrue:
					[stream nextPut: Character cr]
				ifFalse: [c == $t ifTrue:
					[stream nextPut: Character tab]
				ifFalse: [c == $b ifTrue:
					[stream nextPut: Character backspace]
				ifFalse:
					[self class blast.InvalidCharacter raise]]]]]]]]] smalltalkOnly.
	[self unimplemented] translateOnly.
	self endThing!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("( ");
	oo.print(myStream);
	oo.print(")");
/*
udanax-top.st:41464:Binary2Rcvr methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '( ' << myStream << ')'!
*/
}
public XnReadStream stream() {
	return myStream;
/*
udanax-top.st:41469:Binary2Rcvr methodsFor: 'protected: accessing'!
{XnReadStream INLINE} stream
	^ myStream!
*/
}
public static SpecialistRcvr make(TransferSpecialist specialist, XnReadStream stream) {
	Heaper result;
	result = SomeRcvrs.fetch();
	if (result == null) {
		return new Binary2Rcvr(specialist, stream);
	}
	else {
		return 
		/* TODO newBecome */
		new Binary2Rcvr(specialist, stream);
	}
/*
udanax-top.st:41481:Binary2Rcvr class methodsFor: 'creation'!
{SpecialistRcvr} make: specialist {TransferSpecialist} with: stream {XnReadStream}
	| result {Heaper} |
	result := SomeRcvrs fetch.
	result == NULL
		ifTrue: [^ self create: specialist with: stream]
		ifFalse: [^ (self new.Become: result) create: specialist with: stream]!
*/
}
public static void initTimeNonInherited() {
	SomeRcvrs = InstanceCache.make(8);
/*
udanax-top.st:41490:Binary2Rcvr class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeRcvrs := InstanceCache make: 8!
*/
}
public static void linkTimeNonInherited() {
	SomeRcvrs = null;
/*
udanax-top.st:41493:Binary2Rcvr class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeRcvrs := NULL!
*/
}
public Binary2Rcvr() {
/*

Generated during transformation
*/
}
public Binary2Rcvr(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public void getCharToken(char c) {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
}
