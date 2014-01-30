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
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Binary2Xmtr;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class Binary2Xmtr extends SpecialistXmtr {

	protected XnWriteStream myStream;
	protected int myDepth;
	protected static int MaxNumberLength;
	protected static UInt8Array NumberBuffer;
	protected static InstanceCache SomeXmtrs;
/*
udanax-top.st:64435:
SpecialistXmtr subclass: #Binary2Xmtr
	instanceVariableNames: '
		myStream {XnWriteStream}
		myDepth {IntegerVar}'
	classVariableNames: '
		MaxNumberLength {Int32} 
		NumberBuffer {UInt8 star} 
		SomeXmtrs {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:64444:
(Binary2Xmtr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:64595:
Binary2Xmtr class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:64598:
(Binary2Xmtr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Binary2Xmtr.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void sendBooleanVar(boolean b) {
	if (b) {
		myStream.putByte(1);
	}
	else {
		myStream.putByte(0);
	}
	endThing();
/*
udanax-top.st:64449:Binary2Xmtr methodsFor: 'sending'!
{void} sendBooleanVar: b {BooleanVar}
	b ifTrue: [myStream putByte: 1] ifFalse: [myStream putByte: Int0].
	self endThing!
*/
}
public void sendCategory(Category cat) {
	putIntegerVar((specialist().numberOfCategory(cat)) + 1);
	endThing();
/*
udanax-top.st:64453:Binary2Xmtr methodsFor: 'sending'!
{void} sendCategory: cat {Category} 
	self putIntegerVar: (self specialist numberOfCategory: cat) + 1.
	self endThing!
*/
}
/**
 * Sending the normal decimal approximation doesn't work because it introduces
 * roundoff error. What we need to do instead is send a hex encoding of the IEEE
 * double precision (64-bit) representation of the number. For clarity in the
 * textual protocol, we also include the decimal approximation in a comment.
 */
public void sendIEEEDoubleVar(double x) {
	throw new UnimplementedException();
/*
udanax-top.st:64457:Binary2Xmtr methodsFor: 'sending'!
{void} sendIEEEDoubleVar: x {IEEEDoubleVar} 
	"Sending the normal decimal approximation doesn't work because it introduces 
	roundoff error. What we need to do instead is send a hex encoding of the IEEE 
	double precision (64-bit) representation of the number. For clarity in the 
	textual protocol, we also include the decimal approximation in a comment."
	self unimplemented.
	"self putIntegerVar: x DOTmantissa.
	self putIntegerVar: x DOTexponent."
	self endThing!
*/
}
public void sendInt32(int n) {
	putIntegerVar(n);
	endThing();
/*
udanax-top.st:64468:Binary2Xmtr methodsFor: 'sending'!
{void} sendInt32: n {Int32}
	self putIntegerVar: n.
	self endThing!
*/
}
public void sendInt8(int n) {
	myStream.putByte((AboraSupport.modulo(n, 256)));
	endThing();
/*
udanax-top.st:64472:Binary2Xmtr methodsFor: 'sending'!
{void} sendInt8: n {Int8}
	myStream putByte: (n \\ 256).
	self endThing!
*/
}
public void sendIntegerVar(int n) {
	putIntegerVar(n);
	endThing();
/*
udanax-top.st:64476:Binary2Xmtr methodsFor: 'sending'!
{void} sendIntegerVar: n {IntegerVar}
	self putIntegerVar: n.
	self endThing!
*/
}
public void sendString(String s) {
	putIntegerVar(((s.length())));
	myStream.putStr(s);
	endThing();
/*
udanax-top.st:64480:Binary2Xmtr methodsFor: 'sending'!
{void} sendString: s {char star}
	self putIntegerVar: (Integer IntegerVar: (String strlen: s)).
	myStream putStr: s.
	self endThing!
*/
}
public void sendUInt32(int n) {
	putIntegerVar(n);
	endThing();
/*
udanax-top.st:64486:Binary2Xmtr methodsFor: 'sending'!
{void} sendUInt32: n {UInt32}
	self putIntegerVar: n.
	self endThing!
*/
}
public void sendUInt4(int n) {
	putIntegerVar(n);
	endThing();
/*
udanax-top.st:64490:Binary2Xmtr methodsFor: 'sending'!
{void} sendUInt4: n {UInt4}
	self putIntegerVar: n.
	self endThing!
*/
}
public void sendUInt8(int n) {
	myStream.putByte(n);
	endThing();
/*
udanax-top.st:64494:Binary2Xmtr methodsFor: 'sending'!
{void} sendUInt8: n {UInt8}
	myStream putByte: n.
	self endThing!
*/
}
public void sendUInt8Data(UInt8Array array) {
	myStream.putData(array);
	endThing();
/*
udanax-top.st:64498:Binary2Xmtr methodsFor: 'sending'!
{void} sendUInt8Data: array {UInt8Array}
	myStream putData: array.
	self endThing!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("( ");
	oo.print(myStream);
	oo.print(")");
/*
udanax-top.st:64505:Binary2Xmtr methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '( ' << myStream << ')'!
*/
}
/**
 * Put in a separator pattern so we can detect the packets visually.
 */
public void endPacket() {
	myStream.putByte('!');
/*
udanax-top.st:64510:Binary2Xmtr methodsFor: 'protected: sending'!
{void} endPacket
	"Put in a separator pattern so we can detect the packets visually."
	myStream putByte: $!!
*/
}
/*
udanax-top.st:64514:Binary2Xmtr methodsFor: 'protected: sending'!
uint8.
	myStream putByte: $!!
*/
/*
udanax-top.st:64515:Binary2Xmtr methodsFor: 'protected: sending'!
uint8.
	myStream flush.
	super endPacket!
*/
public void endThing() {
	if (myDepth <= 0) {
		endPacket();
	}
/*
udanax-top.st:64517:Binary2Xmtr methodsFor: 'protected: sending'!
{void} endThing
	myDepth <= IntegerVar0 ifTrue: [self endPacket]!
*/
}
/**
 * Send a Dean style humber.  Like Drexler style, except all the tag bits go into the first
 * byte.
 */
public void putIntegerVar(int num) {
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
	int abs;
	XnWriteStream stream;
	int val;
	stream = stream();
	val = num;
	if (val < 0) {
		abs = - val;
	}
	else {
		abs = val;
	}
	if (abs < 
	/* 2**6 */
	64) {
		stream.putByte((val & 127));
		return ;
	}
	if (abs < 
	/* 2**13 */
	8192) {
		stream.putByte((((val >> 8) & 63) | 128));
		stream.putByte((val & 255));
		return ;
	}
	if (abs < 
	/* 2**20 */
	1048576) {
		stream.putByte((((val >> 16) & 31) | 192));
		stream.putByte(((val >> 8) & 255));
		stream.putByte((val & 255));
		return ;
	}
	if (abs < 
	/* 2**27 */
	134217728) {
		stream.putByte((((val >> 24) & 15) | 224));
		stream.putByte(((val >> 16) & 255));
		stream.putByte(((val >> 8) & 255));
		stream.putByte((val & 255));
		return ;
	}
	/* abs < (2**34) */
	if (true) {
		stream.putByte((((val >> 32) & 7) | 240));
		stream.putByte(((val >> 24) & 255));
		stream.putByte(((val >> 16) & 255));
		stream.putByte(((val >> 8) & 255));
		stream.putByte((val & 255));
		return ;
	}
	Eric.thingToDo();
	/* humber case */
/*
udanax-top.st:64520:Binary2Xmtr methodsFor: 'protected: sending'!
{void} putIntegerVar: num {IntegerVar}
	"Send a Dean style humber.  Like Drexler style, except all the tag bits go into the first byte."
	 
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
	| abs {IntegerVar} stream {XnWriteStream wimpy} val {Int32} |
	stream _ self stream.
	val _ num DOTasLong.
	val < Int32Zero ifTrue: [abs _ val negated] ifFalse: [abs _ val].
	abs < "2**6" 64 ifTrue: [stream putByte: (val bitAnd: 127).  ^VOID].
	abs < "2**13"  8192 ifTrue: 
		[stream putByte: (((val bitShiftRight: 8) bitAnd: 2r0111111) bitOr: 2r10000000).
		stream putByte: (val  bitAnd: 255).
		^VOID].
	abs < "2**20"  1048576 ifTrue: 
		[stream putByte: (((val  bitShiftRight: 16) bitAnd: 2r011111) bitOr: 2r11000000).
		stream putByte: ((val  bitShiftRight: 8) bitAnd: 255).
		stream putByte: (val  bitAnd: 255).
		^VOID].
	abs < "2**27"  134217728 ifTrue: 
		[stream putByte: (((val  bitShiftRight: 24) bitAnd: 2r00001111) bitOr: 2r11100000).
		stream putByte: ((val  bitShiftRight: 16) bitAnd: 255).
		stream putByte: ((val  bitShiftRight: 8) bitAnd: 255).
		stream putByte: (val  bitAnd: 255).
		^VOID].
	"abs < (2**34)" true ifTrue: 
		[stream putByte: (((val bitShiftRight: 32) bitAnd: 2r0111) bitOr: 2r11110000).
		stream putByte: ((val bitShiftRight: 24) bitAnd: 255).
		stream putByte: ((val bitShiftRight: 16) bitAnd: 255).
		stream putByte: ((val bitShiftRight: 8) bitAnd: 255).
		stream putByte: (val bitAnd: 255).
		^VOID].
	Eric thingToDo. "humber case"!
*/
}
public void sendNULL() {
	myStream.putByte(0);
	endThing();
/*
udanax-top.st:64563:Binary2Xmtr methodsFor: 'protected: sending'!
{void} sendNULL
	myStream putByte: Int0.
	self endThing!
*/
}
/**
 * start sending an instance of a particular class. Add one because 0 means NULL
 */
public void startNewInstance(Category cat) {
	myDepth = myDepth + 1;
	sendCategory(cat);
/*
udanax-top.st:64568:Binary2Xmtr methodsFor: 'protected: sending'!
{void} startNewInstance: cat {Category} 
	"start sending an instance of a particular class. Add one because 0 means NULL"
	
	myDepth _ myDepth + 1.
	self sendCategory: cat!
*/
}
public XnWriteStream stream() {
	return myStream;
/*
udanax-top.st:64574:Binary2Xmtr methodsFor: 'protected: sending'!
{XnWriteStream INLINE} stream
	^myStream!
*/
}
public Binary2Xmtr(TransferSpecialist specialist, XnWriteStream stream) {
	super(specialist);
	myStream = stream;
	myDepth = 0;
/*
udanax-top.st:64579:Binary2Xmtr methodsFor: 'creation'!
create: specialist {TransferSpecialist} with: stream {XnWriteStream}
	super create: specialist.
	myStream _ stream.
	myDepth _ IntegerVar0!
*/
}
public void destroy() {
	if ( ! (SomeXmtrs.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:64584:Binary2Xmtr methodsFor: 'creation'!
{void} destroy
	(SomeXmtrs store: self) ifFalse: [super destroy]!
*/
}
/**
 * end sending an instance
 */
public void endInstance() {
	myDepth = myDepth - 1;
	endThing();
/*
udanax-top.st:64589:Binary2Xmtr methodsFor: 'specialist sending'!
{void} endInstance
	"end sending an instance"
	myDepth _ myDepth - 1.
	self endThing!
*/
}
public static SpecialistXmtr make(TransferSpecialist specialist, XnWriteStream stream) {
	Heaper result;
	result = SomeXmtrs.fetch();
	if (result == null) {
		return new Binary2Xmtr(specialist, stream);
	}
	else {
		return 
		/* TODO newBecome */
		new Binary2Xmtr(specialist, stream);
	}
/*
udanax-top.st:64603:Binary2Xmtr class methodsFor: 'creation'!
{SpecialistXmtr} make: specialist {TransferSpecialist} with: stream {XnWriteStream}
	| result {Heaper} |
	result := SomeXmtrs fetch.
	result == NULL
		ifTrue: [^ self create: specialist with: stream]
		ifFalse: [^ (self new.Become: result) create: specialist with: stream]!
*/
}
public static void initTimeNonInherited() {
	SomeXmtrs = InstanceCache.make(8);
/*
udanax-top.st:64612:Binary2Xmtr class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeXmtrs := InstanceCache make: 8!
*/
}
public static void linkTimeNonInherited() {
	MaxNumberLength = 400;
	NumberBuffer = new UInt8Array(MaxNumberLength);
	/* Removed smalltalkOnly */
	SomeXmtrs = null;
/*
udanax-top.st:64615:Binary2Xmtr class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	MaxNumberLength _ 400.
	NumberBuffer _ UInt8 vector create: MaxNumberLength.
	[NumberBuffer _ UInt8Array make: MaxNumberLength] smalltalkOnly.
	SomeXmtrs := NULL!
*/
}
public Binary2Xmtr() {
/*

Generated during transformation
*/
}
public Binary2Xmtr(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
