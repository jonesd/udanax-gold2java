/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraCharacterSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TextyXmtr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class TextyXmtr extends SpecialistXmtr {

	protected XnWriteStream myStream;
	protected int myDepth;
	protected boolean myNeedsSep;
/*
udanax-top.st:64621:
SpecialistXmtr subclass: #TextyXmtr
	instanceVariableNames: '
		myStream {XnWriteStream}
		myDepth {IntegerVar}
		myNeedsSep {BooleanVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:64628:
(TextyXmtr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:64845:
TextyXmtr class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:64848:
(TextyXmtr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TextyXmtr.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void sendBooleanVar(boolean b) {
	startThing();
	if (b) {
		myStream.putByte('1');
	}
	else {
		myStream.putByte('0');
	}
	endThing();
/*
udanax-top.st:64633:TextyXmtr methodsFor: 'sending'!
{void} sendBooleanVar: b {BooleanVar}
	self startThing.
	b ifTrue: [myStream putByte: $1 uint8] ifFalse: [myStream putByte: $0 uint8].
	self endThing!
*/
}
public void sendCategory(Category cat) {
	startThing();
	sendIdentifier(cat.name());
	/* Removed smalltalkOnly */
	endThing();
/*
udanax-top.st:64638:TextyXmtr methodsFor: 'sending'!
{void} sendCategory: cat {Category}
	self startThing. 
	[self sendIdentifier: cat name] translateOnly.
	[self sendIdentifier: (cat name copyUpTo: $.)] smalltalkOnly.
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
	startThing();
	myStream.putStr(Double.toString(x));
	/* Removed translateOnly */
	endThing();
/*
udanax-top.st:64644:TextyXmtr methodsFor: 'sending'!
{void} sendIEEEDoubleVar: x {IEEEDoubleVar} 
	"Sending the normal decimal approximation doesn't work because it introduces 
	roundoff error. What we need to do instead is send a hex encoding of the IEEE 
	double precision (64-bit) representation of the number. For clarity in the 
	textual protocol, we also include the decimal approximation in a comment."
	self startThing.
	[myStream putStr: x printString] smalltalkOnly.
	'
	char 	str[CONVERTSTRLEN];
	sprintf(str, "%g", x);
	myStream->putStr(str);' translateOnly.
	self endThing!
*/
}
public void sendInt32(int n) {
	startThing();
	myStream.putStr(Integer.toString(n));
	/* Removed translateOnly */
	endThing();
/*
udanax-top.st:64658:TextyXmtr methodsFor: 'sending'!
{void} sendInt32: n {Int32}
	self startThing.
	[myStream putStr: n printString] smalltalkOnly.
	'
	char 	str[CONVERTSTRLEN];
	sprintf(str, "%d", n);
	myStream->putStr(str);' translateOnly.
	self endThing!
*/
}
public void sendInt8(int n) {
	startThing();
	myStream.putStr(Integer.toString(n));
	/* Removed translateOnly */
	endThing();
/*
udanax-top.st:64667:TextyXmtr methodsFor: 'sending'!
{void} sendInt8: n {Int8}
	self startThing.
	[myStream putStr: n printString] smalltalkOnly.
	'
	char 	str[CONVERTSTRLEN];
	sprintf(str, "%d", n);
	myStream->putStr(str);' translateOnly.
	self endThing!
*/
}
public void sendIntegerVar(int n) {
	startThing();
	myStream.putStr(Integer.toString(n));
	/* Removed translateOnly */
	endThing();
/*
udanax-top.st:64676:TextyXmtr methodsFor: 'sending'!
{void} sendIntegerVar: n {IntegerVar}
	self startThing.
	[myStream putStr: n printString] smalltalkOnly.
	'(&n)->sendSelfTo(this);' translateOnly.
	self endThing!
*/
}
public void sendString(String s) {
	startThing();
	myStream.putByte('"');
	Someone.hack();
	/* not complete C string form */
	for (int doIndex = 0; doIndex < s.length(); doIndex ++ ) {
		char c = (char) s.charAt(doIndex);
		if (c == '\'') {
			myStream.putStr("\\'");
		}
		else {
			if (c == '"') {
				myStream.putStr("\\\"");
			}
			else {
				if (c == (AboraCharacterSupport.cr())) {
					myStream.putStr("\\n");
				}
				else {
					if (c == (AboraCharacterSupport.tab())) {
						myStream.putStr("\\t");
					}
					else {
						if (c == (AboraCharacterSupport.backspace())) {
							myStream.putStr("\\b");
						}
						else {
							if ( ! AboraCharacterSupport.isSeparator(c) || (c == ' ')) {
								myStream.putByte(c);
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
	/* Removed translateOnly */
	myStream.putByte('"');
	endThing();
/*
udanax-top.st:64682:TextyXmtr methodsFor: 'sending'!
{void} sendString: s {char star}
	self startThing.
	myStream putByte: $" uint8.
	[self hack. "not complete C string form"
	s do: [:c {char} |
		c == $' ifTrue:
			[myStream putStr: '\''']
		ifFalse: [c == $" ifTrue:
			[myStream putStr: '\"']
		ifFalse: [c == (Character cr) ifTrue:
			[myStream putStr: '\n']
		ifFalse: [c == (Character tab) ifTrue:
			[myStream putStr: '\t']
		ifFalse: [c == (Character backspace) ifTrue:
			[myStream putStr: '\b']
		ifFalse: [(c isSeparator not or: [c == $ ]) ifTrue:
			[myStream putByte: c uint8]
		ifFalse:
			[self class blast.InvalidCharacter raise]]]]]]]] smalltalkOnly.
	'for(; *s !!= ''\0''; s++) {
		switch (*s) {
		/-case ALERT_CHAR:
			myStream->putStr("\\a");
			break;-/
		case ''\n'':
			myStream->putStr("\\n\\\n");
			break;
		case ''\t'':
			myStream->putStr("\\t");
			break;
		case ''\b'':
			myStream->putStr("\\b");
			break;
		case ''\r'':
			myStream->putStr("\\r");
			break;
		case ''\f'':
			myStream->putStr("\\f");
			break;
		case ''\v'':
			myStream->putStr("\\v");
			break;
		case ''\\'':
			myStream->putStr("\\\\");
			break;
		case ''\'''':
			myStream->putStr("\\\''");
			break;
		case ''\"'':
			myStream->putStr("\\\"");
			break;
		default:
			if (isprint(*s)) {
				myStream->putByte(*s);
			} else {
				BLAST(NON_PRINTING_CHARACTER);
			}
		}
	}' translateOnly.
	myStream putByte: $" uint8.
	self endThing!
*/
}
public void sendUInt32(int n) {
	startThing();
	myStream.putStr(Integer.toString(n));
	/* Removed translateOnly */
	endThing();
/*
udanax-top.st:64745:TextyXmtr methodsFor: 'sending'!
{void} sendUInt32: n {UInt32}
	self startThing.
	[myStream putStr: n printString] smalltalkOnly.
	' 
	char 	str[CONVERTSTRLEN];
	sprintf(str, "%u", n);
	myStream->putStr(str);' translateOnly.
	self endThing!
*/
}
public void sendUInt8(int n) {
	startThing();
	myStream.putStr(Integer.toString(n));
	/* Removed translateOnly */
	endThing();
/*
udanax-top.st:64754:TextyXmtr methodsFor: 'sending'!
{void} sendUInt8: n {UInt8}
	self startThing.
	[myStream putStr: n printString] smalltalkOnly.
	'
	char 	str[CONVERTSTRLEN];
	sprintf(str, "%u", n);
	myStream->putStr(str);' translateOnly.
	self endThing!
*/
}
public void sendUInt8Data(UInt8Array array) {
	startThing();
	myStream.putByte('"');
	for (int i = 0; i < array.count(); i ++ ) {
		myStream.putByte(((int) (array.uIntAt(i))));
	}
	myStream.putByte('"');
	endThing();
/*
udanax-top.st:64763:TextyXmtr methodsFor: 'sending'!
{void} sendUInt8Data: array {UInt8Array}
	self startThing.
	myStream putByte: $" uint8.
	Int32Zero almostTo: array count do: [ :i {Int32} |
		myStream putByte: ((array uIntAt: i) basicCast: UInt8)].
	myStream putByte: $" uint8.
	self endThing!
*/
}
/**
 * end sending an instance
 */
public void endInstance() {
	myStream.putByte(')');
	myDepth = myDepth - 1;
	endThing();
/*
udanax-top.st:64773:TextyXmtr methodsFor: 'specialist sending'!
{void} endInstance
	"end sending an instance"
	myStream putByte: $) uint8.
	myDepth _ myDepth - 1.
	self endThing!
*/
}
public void decrementDepth() {
	myDepth = myDepth - 1;
/*
udanax-top.st:64781:TextyXmtr methodsFor: 'protected: sending'!
{void} decrementDepth
	myDepth _ myDepth - 1!
*/
}
public void endPacket() {
	myStream.putByte(';');
	myStream.flush();
	myNeedsSep = false;
	super.endPacket();
/*
udanax-top.st:64784:TextyXmtr methodsFor: 'protected: sending'!
{void} endPacket
	myStream putByte: $; uint8.
	myStream flush.
	myNeedsSep _ false.
	super endPacket!
*/
}
public void endThing() {
	if (myDepth == 0) {
		endPacket();
	}
	else {
		myNeedsSep = true;
	}
/*
udanax-top.st:64790:TextyXmtr methodsFor: 'protected: sending'!
{void} endThing
	myDepth == IntegerVar0
		ifTrue: [self endPacket]
		ifFalse: [myNeedsSep _ true]!
*/
}
public void incrementDepth() {
	myDepth = myDepth + 1;
/*
udanax-top.st:64795:TextyXmtr methodsFor: 'protected: sending'!
{void} incrementDepth
	myDepth _ myDepth + 1!
*/
}
public void putByte(int b) {
	myStream.putByte(b);
/*
udanax-top.st:64798:TextyXmtr methodsFor: 'protected: sending'!
{void} putByte: b {UInt8}
	myStream putByte: b!
*/
}
public void sendNULL() {
	startThing();
	sendIdentifier("NULL");
	endThing();
/*
udanax-top.st:64801:TextyXmtr methodsFor: 'protected: sending'!
{void} sendNULL
	self startThing.
	self sendIdentifier: 'NULL'.
	self endThing!
*/
}
/**
 * start sending an instance of a particular class
 */
public void startNewInstance(Category cat) {
	myDepth = myDepth + 1;
	sendCategory(cat);
	myStream.putByte('(');
	myNeedsSep = false;
/*
udanax-top.st:64807:TextyXmtr methodsFor: 'protected: sending'!
{void} startNewInstance: cat {Category} 
	"start sending an instance of a particular class"
	
	myDepth _ myDepth + 1.
	self sendCategory: cat.
	myStream putByte: $( uint8.
	myNeedsSep _ false!
*/
}
public void startThing() {
	if (myNeedsSep) {
		myStream.putByte(',');
	}
	myNeedsSep = false;
/*
udanax-top.st:64815:TextyXmtr methodsFor: 'protected: sending'!
{void} startThing
	myNeedsSep ifTrue: [myStream putByte: $, uint8].
	myNeedsSep _ false!
*/
}
/**
 * send an identifier
 */
public void sendIdentifier(String identifier) {
	myStream.putStr(identifier);
/*
udanax-top.st:64821:TextyXmtr methodsFor: 'private: sending'!
{void} sendIdentifier: identifier {char star}
	"send an identifier"
	myStream putStr: identifier!
*/
}
public TextyXmtr(TransferSpecialist specialist, XnWriteStream stream) {
	super(specialist);
	myStream = stream;
	myDepth = 0;
	myNeedsSep = false;
/*
udanax-top.st:64828:TextyXmtr methodsFor: 'creation'!
create: specialist {TransferSpecialist} with: stream {XnWriteStream}
	super create: specialist.
	myStream _ stream.
	myDepth _ IntegerVar0.
	myNeedsSep _ false!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("( ");
	oo.print(myStream);
	oo.print(")");
/*
udanax-top.st:64836:TextyXmtr methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '( ' << myStream << ')'!
*/
}
public void sendHeaper(Heaper object) {
	super.sendHeaper(object);
/*
udanax-top.st:64841:TextyXmtr methodsFor: 'overloading junk'!
{void} sendHeaper: object {Heaper}
	super sendHeaper: object!
*/
}
public static SpecialistXmtr make(TransferSpecialist specialist, XnWriteStream stream) {
	return new TextyXmtr(specialist, stream);
/*
udanax-top.st:64853:TextyXmtr class methodsFor: 'creation'!
{SpecialistXmtr} make: specialist {TransferSpecialist} with: stream {XnWriteStream}
	^self create: specialist with: stream!
*/
}
public TextyXmtr() {
/*

Generated during transformation
*/
}
public TextyXmtr(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
