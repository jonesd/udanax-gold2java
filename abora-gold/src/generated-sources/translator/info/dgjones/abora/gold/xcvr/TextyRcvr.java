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
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.TextyRcvr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xpp.basic.Category;
import java.io.PrintWriter;
import java.io.StringWriter;

public class TextyRcvr extends SpecialistRcvr {

	protected XnReadStream myStream;
	protected int myDepth;
	protected static UInt8Array ReceiveStringBuffer;
	protected static int ReceiveStringBufferSize;
/*
udanax-top.st:41496:
SpecialistRcvr subclass: #TextyRcvr
	instanceVariableNames: '
		myStream {XnReadStream}
		myDepth {IntegerVar}'
	classVariableNames: '
		ReceiveStringBuffer {char vector} 
		ReceiveStringBufferSize {Int4 const} '
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:41504:
(TextyRcvr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:41800:
TextyRcvr class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:41803:
(TextyRcvr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TextyRcvr.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean receiveBooleanVar() {
	char result;
	startThing();
	result = skipWhiteSpace();
	endThing();
	if (result == '1') {
		return true;
	}
	else {
		if (result == '0') {
			return false;
		}
		else {
			throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
		}
	}
/*
udanax-top.st:41509:TextyRcvr methodsFor: 'receiving'!
{BooleanVar} receiveBooleanVar
	| result {BooleanVar} |
	self startThing.
	result _ self skipWhiteSpace.
	self endThing.
	result == $1 ifTrue: [^true] ifFalse: [result == $0 ifTrue: [^false] ifFalse: [Heaper BLAST: #InvalidRequest]].
	^false "fodder"!
*/
}
public Category receiveCategory() {
	getIdentifier(ReceiveStringBuffer, ReceiveStringBufferSize-1);
	if ((ReceiveStringBuffer.compareTo("NULL")) == 0) {
		endThing();
		return null;
	}
	/* Removed smalltalkOnly */
	return Category.find(ReceiveStringBuffer);
/*
udanax-top.st:41517:TextyRcvr methodsFor: 'receiving'!
{Category} receiveCategory
	self getIdentifier: ReceiveStringBuffer with: ReceiveStringBufferSize-1.
	(String strcmp: ReceiveStringBuffer with: 'NULL') == UInt32Zero
		ifTrue: [self endThing.
				^NULL].
	[^Category find: (ReceiveStringBuffer copyUpTo: Character null)] smalltalkOnly.
	^Category find: ReceiveStringBuffer.!
*/
}
/**
 * Fill the array with data from the stream.
 */
public void receiveData(UInt8Array array) {
	startThing();
	getCharToken('"');
	for (int i = 0; i < array.count(); i ++ ) {
		array.storeUInt(i, myStream.getByte());
	}
	getCharToken('"');
	endThing();
/*
udanax-top.st:41525:TextyRcvr methodsFor: 'receiving'!
{void} receiveData: array {UInt8Array}
	"Fill the array with data from the stream."
	
	self startThing.
	self getCharToken: $".
	Int32Zero almostTo: array count do: [ :i {Int32} | 
		array at: i storeUInt: myStream getByte].
	self getCharToken: $".
	self endThing!
*/
}
public double receiveIEEEDoubleVar() {
	startThing();
	throw new UnimplementedException();
/*
udanax-top.st:41535:TextyRcvr methodsFor: 'receiving'!
{IEEEDoubleVar} receiveIEEEDoubleVar
	self startThing.
	self unimplemented.
	self endThing.
	^0.0!
*/
}
public int receiveInt32() {
	int result;
	startThing();
	result = receiveNumber();
	endThing();
	return result;
/*
udanax-top.st:41541:TextyRcvr methodsFor: 'receiving'!
{Int32} receiveInt32 
	| result {Int32} |
	self startThing.
	result _ self receiveNumber DOTasLong.
	self endThing.
	^result!
*/
}
public int receiveInt8() {
	int result;
	startThing();
	result = receiveNumber();
	endThing();
	return result;
/*
udanax-top.st:41548:TextyRcvr methodsFor: 'receiving'!
{Int8} receiveInt8
	| result {Int8} | 
	self startThing.
	result _ self receiveNumber DOTasInt.
	self endThing.
	^result!
*/
}
public int receiveIntegerVar() {
	int result;
	startThing();
	result = receiveNumber();
	endThing();
	return result;
/*
udanax-top.st:41555:TextyRcvr methodsFor: 'receiving'!
{IntegerVar} receiveIntegerVar
	| result {IntegerVar} |
	self startThing. 
	result _ self receiveNumber.
	self endThing.
	^result
	
	"[^IntegerVar localIntVar: self with: #tcsj] translateOnly."!
*/
}
public String receiveString() {
	String result;
	startThing();
	StringWriter stringWriter = new StringWriter();
	PrintWriter stream = new PrintWriter(stringWriter);
	char c;
	getCharToken('"');
	while ('"' != (c = (char) (myStream.getByte()))) {
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
	result = stringWriter.toString();
	/* Removed translateOnly */
	endThing();
	return result;
/*
udanax-top.st:41564:TextyRcvr methodsFor: 'receiving'!
{char star} receiveString
	| result {char star} |
	self startThing.
	[result _ String streamContents: [ :stream | | c {char} |
		self getCharToken: $".
		[$" ~~ (c _ Character char: myStream getByte)] whileTrue:
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
					[Heaper BLAST: #InvalidCharacter]]]]]]]]] smalltalkOnly.
	'char *buf = TextyRcvr::ReceiveStringBuffer;
	UInt32 max = TextyRcvr::ReceiveStringBufferSize;
	this->getCharToken(''"'');
	for (char c = myStream->getByte();
	  c !!= ''\"'';
	  c = myStream->getByte()) {
		if (max <= 1) {
			BLAST(STRING_TOO_LONG);
		}
		max -= 1;
		if (c !!= ''\\'') {
			*buf++ = c;
		} else {
			c = myStream->getByte();
			switch (c) {
			case ''a'':
				*buf++ = ALERT_CHAR;
				break;
			case ''?'':
/- ANSI permits, but does not require, ''?'' to be escaped.  Therefore, even -/
/-  though we do not send it escaped, for consistency with the standard, we -/
/-  can receive it either way.  -/
				*buf++ = ''?'';
				break;
			case ''\n'':
				max += 1;
				break;
			case ''n'':
				*buf++ = ''\n'';
				break;
			case ''t'':
				*buf++ = ''\t'';
				break;
			case ''b'':
				*buf++ = ''\b'';
				break;
			case ''r'':
				*buf++ = ''\r'';
				break;
			case ''f'':
				*buf++ = ''\f'';
				break;
			case ''v'':
				*buf++ = ''\v'';
				break;
			case ''\\'':
				*buf++ = ''\\'';
				break;
			case ''\'''':
				*buf++ = ''\'''';
				break;
			case ''\"'':
				*buf++ = ''\"'';
				break;
			default:
				BLAST(UNRECOGNIZED_ESCAPE);
			}
		}
	}
	*buf++ = ''\0'';
	Int32 size = buf - ReceiveStringBuffer;
  	result = strcpy(new char[size],ReceiveStringBuffer);' translateOnly.
	self endThing.
	^result!
*/
}
public int receiveUInt32() {
	int result;
	startThing();
	result = receiveNumber();
	endThing();
	return result;
/*
udanax-top.st:41651:TextyRcvr methodsFor: 'receiving'!
{UInt32} receiveUInt32
	| result {UInt32} | 
	self startThing.
	result _ self receiveNumber DOTasLong.
	self endThing.
	^result!
*/
}
public int receiveUInt8() {
	int result;
	startThing();
	result = receiveNumber();
	endThing();
	return result;
/*
udanax-top.st:41658:TextyRcvr methodsFor: 'receiving'!
{UInt8} receiveUInt8
	| result {UInt8} | 
	self startThing.
	result _ self receiveNumber DOTasInt.
	self endThing.
	^result!
*/
}
public void decrementDepth() {
	myDepth = myDepth - 1;
/*
udanax-top.st:41667:TextyRcvr methodsFor: 'protected: lexer'!
{void} decrementDepth
	myDepth _ myDepth - 1!
*/
}
public void endOfInstance() {
	getCharToken(')');
	myDepth = myDepth - 1;
	endThing();
/*
udanax-top.st:41670:TextyRcvr methodsFor: 'protected: lexer'!
{void} endOfInstance
	self getCharToken: $).
	myDepth _ myDepth - 1.
	self endThing!
*/
}
public void endPacket() {
	getCharToken(';');
	super.endPacket();
/*
udanax-top.st:41675:TextyRcvr methodsFor: 'protected: lexer'!
{void} endPacket
	self getCharToken: $;.
	super endPacket!
*/
}
public Category fetchStartOfInstance() {
	Category cat;
	startThing();
	cat = receiveCategory();
	if (cat == null) {
		return null;
	}
	myDepth = myDepth + 1;
	getCharToken('(');
	return cat;
/*
udanax-top.st:41679:TextyRcvr methodsFor: 'protected: lexer'!
{Category} fetchStartOfInstance
	| cat {Category} |
	self startThing.
	cat _ self receiveCategory.
	cat == NULL ifTrue: [^NULL].
	myDepth _ myDepth + 1.
	self getCharToken: $(.
	^cat!
*/
}
public int getByte() {
	return myStream.getByte();
/*
udanax-top.st:41689:TextyRcvr methodsFor: 'protected: lexer'!
{UInt32} getByte
	^myStream getByte!
*/
}
/**
 * match a character from the input stream
 */
public void getCharToken(char referent) {
	char c;
	c = skipWhiteSpace();
	if (c != referent) {
		throw new AboraRuntimeException(AboraRuntimeException.WRONG_CHARACTER);
	}
/*
udanax-top.st:41692:TextyRcvr methodsFor: 'protected: lexer'!
{void} getCharToken: referent {Character}
	"match a character from the input stream"
	
	| c {char} |
	c _ self skipWhiteSpace.
	c ~~ referent ifTrue:
		[Heaper BLAST: #WrongCharacter]!
*/
}
/**
 * get an identifier from the stream into a pre-allocated buffer
 */
public void getIdentifier(UInt8Array buf, int limit) {
	char c;
	int nextPos;
	nextPos = 0;
	c = skipWhiteSpace();
	while ((Character.isLetterOrDigit(c)) || (c == '_')) {
		if (nextPos >= limit) {
			throw new AboraRuntimeException(AboraRuntimeException.IDENTIFIER_TOO_LONG);
		}
		/* Removed smalltalkOnly */
		buf.put(nextPos, c);
		nextPos = nextPos + 1;
		c = (char) (myStream.getByte());
	}
	myStream.putBack(c);
	/* Removed smalltalkOnly */
	buf.put(nextPos, ((char) (0)));
/*
udanax-top.st:41700:TextyRcvr methodsFor: 'protected: lexer'!
{void} getIdentifier: buf {char star} with: limit {Int32}
	"get an identifier from the stream into a pre-allocated buffer"
	| c {char} nextPos {Int32} |
	nextPos _ Int32Zero.
	c _ self skipWhiteSpace.
	[(Character isalnum: c) or: [c == $_]] whileTrue:
		[nextPos >= limit ifTrue: [TextyRcvr BLAST: #IdentifierTooLong].
		[buf at: nextPos+1 put: c] smalltalkOnly.
		[buf at: nextPos put: c] translateOnly.
		nextPos _ nextPos + 1.
		c _ Character char: myStream getByte].
	myStream putBack: c uint8.
	[buf at: nextPos+1 put: (Character char: UInt32Zero)] smalltalkOnly.
	[buf at: nextPos put: (Character char: UInt32Zero)] translateOnly!
*/
}
public void incrementDepth() {
	myDepth = myDepth + 1;
/*
udanax-top.st:41716:TextyRcvr methodsFor: 'protected: lexer'!
{void} incrementDepth
	myDepth _ myDepth + 1!
*/
}
/**
 * return the first character following white space
 */
public char skipWhiteSpace() {
	char c;
	c = (char) (myStream.getByte());
	while ((Character.isWhitespace(c)) || (c == ',')) {
		c = (char) (myStream.getByte());
	}
	return c;
/*
udanax-top.st:41719:TextyRcvr methodsFor: 'protected: lexer'!
{char} skipWhiteSpace
	"return the first character following white space"
	| c {char} |
	c _ Character char: myStream getByte.
	[(Character isspace: c) or: [c == $,]] whileTrue: [c _ Character char: myStream getByte].
	^c!
*/
}
/**
 * Receive an arbitrary number.  Convert to the lesser types by range checking and casting.
 */
public int receiveNumber() {
	int value;
	boolean neg;
	int c;
	c = skipWhiteSpace();
	neg = c == '-';
	if (neg) {
		c = (char) (myStream.getByte());
	}
	value = 0;
	while (Character.isDigit((char) c)) {
		int digit;
		digit = c - '0';
		value = value * 10 + digit;
		c = (char) (myStream.getByte());
	}
	myStream.putBack(c);
	if (neg) {
		return - value;
	}
	return value;
/*
udanax-top.st:41728:TextyRcvr methodsFor: 'private: receiving'!
{IntegerVar} receiveNumber
	"Receive an arbitrary number.  Convert to the lesser types by range checking and casting."
 
	| value {IntegerVar} neg {BooleanVar} c {UInt8} |
	c _ self skipWhiteSpace.
	neg _ c == $-.
	neg ifTrue: [c _ Character char: myStream getByte].
	value _ IntegerVar0.
	[Character isdigit: c] whileTrue: 
		[| digit {UInt8} |
		digit _ c uint8 - $0 uint8.
		value _ value * 10 + digit.
		c _ Character char: myStream getByte].
	myStream putBack: c uint8.
	neg ifTrue: [^value negated].
	^value!
*/
}
public TextyRcvr(TransferSpecialist specialist, XnReadStream stream) {
	super(specialist);
	myStream = stream;
	myDepth = 0;
/*
udanax-top.st:41747:TextyRcvr methodsFor: 'creation'!
create: specialist {TransferSpecialist} with: stream {XnReadStream}
	super create: specialist.
	myStream _ stream.
	myDepth _ IntegerVar0!
*/
}
public String receiveEverything() {
	StringWriter stringWriter = new StringWriter();
	PrintWriter stream = new PrintWriter(stringWriter);
	while ( ! (myStream.end())) {
		stream.print(((char) (myStream.getByte())));
	}
	return stringWriter.toString();
/*
udanax-top.st:41755:TextyRcvr methodsFor: 'smalltalk: receiving'!
{String} receiveEverything
	^String streamContents: [ :stream |
		[myStream atEnd] whileFalse:
			[stream nextPut: (Character char: myStream getByte)]]!
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
udanax-top.st:41762:TextyRcvr methodsFor: 'smalltalk: deja vu'!
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
					[Heaper BLAST: #InvalidCharacter]]]]]]]]] smalltalkOnly.
	[self unimplemented] translateOnly.
	self endThing!
*/
}
public void endThing() {
	if (myDepth == 0) {
		endPacket();
	}
/*
udanax-top.st:41788:TextyRcvr methodsFor: 'protected: receiving'!
{void} endThing
	myDepth == IntegerVar0 ifTrue: [self endPacket]!
*/
}
public void startThing() {
	if (myDepth == 0) {
		myStream.refill();
	}
/*
udanax-top.st:41791:TextyRcvr methodsFor: 'protected: receiving'!
{void} startThing
	myDepth == IntegerVar0 ifTrue: [myStream refill]!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("( ");
	oo.print(myStream);
	oo.print(")");
/*
udanax-top.st:41796:TextyRcvr methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '( ' << myStream << ')'!
*/
}
public static SpecialistRcvr make(TransferSpecialist specialist, XnReadStream stream) {
	return new TextyRcvr(specialist, stream);
/*
udanax-top.st:41808:TextyRcvr class methodsFor: 'creation'!
{SpecialistRcvr} make: specialist {TransferSpecialist} with: stream {XnReadStream}
	^self create: specialist with: stream!
*/
}
/**
 * !!!!!!!! This constant size buffer is a bad idea.  It's going to get us in trouble.
 */
public static void linkTimeNonInherited() {
	ReceiveStringBufferSize = 4096;
	/* 4088 is longest allowable C++ class name. */
	/* Removed translateOnly */
	ReceiveStringBuffer = new UInt8Array(ReceiveStringBufferSize);
/*
udanax-top.st:41813:TextyRcvr class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	"!!!!!!!! This constant size buffer is a bad idea.  It's going to get us in trouble."
	
	ReceiveStringBufferSize _ 4096. "4088 is longest allowable C++ class name."
	'static char permReceiveStringBuffer[4096];
char * TextyRcvr::ReceiveStringBuffer = permReceiveStringBuffer;' translateOnly.
	[ReceiveStringBuffer := String new: ReceiveStringBufferSize] smalltalkOnly.!
*/
}
public TextyRcvr() {
/*

Generated during transformation
*/
}
public TextyRcvr(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
