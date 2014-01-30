/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.set;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.set.SHTO;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * SHTO (SpecialHashTestObject) is used for testing hash sets.  It stores an identifying
 * string, along with the hash that it is to return.  This allows a) system independent
 * testing - as the hash will be the same in all test output files, and b) provides for
 * testing complex hash value interactions with spending years looking for the right objects
 * to generate critical hash values.
 */
public class SHTO extends Heaper {

	protected int myHashValue;
	protected Sequence myStringValue;
/*
udanax-top.st:51460:
Heaper subclass: #SHTO
	instanceVariableNames: '
		myHashValue {UInt4}
		myStringValue {Sequence}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-set'!
*/
/*
udanax-top.st:51466:
SHTO comment:
'SHTO (SpecialHashTestObject) is used for testing hash sets.  It stores an identifying string, along with the hash that it is to return.  This allows a) system independent testing - as the hash will be the same in all test output files, and b) provides for testing complex hash value interactions with spending years looking for the right objects to generate critical hash values.'!
*/
/*
udanax-top.st:51468:
(SHTO getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:51512:
SHTO class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:51515:
(SHTO getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SHTO.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return myHashValue;
/*
udanax-top.st:51473:SHTO methodsFor: 'tests'!
{UInt32} actualHashForEqual
	^ myHashValue!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof SHTO) {
		SHTO foo = (SHTO) other;
		return myHashValue == foo.hashForEqual() && (myStringValue.isEqual(foo.stringValue()));
	}
	else {
		return false;
	}
/*
udanax-top.st:51477:SHTO methodsFor: 'tests'!
{BooleanVar} isEqual: other {Heaper} 
	other cast: SHTO
		into: [:foo {SHTO} |
			^myHashValue = foo hashForEqual and: [myStringValue isEqual: foo stringValue]]
		others: [
			^false].
	^false "fodder"!
*/
}
public SHTO(Sequence onString, int onHash) {
	super();
	myStringValue = onString;
	myHashValue = onHash;
/*
udanax-top.st:51487:SHTO methodsFor: 'creation'!
create: onString {Sequence} with: onHash {UInt32}
	super create.
	myStringValue _ onString.
	myHashValue _ onHash!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(AboraSupport.toBaseString(myHashValue, 16));
	/* Removed translateOnly */
	oo.print(", ");
	oo.print(myStringValue);
	oo.print(")");
/*
udanax-top.st:51495:SHTO methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '('.
	[myHashValue printOn: oo base: 16] smalltalkOnly.
	'{
		char	buffer[9];
		sprintf(buffer, "%X", myHashValue);
		oo << buffer;
	}' translateOnly.
	oo << ', ' << myStringValue << ')'!
*/
}
public Sequence stringValue() {
	return myStringValue;
/*
udanax-top.st:51508:SHTO methodsFor: 'private: accessing'!
{Sequence} stringValue
	^ myStringValue!
*/
}
public static SHTO make(String aString, int aHashVal) {
	Sequence pack;
	pack = Sequence.string(aString);
	return new SHTO(pack, aHashVal);
/*
udanax-top.st:51520:SHTO class methodsFor: 'make'!
make: aString {char vector} with: aHashVal {UInt32}
	| pack {Sequence} |
	pack _ Sequence string: aString.
	^ self create: pack with: aHashVal!
*/
}
public SHTO() {
/*

Generated during transformation
*/
}
public SHTO(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public static SHTO make(String aString) {
	return make(aString, 0);
/*

Generated during transformation: AddDefaultParameter
*/
}
}
