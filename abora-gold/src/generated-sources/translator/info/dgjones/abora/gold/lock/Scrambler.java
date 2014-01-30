/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.lock;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.lock.Scrambler;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A Scrambler implements a one-way hash function. It should be one-way, in that it should be
 * difficult to unscramble, and it should be a hash, in that two similar inputs should
 * produce very different outputs. It is furthermore desirable but not essential that the
 * algorithm be cryptographically secure (the only way to unscramble an output is by
 * scrambling all possible inputs and comparing), and one-to-one (two different inputs never
 * produce the same output). Each subclass implements some particular algorithm such as
 * Snefru, in response to the scrambling protocol.
 * The system maintains a table of all of the known Scramblers, indexed by name (a
 * PackOBits). At initialization time, each concrete subclass should use the
 * DEFINE_SCRAMBLER("identifier",(scramblerExpression)) macro to place an instance in the
 * table at some appropriate identifier. DEFINE_SCRAMBLER must be invoked inside an
 * Initializer (e.g. in an initTimeNonInherited method).
 * MatchLockSmiths store passwords in scrambled form, so that being able to read the
 * LockSmith is not enough to find out the password. They also store the name of the
 * Scrambler used to scramble it, so that trial passwords can be scrambled and compared.
 */
public class Scrambler extends Heaper {

	protected static MuTable AllScramblers;
/*
udanax-top.st:45035:
Heaper subclass: #Scrambler
	instanceVariableNames: ''
	classVariableNames: 'AllScramblers {MuTable of: Sequence and: Scrambler} '
	poolDictionaries: ''
	category: 'Xanadu-lock'!
*/
/*
udanax-top.st:45039:
Scrambler comment:
'A Scrambler implements a one-way hash function. It should be one-way, in that it should be difficult to unscramble, and it should be a hash, in that two similar inputs should produce very different outputs. It is furthermore desirable but not essential that the algorithm be cryptographically secure (the only way to unscramble an output is by scrambling all possible inputs and comparing), and one-to-one (two different inputs never produce the same output). Each subclass implements some particular algorithm such as Snefru, in response to the scrambling protocol. 
 
The system maintains a table of all of the known Scramblers, indexed by name (a PackOBits). At initialization time, each concrete subclass should use the DEFINE_SCRAMBLER("identifier",(scramblerExpression)) macro to place an instance in the table at some appropriate identifier. DEFINE_SCRAMBLER must be invoked inside an Initializer (e.g. in an initTimeNonInherited method).
MatchLockSmiths store passwords in scrambled form, so that being able to read the LockSmith is not enough to find out the password. They also store the name of the Scrambler used to scramble it, so that trial passwords can be scrambled and compared.'!
*/
/*
udanax-top.st:45045:
(Scrambler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:45061:
Scrambler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:45064:
(Scrambler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Scrambler.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Carry out a one-way hash function on the given clear text.
 */
public UInt8Array scramble(UInt8Array clear) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:45050:Scrambler methodsFor: 'scrambling'!
{UInt8Array} scramble: clear {UInt8Array}
	"Carry out a one-way hash function on the given clear text."
	
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:45057:Scrambler methodsFor: 'tesing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/**
 * Register the existence of a particular kind of scrambler. The identifier must be unique.
 */
public static void remember(Sequence identifier, Scrambler scrambler) {
	AllScramblers.introduce(identifier, scrambler);
/*
udanax-top.st:45069:Scrambler class methodsFor: 'was protected'!
{void} remember: identifier {Sequence} with: scrambler {Scrambler}
	"Register the existence of a particular kind of scrambler. The identifier must be unique."
	
	AllScramblers at: identifier introduce: scrambler!
*/
}
/**
 * Return a scrambler with the given name. Fail with
 * BLAST(NoSuchScrambler) if there is none.
 */
public static Scrambler make(UInt8Array identifier) {
	try {
		return (Scrambler) (AllScramblers.get((Sequence.numbers(identifier))));
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.NOT_IN_TABLE.equals(ex.getMessage())) {
			throw new AboraRuntimeException(AboraRuntimeException.NO_SUCH_SCRAMBLER);
		}
		else {
			throw ex;
		}
	}
/*
udanax-top.st:45076:Scrambler class methodsFor: 'accessing'!
{Scrambler} make: identifier {UInt8Array}
	"Return a scrambler with the given name. Fail with
		BLAST(NoSuchScrambler) if there is none."
	
	ScruTable problems.NotInTable
		handle: [ :boom | Heaper BLAST: #NoSuchScrambler]
		do: [^(AllScramblers get: (Sequence numbers: identifier)) cast: Scrambler]!
*/
}
public static void initTimeNonInherited() {
	AllScramblers = MuTable.make(SequenceSpace.make());
/*
udanax-top.st:45086:Scrambler class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	self REQUIRES: MuTable.
	self REQUIRES: SequenceSpace.
	AllScramblers := MuTable make: SequenceSpace make.!
*/
}
public static void linkTimeNonInherited() {
	AllScramblers = null;
/*
udanax-top.st:45092:Scrambler class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	AllScramblers := NULL.!
*/
}
public static void DEFINEUSCRAMBLER(String identifier, Scrambler scrambler) {
	Scrambler.remember((Sequence.string(identifier)), scrambler);
/*
udanax-top.st:45098:Scrambler class methodsFor: 'smalltalk: macros'!
{void} DEFINE.U.SCRAMBLER: identifier {String} with: scrambler {Scrambler}
	self REQUIRES: Scrambler.
	Scrambler remember: (Sequence string: identifier)
		with: scrambler!
*/
}
public Scrambler() {
/*

Generated during transformation
*/
}
public Scrambler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
