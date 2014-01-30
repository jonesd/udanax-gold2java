/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.diskman;

import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.counter.MultiCounter;
import info.dgjones.abora.gold.diskman.DiskTester;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.DoublingFlock;
import info.dgjones.abora.gold.snarf.PairFlock;
import info.dgjones.abora.gold.snarf.SnarfPacker;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class DiskTester extends Tester {

	protected Counter myBootCounter;
/*
udanax-top.st:58208:
Tester subclass: #DiskTester
	instanceVariableNames: 'myBootCounter {Counter NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-diskman'!
*/
/*
udanax-top.st:58212:
(DiskTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DiskTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * self runTest: #destroyTest:
 */
public void destroyTest(PrintWriter oo) {
	MuTable table;
	table = MuTable.make(IntegerSpace.make());
	for (int i = 1; i <= 100; i ++ ) {
		Abraham shep;
		table.intIntroduce(i, (DoublingFlock.make(i, i)));
		shep = (Abraham) (table.intFetch(i / 2));
		if (shep != null) {
			shep.destroy();
			table.intWipe(i / 2);
		}
		Someone.unimplemented();
		/* CurrentPacker fluidVar makeConsistent. */
		if (AboraSupport.modulo(i, 20) == 0) {
			((SnarfPacker) ((DiskManager) CurrentPacker.fluidGet())).makePersistent();
		}
	}
	((DiskManager) CurrentPacker.fluidGet()).purge();
/*
udanax-top.st:58217:DiskTester methodsFor: 'tests'!
{void} destroyTest: oo {ostream reference unused} 
	"self runTest: #destroyTest:"
	| table {MuTable} |
	table _ MuTable make: IntegerSpace make.
	1 to: 100 do: [:i {Int32} |
		| shep {Abraham} |
		table atInt: i introduce: (DoublingFlock make: i with: i).
		shep _ (table intFetch: i // 2) cast: Abraham.
		shep ~~ NULL ifTrue:
			[shep destroy.
			table intWipe: i // 2].
		self unimplemented.
		"CurrentPacker fluidVar makeConsistent."
		i \\ 20 == Int32Zero ifTrue: [(CurrentPacker fluidGet cast: SnarfPacker) makePersistent]].
	CurrentPacker fluidGet purge!
*/
}
/**
 * self runTest: #forward1Test:
 */
public void forward1Test(PrintWriter oo) {
	DoublingFlock a;
	DoublingFlock b;
	SnarfPacker packer;
	a = DoublingFlock.make(1);
	b = DoublingFlock.make(2);
	packer = (SnarfPacker) ((DiskManager) CurrentPacker.fluidGet());
	oo.print("Flock a is ");
	oo.print(a);
	oo.print(" at ");
	oo.print(a.getInfo());
	oo.print("\n"+
"Flock b is ");
	oo.print(b);
	oo.print(" at ");
	oo.print(b.getInfo());
	oo.print("\n"+
"");
	packer.makePersistent();
	while (a.getInfo().snarfID() == b.getInfo().snarfID()) {
		a.doDouble();
		b.doDouble();
		oo.print("doubled to ");
		oo.print(a.count());
		oo.print("\n"+
"");
		/* a count >= 512 ifTrue: [self halt]. */
		packer.makePersistent();
	}
/*
udanax-top.st:58234:DiskTester methodsFor: 'tests'!
{void} forward1Test:  oo {ostream reference} 
	"self runTest: #forward1Test:"
	| a {DoublingFlock} b {DoublingFlock} packer {SnarfPacker} |
	a _ DoublingFlock make: 1.
	b _ DoublingFlock make: 2.
	packer _ CurrentPacker fluidGet cast: SnarfPacker.
	oo << 'Flock a is ' << a << ' at ' << a getInfo <<'
Flock b is ' << b << ' at ' << b getInfo <<'
'.	packer makePersistent.
	[a getInfo snarfID == b getInfo snarfID] whileTrue:
		[a doDouble.
		b doDouble.
		oo << 'doubled to ' << a count << '
'.		"a count >= 512 ifTrue: [self halt]."
		packer makePersistent].!
*/
}
/**
 * self runTest: #forward2Test:
 */
public void forward2Test(PrintWriter oo) {
	PairFlock pair;
	SnarfPacker packer;
	pair = new PairFlock((DoublingFlock.make(1)), (DoublingFlock.make(2)));
	packer = (SnarfPacker) ((DiskManager) CurrentPacker.fluidGet());
	oo.print("Flock a is ");
	oo.print(pair.left());
	oo.print(" at ");
	oo.print(pair.left().getInfo());
	oo.print("\n"+
"Flock b is ");
	oo.print(pair.right());
	oo.print(" at ");
	oo.print(pair.right().getInfo());
	oo.print("\n"+
"");
	packer.makePersistent();
	while (pair.left().getInfo().snarfID() == pair.right().getInfo().snarfID()) {
		((DoublingFlock) pair.left()).doDouble();
		((DoublingFlock) pair.right()).doDouble();
		oo.print("doubled to ");
		oo.print(((DoublingFlock) pair.left()).count());
		oo.print("\n"+
"");
		/* pair left count >= 512 ifTrue: [self halt]. */
		packer.purge();
	}
/*
udanax-top.st:58251:DiskTester methodsFor: 'tests'!
{void} forward2Test:  oo {ostream reference} 
	"self runTest: #forward2Test:"
	| pair {PairFlock} packer {SnarfPacker} |
	pair _ PairFlock create: (DoublingFlock make: 1) with: (DoublingFlock make: 2).
	packer _ CurrentPacker fluidGet cast: SnarfPacker.
	oo << 'Flock a is ' << pair left << ' at ' << pair left getInfo <<'
Flock b is ' << pair right << ' at ' << pair right getInfo <<'
'.	packer makePersistent.
	[pair left getInfo snarfID == pair right getInfo snarfID] whileTrue:
		[(pair left cast: DoublingFlock) doDouble.
		(pair right cast: DoublingFlock) doDouble.
		oo << 'doubled to ' << (pair left cast: DoublingFlock) count << '
'.		"pair left count >= 512 ifTrue: [self halt]."
		packer purge].!
*/
}
/**
 * self runTest: #toDiskAndBackTestOn:
 */
public void toDiskAndBackTestOn(PrintWriter aStream) {
	/* test writing to disk and reading back */
	MultiCounter firstCounter;
	MultiCounter secondCounter;
	aStream.print("\n"+
"Test ability to write an object to disk and read it back\n"+
"");
	firstCounter = MultiCounter.make(5);
	firstCounter.incrementBoth();
	secondCounter = MultiCounter.make();
	secondCounter.incrementFirst();
	secondCounter.incrementFirst();
	secondCounter.incrementBoth();
	aStream.print("\n"+
"First MultiCounter = ");
	aStream.print(firstCounter);
	aStream.print("\n"+
"Second MultiCounter = ");
	aStream.print(secondCounter);
	aStream.print("\n"+
"\n"+
"Purging.");
	((DiskManager) CurrentPacker.fluidGet()).purge();
	aStream.print("\n"+
"\n"+
"Bringing First MultiCounter back; value = ");
	aStream.print(firstCounter);
	firstCounter.decrementBoth();
	firstCounter.decrementSecond();
	aStream.print("\n"+
"First MultiCounter = ");
	aStream.print(firstCounter);
	aStream.print("\n"+
"\n"+
"Bringing Second MultiCounter back and incrementing.");
	secondCounter.incrementSecond();
	secondCounter.incrementSecond();
	secondCounter.incrementBoth();
	aStream.print("\n"+
"Second MultiCounter = ");
	aStream.print(secondCounter);
	aStream.print("\n"+
"\n"+
"Purging again.");
	((DiskManager) CurrentPacker.fluidGet()).purge();
	aStream.print("\n"+
"\n"+
"Bringing First MultiCounter back; value = ");
	aStream.print(firstCounter);
	firstCounter.decrementBoth();
	firstCounter.decrementSecond();
	aStream.print("\n"+
"First MultiCounter = ");
	aStream.print(firstCounter);
	aStream.print("\n"+
"\n"+
"Bringing Second MultiCounter back and incrementing.");
	secondCounter.incrementSecond();
	secondCounter.incrementSecond();
	secondCounter.incrementBoth();
	aStream.print("\n"+
"Second MultiCounter = ");
	aStream.print(secondCounter);
/*
udanax-top.st:58267:DiskTester methodsFor: 'tests'!
{void} toDiskAndBackTestOn:  aStream {ostream reference} 
	"self runTest: #toDiskAndBackTestOn:"
	"test writing to disk and reading back"
	| firstCounter {MultiCounter} secondCounter {MultiCounter} |
	aStream << '
Test ability to write an object to disk and read it back
'.
	firstCounter _ MultiCounter make: 5.
	firstCounter incrementBoth.
	secondCounter _ MultiCounter make.
	secondCounter incrementFirst; incrementFirst; incrementBoth.
	aStream << '
First MultiCounter = ' << firstCounter.
	aStream << '
Second MultiCounter = ' << secondCounter.
	aStream << '
Purging.'.
	CurrentPacker fluidGet purge.
	aStream << '
Bringing First MultiCounter back; value = ' << firstCounter.
	firstCounter decrementBoth; decrementSecond.
	aStream << '
First MultiCounter = ' << firstCounter.
	
	aStream << '
Bringing Second MultiCounter back and incrementing.'.
	secondCounter incrementSecond; incrementSecond; incrementBoth.
	aStream << '
Second MultiCounter = ' << secondCounter.
	aStream << '
Purging again.'.
	CurrentPacker fluidGet purge.
	aStream << '
Bringing First MultiCounter back; value = ' << firstCounter.
	firstCounter decrementBoth; decrementSecond.
	aStream << '
First MultiCounter = ' << firstCounter.
	
	aStream << '
Bringing Second MultiCounter back and incrementing.'.
	secondCounter incrementSecond; incrementSecond; incrementBoth.
	aStream << '
Second MultiCounter = ' << secondCounter.!
*/
}
/**
 * DiskTester runTest
 */
public void allTestsOn(PrintWriter oo) {
	Connection conn;
	conn = Connection.make(AboraSupport.findCategory(Counter.class));
	myBootCounter = (Counter) conn.bootHeaper();
	destroyTest(oo);
	toDiskAndBackTestOn(oo);
	forward1Test(oo);
	forward2Test(oo);
	conn.destroy();
/*
udanax-top.st:58324:DiskTester methodsFor: 'running tests'!
{void} allTestsOn: oo {ostream reference}
	"DiskTester runTest"
	
	| conn {Connection} |
	conn _ Connection make: Counter.
	myBootCounter _ conn bootHeaper cast: Counter.
	self destroyTest: oo.
	self toDiskAndBackTestOn: oo.
	self forward1Test: oo.
	self forward2Test: oo.
	conn destroy!
*/
}
public void restartDiskTester(Rcvr rcvr) {
	myBootCounter = null;
/*
udanax-top.st:58338:DiskTester methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartDiskTester: rcvr {Rcvr unused default: NULL}
	myBootCounter _ NULL!
*/
}
public DiskTester(Rcvr receiver) {
	super(receiver);
	restartDiskTester(receiver);
/*
udanax-top.st:58343:DiskTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	self restartDiskTester: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:58347:DiskTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public DiskTester() {
/*

Generated during transformation
*/
}
}
