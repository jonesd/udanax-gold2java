/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.locks.BooLock;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PrimDataArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.detect.FeStatusDetector;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeBooLockSmith;
import info.dgjones.abora.gold.nadmin.FeClubDescription;
import info.dgjones.abora.gold.nadmin.FeWallLockSmith;
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeClub;
import info.dgjones.abora.gold.nkernel.FeDataHolder;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeElementBundle;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nkernel.WorksTestFillRangeDetector;
import info.dgjones.abora.gold.nkernel.WorksTestStatusDetector;
import info.dgjones.abora.gold.nkernel.WorksTester;
import info.dgjones.abora.gold.nkernel.WorksWaitDetector;
import info.dgjones.abora.gold.nlinks.FeSingleRef;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.wrapper.FeSet;
import info.dgjones.abora.gold.wrapper.FeText;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class WorksTester extends Tester {

	protected Connection myConnection;
	protected String myCR;
	protected ID myTestID;
	protected static WorksTester TheTester;
/*
udanax-top.st:62128:
Tester subclass: #WorksTester
	instanceVariableNames: '
		myConnection {Connection NOCOPY}
		myCR {Character star NOCOPY}
		myTestID {ID NOCOPY}'
	classVariableNames: 'TheTester {WorksTester} '
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:62135:
(WorksTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
/*
udanax-top.st:62713:
WorksTester class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:62716:
(WorksTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WorksTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void allTestsOn(PrintWriter oo) {
	ID testID;
	myConnection = Connection.make(AboraSupport.findCategory(FeServer.class));
	myConnection.bootHeaper();
	CurrentKeyMaster.fluidSet(((BooLock) (FeServer.loginByName((WorksTester.sequence("Test"))))).boo());
	((FeKeyMaster) CurrentKeyMaster.fluidGet()).incorporate(FeKeyMaster.makePublic());
	testID = WorksTester.clubID((WorksTester.sequence("Test")));
	InitialOwner.fluidSet(testID);
	InitialReadClub.fluidSet(testID);
	InitialEditClub.fluidSet(testID);
	InitialSponsor.fluidSet(testID);
	CurrentAuthor.fluidSet(testID);
	makeEditionTestOn(oo);
	editionTestOn(oo);
	AboraSupport.smalltalkOnly();
	{
		((DiskManager) CurrentPacker.fluidGet()).destroyAbandoned();
	}
	crossTestOn(oo);
	compareTestOn(oo);
	AboraSupport.smalltalkOnly();
	{
		((DiskManager) CurrentPacker.fluidGet()).destroyAbandoned();
	}
	globalIDTestOn(oo);
	AboraSupport.smalltalkOnly();
	{
		((DiskManager) CurrentPacker.fluidGet()).destroyAbandoned();
	}
	workTestOn(oo);
	endorseTestOn(oo);
	AboraSupport.smalltalkOnly();
	{
		((DiskManager) CurrentPacker.fluidGet()).destroyAbandoned();
	}
	historyTestOn(oo);
	sponsorTestOn(oo);
	AboraSupport.smalltalkOnly();
	{
		((DiskManager) CurrentPacker.fluidGet()).destroyAbandoned();
	}
	kmTestOn(oo);
	transclusionsTestOn(oo);
	AboraSupport.smalltalkOnly();
	{
		((DiskManager) CurrentPacker.fluidGet()).destroyAbandoned();
	}
	transcludersBugTestOn(oo);
	ownerTestOn(oo);
	AboraSupport.smalltalkOnly();
	{
		((DiskManager) CurrentPacker.fluidGet()).destroyAbandoned();
	}
	labelTestOn(oo);
	AboraSupport.smalltalkOnly();
	{
		((DiskManager) CurrentPacker.fluidGet()).destroyAbandoned();
	}
	FeServer.waitForWrite((WorksWaitDetector.make(oo, "Test done!")));
/*
udanax-top.st:62140:WorksTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference}
	
	| testID {ID} |
	myConnection := Connection make: FeServer.
	myConnection bootHeaper.
	CurrentKeyMaster fluidSet: 
		((FeServer loginByName: (WorksTester sequence: 'Test')) cast: BooLock) boo.
	CurrentKeyMaster fluidGet incorporate: FeKeyMaster makePublic.
	testID := WorksTester clubID: (WorksTester sequence: 'Test').
	InitialOwner fluidSet: testID.
	InitialReadClub fluidSet: testID.
	InitialEditClub fluidSet: testID.
	InitialSponsor fluidSet: testID.
	CurrentAuthor fluidSet: testID.
	
	self makeEditionTestOn: oo.
	self editionTestOn: oo.
[CurrentPacker fluidGet destroyAbandoned] smalltalkOnly.
	self crossTestOn: oo.
	self compareTestOn: oo.
[CurrentPacker fluidGet destroyAbandoned] smalltalkOnly.
	
	self globalIDTestOn: oo.
[CurrentPacker fluidGet destroyAbandoned] smalltalkOnly.
	self workTestOn: oo.
	self endorseTestOn: oo.
[CurrentPacker fluidGet destroyAbandoned] smalltalkOnly.
	self historyTestOn: oo.
	self sponsorTestOn: oo.
[CurrentPacker fluidGet destroyAbandoned] smalltalkOnly.
	self kmTestOn: oo.
	self transclusionsTestOn: oo.
[CurrentPacker fluidGet destroyAbandoned] smalltalkOnly.
	self transcludersBugTestOn: oo.
	self ownerTestOn: oo.
[CurrentPacker fluidGet destroyAbandoned] smalltalkOnly.
	self labelTestOn: oo.
[CurrentPacker fluidGet destroyAbandoned] smalltalkOnly.
	
	FeServer waitForWrite: (WorksWaitDetector make: oo with: 'Test done!!').!
*/
}
/**
 * Test the various version comparision operations
 */
public void compareTestOn(PrintWriter oo) {
	FeEdition a;
	FeEdition b;
	FeWork work1;
	FeWork work2;
	FeWork work3;
	FeEdition edn;
	XnRegion region;
	a = FeEdition.placeHolders((IntegerRegion.interval(0, 100)));
	b = (((a.copy((IntegerSpace.make().below((IntegerPos.make(50)), false)))).transformedBy((IntegerSpace.make().translation(100)))).combine((FeEdition.placeHolders((IntegerSpace.make().interval((IntegerPos.make(0)), (IntegerPos.make(50)))))))).combine(((a.copy((IntegerSpace.make().interval((IntegerPos.make(25)), (IntegerPos.make(75)))))).transformedBy((IntegerSpace.make().translation(25)))));
	oo.print("a sharedWith b: ");
	oo.print((a.sharedWith(b)));
	oo.print(myCR);
	oo.print("a notSharedWith b: ");
	oo.print((a.notSharedWith(b)));
	oo.print(myCR);
	oo.print("a sharedRegion b: ");
	oo.print((a.sharedRegion(b)));
	oo.print(myCR);
	oo.print("a mapSharedTo b: ");
	oo.print((a.mapSharedTo(b)));
	oo.print(myCR);
	oo.print("a sharedRegion b copy [120,130): ");
	oo.print((a.sharedRegion((b.copy((IntegerRegion.make(120, 130)))))));
	oo.print(myCR);
	oo.print("a keysOf a[50]: ");
	oo.print((a.positionsOf((a.get((IntegerPos.make(50)))))));
	oo.print(myCR);
	oo.print("b sharedWith a: ");
	oo.print((b.sharedWith(a)));
	oo.print(myCR);
	oo.print("b notSharedWith a: ");
	oo.print((b.notSharedWith(a)));
	oo.print(myCR);
	oo.print("b mapSharedTo a: ");
	oo.print((b.mapSharedTo(a)));
	oo.print(myCR);
	oo.print("b sharedRegion a: ");
	oo.print((b.sharedRegion(a)));
	oo.print(myCR);
	oo.print("b sharedRegion a copy [20,30): ");
	oo.print((b.sharedRegion((a.copy((IntegerRegion.make(20, 30)))))));
	oo.print(myCR);
	oo.print("b positionsOf a[50]: ");
	oo.print((b.positionsOf((a.get((IntegerPos.make(50)))))));
	oo.print(myCR);
	work1 = FeWork.make((FeText.make((UInt8Array.string("foo")))).edition());
	work2 = FeWork.make((FeEdition.fromOne(IntegerPos.make(0), work1)));
	edn = FeEdition.fromOne(IntegerPos.make(0), work1);
	work3 = (FeWork) work2.edition().theOne();
	region = edn.positionsOf(work3);
	oo.print("region = ");
	oo.print(region);
	oo.print(myCR);
/*
udanax-top.st:62186:WorksTester methodsFor: 'tests'!
{void} compareTestOn: oo {ostream reference}
	"Test the various version comparision operations"
	
	| a {FeEdition} b {FeEdition} work1 {FeWork} work2 {FeWork} work3 {FeWork} edn {FeEdition} region {XnRegion} |
	a := FeEdition placeHolders: (IntegerRegion interval: Int32Zero with:100).
	b := (((a copy: (IntegerSpace make below: (IntegerPos make: 50) with: false))
		transformedBy: (IntegerSpace make translation: 100))
		combine: (FeEdition placeHolders: (IntegerSpace make interval: (IntegerPos make: Int32Zero) with: (IntegerPos make: 50))))
		combine: ((a copy: (IntegerSpace make interval: (IntegerPos make: 25) with: (IntegerPos make: 75)))
		transformedBy: (IntegerSpace make translation: 25)).
	oo << 'a sharedWith b: ' << (a sharedWith: b) << myCR
		<< 'a notSharedWith b: ' << (a notSharedWith: b) << myCR
		<< 'a sharedRegion b: ' << (a sharedRegion: b) << myCR
		<< 'a mapSharedTo b: ' << (a mapSharedTo: b) << myCR
		<< 'a sharedRegion b copy [120,130): '
			<< (a sharedRegion: (b copy: (IntegerRegion make: 120 with: 130))) << myCR
		<< 'a keysOf a[50]: ' << (a positionsOf: (a get: (IntegerPos make: 50))) << myCR
		<< 'b sharedWith a: ' << (b sharedWith: a) << myCR
		<< 'b notSharedWith a: ' << (b notSharedWith: a) << myCR
		<< 'b mapSharedTo a: ' << (b mapSharedTo: a) << myCR
		<< 'b sharedRegion a: ' << (b sharedRegion: a) << myCR
		<< 'b sharedRegion a copy [20,30): '
			<< (b sharedRegion: (a copy: (IntegerRegion make: 20 with: 30))) << myCR
		<< 'b positionsOf a[50]: ' << (b positionsOf: (a get: (IntegerPos make: 50))) << myCR.
	work1 := FeWork make: (FeText make: (UInt8Array string: 'foo')) edition.
	work2 := FeWork make: (FeEdition fromOne: Int32Zero integer with: work1).
	edn := FeEdition fromOne: Int32Zero integer with: work1.
	work3 := work2 edition theOne cast: FeWork.
	region := edn positionsOf: work3.
	oo << 'region = ' << region << myCR.!
*/
}
public void crossTestOn(PrintWriter oo) {
	PtrArray four;
	IDSpace is;
	CrossSpace cross;
	FeEdition doc;
	oo.print(myCR);
	oo.print("CrossSpace retrieval test");
	oo.print(myCR);
	four = PtrArray.nulls(4);
	is = IDSpace.unique();
	four.store(0, is);
	four.store(1, IntegerSpace.make());
	four.store(2, IntegerSpace.make());
	four.store(3, IntegerSpace.make());
	cross = CrossSpace.make(four);
	doc = FeEdition.empty(cross);
	for (int i = 1; i <= 10; i ++ ) {
		four.store(0, is.newID().asRegion());
		four.store(1, (IntegerSpace.make().interval((IntegerPos.make(i)), (IntegerPos.make(i + 4)))));
		four.store(2, (IntegerSpace.make().interval((IntegerPos.make(i)), (IntegerPos.make(21 - i)))));
		four.store(3, (IntegerSpace.make().interval((IntegerPos.make(i)), (IntegerPos.make(i + 1)))));
		doc = doc.combine((FeEdition.fromAll((cross.crossOfRegions(four)), (FeDataHolder.make((PrimIntValue.make(i)))))));
	}
	for (int j = 1; j <= 3; j ++ ) {
		oo.print("Looking for dimension ");
		oo.print(j);
		oo.print(" >= 10");
		oo.print(myCR);
		Stepper stomper = (doc.copy((cross.extrusion(j, (IntegerSpace.make().above((IntegerPos.make(10)), true)))))).retrieve();
		for (; stomper.hasValue(); stomper.step()) {
			FeElementBundle bundle = (FeElementBundle) stomper.fetch();
			if (bundle == null) {
				continue ;
			}
			oo.print("found ");
			oo.print(bundle.element());
			oo.print(" at ");
			oo.print(bundle.region());
			oo.print(myCR);
		}
		stomper.destroy();
	}
/*
udanax-top.st:62217:WorksTester methodsFor: 'tests'!
{void} crossTestOn: oo {ostream reference}
	| four {PtrArray} is {IDSpace} cross {CrossSpace} doc {FeEdition} |
	oo << myCR << 'CrossSpace retrieval test' <<myCR.
	four := PtrArray nulls: 4.
	is := IDSpace unique.
	four at: UInt32Zero store: is.
	four at: 1 store: IntegerSpace make.
	four at: 2 store: IntegerSpace make.
	four at: 3 store: IntegerSpace make.
	cross := CrossSpace make: four.
	doc := FeEdition empty: cross.
	
	1 to: 10 do: [ :i {Int32} |
		four at: UInt32Zero store: is newID asRegion.
		four at: 1 store: (IntegerSpace make interval: (IntegerPos make: i) with: (IntegerPos make: i + 4)).
		four at: 2 store: (IntegerSpace make interval: (IntegerPos make: i) with: (IntegerPos make: 21 - i)).
		four at: 3 store: (IntegerSpace make interval: (IntegerPos make: i) with: (IntegerPos make: i + 1)).
		doc := doc combine: (FeEdition fromAll: (cross crossOfRegions: four)
			with: (FeDataHolder make: (PrimIntValue make: i)))].
	
	1 to: 3 do: [ :j {Int32} |
		oo << 'Looking for dimension ' << j << ' >= 10' << myCR.
		(doc copy: (cross extrusion: j with: (IntegerSpace make above: (IntegerPos make: 10) with: true)))
			retrieve forEach: [ :bundle {FeElementBundle} |
				oo << 'found ' << bundle element << ' at ' << bundle region << myCR]].!
*/
}
/**
 * Test the simple Edition operations
 */
public void editionTestOn(PrintWriter oo) {
	FeEdition edition;
	oo.print("Testing various Edition operations");
	oo.print(myCR);
	edition = FeEdition.empty(IntegerSpace.make());
	oo.print("initially: ");
	oo.print(edition);
	oo.print(myCR);
	oo.print(" coordinateSpace: ");
	oo.print(edition.coordinateSpace());
	oo.print(myCR);
	oo.print(" count: ");
	oo.print(edition.count());
	oo.print(myCR);
	oo.print(" domain: ");
	oo.print(edition.domain());
	oo.print(myCR);
	oo.print(" isEmpty: ");
	oo.print(edition.isEmpty());
	oo.print(myCR);
	oo.print(" isFinite: ");
	oo.print(edition.isFinite());
	oo.print(myCR);
	edition = edition.with((IntegerPos.make(0)), FeRangeElement.placeHolder());
	oo.print("with(0): ");
	oo.print(edition);
	oo.print(myCR);
	oo.print(" theOne: ");
	oo.print(edition.theOne());
	oo.print(myCR);
	edition = edition.withAll((IntegerSpace.make().above((IntegerPos.make(1)), true)), (FeDataHolder.make((PrimIntValue.make(65)))));
	oo.print("withAll: ");
	oo.print(edition);
	oo.print(myCR);
	oo.print(" domain: ");
	oo.print(edition.domain());
	oo.print(myCR);
	oo.print(" isEmpty: ");
	oo.print(edition.isEmpty());
	oo.print(myCR);
	oo.print(" isFinite: ");
	oo.print(edition.isFinite());
	oo.print(myCR);
	oo.print("stepper:");
	oo.print(myCR);
	TableStepper stomper = (edition.stepper((IntegerSpace.make().interval((IntegerPos.make(0)), (IntegerPos.make(2))))));
	for (; stomper.hasValue(); stomper.step()) {
		Position p = (Position) stomper.position();
		FeRangeElement v = (FeRangeElement) stomper.fetch();
		if (v == null) {
			continue ;
		}
		oo.print(" ");
		oo.print(p);
		oo.print(" -> ");
		oo.print(v);
		oo.print(myCR);
	}
	stomper.destroy();
	edition = edition.without((IntegerPos.make(3)));
	oo.print("without 3");
	oo.print(edition);
	oo.print(myCR);
	edition = edition.withoutAll((IntegerSpace.make().above((IntegerPos.make(2)), true)));
	oo.print("withoutAll: ");
	oo.print(edition);
	oo.print(myCR);
	oo.print(" count: ");
	oo.print(edition.count());
	oo.print(myCR);
	oo.print(" domain: ");
	oo.print(edition.domain());
	oo.print(myCR);
	oo.print(" isEmpty: ");
	oo.print(edition.isEmpty());
	oo.print(myCR);
	oo.print(" isFinite: ");
	oo.print(edition.isFinite());
	oo.print(myCR);
	oo.print(" get 1: ");
	oo.print((edition.get((IntegerPos.make(1)))));
	oo.print(myCR);
	oo.print("combined: ");
	oo.print((edition.combine((FeEdition.fromOne((IntegerPos.make(5)), FeRangeElement.placeHolder())))));
	oo.print(myCR);
	oo.print("replaced: ");
	oo.print((edition.replace((FeEdition.fromOne((IntegerPos.make(1)), FeRangeElement.placeHolder())))));
	oo.print(myCR);
/*
udanax-top.st:62244:WorksTester methodsFor: 'tests'!
{void} editionTestOn: oo {ostream reference}
	"Test the simple Edition operations"
	
	| edition {FeEdition} |
	oo << 'Testing various Edition operations' << myCR.
	edition := FeEdition empty: IntegerSpace make.
	oo << 'initially: ' << edition << myCR
		<< ' coordinateSpace: ' << edition coordinateSpace << myCR
		<< ' count: ' << edition count << myCR
		<< ' domain: ' << edition domain << myCR
		<< ' isEmpty: ' << edition isEmpty << myCR
		<< ' isFinite: ' << edition isFinite << myCR.
	edition := edition with: (IntegerPos make: Int32Zero) with: FeRangeElement placeHolder.
	oo << 'with(0): ' << edition << myCR
		<< ' theOne: ' << edition theOne << myCR.
	edition := edition withAll: (IntegerSpace make above: (IntegerPos make: 1) with: true)
		with: (FeDataHolder make: (PrimIntValue make: 65)).
	oo << 'withAll: ' << edition << myCR
		<< ' domain: ' << edition domain << myCR
		<< ' isEmpty: ' << edition isEmpty << myCR
		<< ' isFinite: ' << edition isFinite << myCR.
	oo << 'stepper:' << myCR.
	[ScruTable] USES.
	(edition stepper: (IntegerSpace make interval: (IntegerPos make: IntegerVarZero) with: (IntegerPos make: 2))) forPositions:
		[ :p {Position} :v {FeRangeElement} |
		oo << ' ' << p << ' -> ' << v << myCR].
	edition := edition without: (IntegerPos make: 3).
	oo << 'without 3' << edition << myCR.
	edition := edition withoutAll: (IntegerSpace make above: (IntegerPos make: 2) with: true).
	oo << 'withoutAll: ' << edition << myCR
		<< ' count: ' << edition count << myCR
		<< ' domain: ' << edition domain << myCR
		<< ' isEmpty: ' << edition isEmpty << myCR
		<< ' isFinite: ' << edition isFinite << myCR
		<< ' get 1: ' << (edition get: (IntegerPos make: 1)) << myCR.
	
	oo << 'combined: ' << (edition combine: (FeEdition fromOne: (IntegerPos make: 5)
			with: FeRangeElement placeHolder)) << myCR.
	oo << 'replaced: ' << (edition replace: (FeEdition fromOne: (IntegerPos make: 1)
			with: FeRangeElement placeHolder)) << myCR.!
*/
}
/**
 * Test endorsing and unendorsing Editions and Works
 */
public void endorseTestOn(PrintWriter oo) {
	FeEdition e1;
	FeWork w1;
	ID iD;
	IDRegion userRegion;
	e1 = FeEdition.empty(IntegerSpace.make());
	w1 = FeWork.make(e1);
	oo.print("Initial endorsements:");
	oo.print(myCR);
	oo.print("  on Edition: ");
	oo.print(e1.endorsements());
	oo.print(myCR);
	oo.print("  on Work: ");
	oo.print(w1.endorsements());
	oo.print(myCR);
	oo.print(myCR);
	userRegion = (IDRegion) ((ID) CurrentAuthor.fluidGet()).asRegion();
	e1.endorse((FeServer.endorsementRegion(userRegion, userRegion)));
	iD = IDSpace.global().newID();
	w1.endorse((FeServer.endorsementRegion(userRegion, ((IDRegion) iD.asRegion()))));
	oo.print("After endorsing:");
	oo.print(myCR);
	oo.print("  on Edition: ");
	oo.print(e1.endorsements());
	oo.print(myCR);
	oo.print("  on Work: ");
	oo.print(w1.endorsements());
	oo.print(myCR);
	e1.retract((FeServer.endorsementRegion(userRegion, userRegion)));
	w1.retract((FeServer.endorsementRegion(userRegion, ((IDRegion) iD.asRegion()))));
	oo.print("After unendorsing:");
	oo.print(myCR);
	oo.print("  on Edition: ");
	oo.print(e1.endorsements());
	oo.print(myCR);
	oo.print("  on Work: ");
	oo.print(w1.endorsements());
	oo.print(myCR);
/*
udanax-top.st:62285:WorksTester methodsFor: 'tests'!
{void} endorseTestOn: oo {ostream reference}
	"Test endorsing and unendorsing Editions and Works"
	
	| e1 {FeEdition} w1 {FeWork} iD {ID} userRegion {IDRegion} |
	e1 := FeEdition empty: IntegerSpace make.
	w1 := FeWork make: e1.
	oo << 'Initial endorsements:' << myCR
		<< '  on Edition: ' << e1 endorsements << myCR
		<< '  on Work: ' << w1 endorsements << myCR << myCR.
	
	userRegion _ CurrentAuthor fluidGet asRegion cast: IDRegion.
	e1 endorse: (FeServer endorsementRegion: userRegion with: userRegion).
	iD := IDSpace global newID.
	w1 endorse: (FeServer endorsementRegion: userRegion with: (iD asRegion cast: IDRegion)).
	oo << 'After endorsing:' << myCR
		<< '  on Edition: ' << e1 endorsements << myCR
		<< '  on Work: ' << w1 endorsements << myCR.
	
	e1 retract: (FeServer endorsementRegion: userRegion with: userRegion).
	w1 retract: (FeServer endorsementRegion: userRegion with: (iD asRegion cast: IDRegion)).
	oo << 'After unendorsing:' << myCR
		<< '  on Edition: ' << e1 endorsements << myCR
		<< '  on Work: ' << w1 endorsements << myCR.!
*/
}
/**
 * Test assigning and retrieving by global IDs
 */
public void globalIDTestOn(PrintWriter oo) {
	FeRangeElement p1;
	ID id1a;
	ID id1b;
	IDRegion ids;
	FeRangeElement p2;
	ID id2;
	FeEdition ed;
	p1 = FeRangeElement.placeHolder();
	if ( ! ((ids = FeServer.iDsOf(p1)).isEmpty())) {
		oo.print("Newly created place holder ");
		oo.print(p1);
		oo.print(" should not have had any IDs but was reported to have ");
		oo.print(ids);
		oo.print(myCR);
	}
	id1a = FeServer.assignID(p1);
	if ( ! ((ids = FeServer.iDsOf(p1)).isEqual(id1a.asRegion()))) {
		oo.print("PlaceHolder ");
		oo.print(p1);
		oo.print(" should have IDs ");
		oo.print(id1a.asRegion());
		oo.print(" but was reported to have IDs ");
		oo.print(ids);
		oo.print(myCR);
	}
	id1b = FeServer.assignID(p1);
	if ( ! ((ids = FeServer.iDsOf(p1)).isEqual((id1a.asRegion().with(id1b))))) {
		oo.print("PlaceHolder ");
		oo.print(p1);
		oo.print(" should have IDs ");
		oo.print((id1a.asRegion().with(id1b)));
		oo.print(" but was reported to have IDs ");
		oo.print(ids);
		oo.print(myCR);
	}
	p2 = FeRangeElement.placeHolder();
	id2 = FeServer.assignID(p2);
	ed = (FeEdition.fromOne((IntegerPos.make(0)), p1)).combine((FeEdition.fromOne((IntegerPos.make(1)), p2)));
	if ( ! ((ids = FeServer.iDsOfRange(ed)).isEqual(((id1a.asRegion().with(id1b)).with(id2))))) {
		oo.print("PlaceHolders ");
		oo.print(ed);
		oo.print(" should have IDs ");
		oo.print(((id1a.asRegion().with(id1b)).with(id2)));
		oo.print(" but was reported to have IDs ");
		oo.print(ids);
		oo.print(myCR);
	}
	oo.print("Global ID assignment test successful\n"+
"");
/*
udanax-top.st:62309:WorksTester methodsFor: 'tests'!
{void} globalIDTestOn: oo {ostream reference}
	"Test assigning and retrieving by global IDs"
	
	| p1 {FeRangeElement as: FePlaceHolder} id1a {ID} id1b {ID} ids {IDRegion}
	  p2 {FeRangeElement as: FePlaceHolder} id2 {ID} ed {FeEdition} |
	p1 := FeRangeElement placeHolder.
	(ids := FeServer iDsOf: p1) isEmpty ifFalse:
		[oo << 'Newly created place holder ' << p1
			<< ' should not have had any IDs but was reported to have ' << ids << myCR].
	id1a := FeServer assignID: p1.
	((ids := FeServer iDsOf: p1) isEqual: id1a asRegion) ifFalse:
		[oo << 'PlaceHolder ' << p1
			<< ' should have IDs ' << id1a asRegion
			<< ' but was reported to have IDs ' << ids << myCR].
	id1b := FeServer assignID: p1.
	((ids := FeServer iDsOf: p1) isEqual: (id1a asRegion with: id1b)) ifFalse:
		[oo << 'PlaceHolder ' << p1
			<< ' should have IDs ' << (id1a asRegion with: id1b)
			<< ' but was reported to have IDs ' << ids << myCR].
	p2 := FeRangeElement placeHolder.
	id2 := FeServer assignID: p2.
	ed := (FeEdition fromOne: (IntegerPos make: Int32Zero) with: p1)
		combine: (FeEdition fromOne: (IntegerPos make: 1) with: p2).
	((ids := FeServer iDsOfRange: ed)
			isEqual: ((id1a asRegion with: id1b) with: id2))
		ifFalse:
			[oo << 'PlaceHolders ' << ed
				<< ' should have IDs ' << ((id1a asRegion with: id1b) with: id2)
				<< ' but was reported to have IDs ' << ids << myCR].
	oo << 'Global ID assignment test successful
'!
*/
}
public void historyTestOn(PrintWriter oo) {
	FeWork work;
	work = FeWork.make((FeEdition.fromArray((WorksTester.string("Howdy doody.")))));
	work.setHistoryClub(FeServer.publicClubID());
	work.revise((FeEdition.fromArray((WorksTester.string("Good bye")))));
	work.revise((FeEdition.fromArray((WorksTester.string("Much better.")))));
	oo.print("The trail is: ");
	oo.print(work.revisions());
	oo.print(myCR);
	TableStepper stomper = work.revisions().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Position position = (Position) stomper.position();
		FeWork value = (FeWork) stomper.fetch();
		if (value == null) {
			continue ;
		}
		oo.print(position);
		oo.print("->");
		oo.print(((FeArrayBundle) value.edition().retrieve().theOne()).array());
		oo.print(myCR);
	}
	stomper.destroy();
	/* self halt. */
	oo.print(myCR);
/*
udanax-top.st:62342:WorksTester methodsFor: 'tests'!
{void} historyTestOn: oo {ostream reference}
	| work {FeWork} |
	work _ FeWork make: (FeEdition fromArray: (WorksTester string: 'Howdy doody.')).
	work setHistoryClub: FeServer publicClubID.
	work revise: (FeEdition fromArray: (WorksTester string: 'Good bye')).
	work revise: (FeEdition fromArray: (WorksTester string: 'Much better.')).
	oo << 'The trail is: ' << work revisions << myCR.
	work revisions stepper forPositions: 
		[:position {Position} :value {FeWork} |
		oo << position << '->' << (value edition retrieve theOne cast: FeArrayBundle) array << myCR].
	
	"self halt."
	oo << myCR.!
*/
}
/**
 * Test the operation of KeyMasters
 */
public void kmTestOn(PrintWriter oo) {
	FeKeyMaster km;
	FeWrapperSpec clubspec;
	FeClub test;
	FeClub club1;
	FeStatusDetector detect1;
	FeWork work1;
	FeClub club2;
	FeStatusDetector detect2;
	FeWork work2;
	FeClubDescription desc;
	km = ((FeKeyMaster) CurrentKeyMaster.fluidGet()).copy();
	Object currentKeyMasterOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentKeyMaster, km);
	try {
		clubspec = FeWrapperSpec.get((WorksTester.sequence("ClubDescription")));
		test = (FeClub) (FeServer.get(((ID) CurrentAuthor.fluidGet())));
		club1 = (FeClub) FeClub.make((FeClubDescription.make(FeSet.make(), FeBooLockSmith.make())).edition());
		oo.print("Club1 ");
		oo.print((FeServer.iDOf(club1)));
		oo.print(" is initially ");
		oo.print((clubspec.wrap(club1.edition())));
		oo.print(myCR);
		oo.print("and CurrentKeyMaster is ");
		oo.print(km.actualAuthority());
		oo.print(myCR);
		oo.print(myCR);
		oo.print("Logged in as ");
		oo.print(((BooLock) (FeServer.login((FeServer.iDOf(club1))))).boo());
		club2 = (FeClub) FeClub.make((FeClubDescription.make(FeSet.make(), FeBooLockSmith.make())).edition());
		oo.print("Club 2 ");
		oo.print((FeServer.iDOf(club2)));
		oo.print(" is initially ");
		oo.print((clubspec.wrap(club2.edition())));
		oo.print(myCR);
		oo.print("and CurrentKeyMaster is ");
		oo.print(km.actualAuthority());
		oo.print(myCR);
		oo.print(myCR);
		Object initialEditClubOldValue = AboraBlockSupport.enterFluidBindDuring(InitialEditClub, (FeServer.iDOf(club1)));
		try {
			Object initialReadClubOldValue = AboraBlockSupport.enterFluidBindDuring(InitialReadClub, (FeServer.iDOf(club1)));
			try {
				work1 = FeWork.make((FeEdition.empty(IntegerSpace.make())));
			}
			finally {
				AboraBlockSupport.exitFluidBindDuring(InitialReadClub, initialReadClubOldValue);
			}
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(InitialEditClub, initialEditClubOldValue);
		}
		detect1 = (WorksTestStatusDetector.make(oo, "\n"+
"Work 1"));
		work1.addStatusDetector(detect1);
		oo.print("Giving Work 1 edit authority to Club 1");
		oo.print(myCR);
		work1.requestGrab();
		Object initialEditClubOldValue1 = AboraBlockSupport.enterFluidBindDuring(InitialEditClub, (FeServer.iDOf(club2)));
		try {
			work2 = FeWork.make((FeEdition.empty(IntegerSpace.make())));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(InitialEditClub, initialEditClubOldValue1);
		}
		detect2 = (WorksTestStatusDetector.make(oo, "\n"+
"Work 2"));
		work2.addStatusDetector(detect2);
		oo.print("Giving Work 2 edit authority to Club 2");
		oo.print(myCR);
		work2.requestGrab();
		desc = (FeClubDescription) (clubspec.wrap(club1.edition()));
		club1.revise((desc.withMembership((desc.membership().with(test)))).edition());
		oo.print("Club 1 should now have Test as a member: ");
		oo.print((clubspec.wrap(club1.edition())));
		oo.print(myCR);
		oo.print("So CurrentKeyMaster should have Club 1 authority: ");
		oo.print(km.actualAuthority());
		oo.print(myCR);
		oo.print("and Work 1 should have become grabbed: ");
		oo.print(work1.canRevise());
		oo.print(myCR);
		oo.print(myCR);
		desc = (FeClubDescription) (clubspec.wrap(club2.edition()));
		club2.revise((desc.withMembership((desc.membership().with(club1)))).edition());
		oo.print("Club 2 should now have Club 1 as a member: ");
		oo.print((clubspec.wrap(club2.edition())));
		oo.print(myCR);
		oo.print("So CurrentKeyMaster should have Club 2 authority: ");
		oo.print(km.actualAuthority());
		oo.print(myCR);
		oo.print("and Work 2 should have become grabbed: ");
		oo.print(work2.canRevise());
		oo.print(myCR);
		oo.print(myCR);
		desc = (FeClubDescription) (clubspec.wrap(club2.edition()));
		club2.revise((desc.withMembership(((desc.membership().without(club1)).with((FeServer.get(FeServer.publicClubID())))))).edition());
		oo.print("Club 2 should have Public but not Club 1 as a member: ");
		oo.print((clubspec.wrap(club2.edition())));
		oo.print(myCR);
		oo.print("So CurrentKeyMaster should retain Club 2 authority: ");
		oo.print(km.actualAuthority());
		oo.print(myCR);
		oo.print("and Work 2 should remain grabbed: ");
		oo.print(work2.canRevise());
		oo.print(myCR);
		oo.print(myCR);
		km.removeLogins(((IDRegion) FeServer.publicClubID().asRegion()));
		oo.print("The combined KeyMaster should have lost Public & Club 2 authority: ");
		oo.print(" login ");
		oo.print(km.loginAuthority());
		oo.print(myCR);
		oo.print(" actual ");
		oo.print(km.actualAuthority());
		oo.print(myCR);
		oo.print("and Work 2 should have become released but readable:");
		oo.print(" canRevise ");
		oo.print(work2.canRevise());
		oo.print(" canRead ");
		oo.print(work2.canRead());
		oo.print(myCR);
		oo.print(myCR);
		desc = (FeClubDescription) (clubspec.wrap(club1.edition()));
		club1.revise((desc.withMembership((desc.membership().without(test)))).edition());
		oo.print("Club 1 should no longer have Test as a member: ");
		oo.print((clubspec.wrap(club1.edition())));
		oo.print(myCR);
		oo.print("So CurrentKeyMaster should not have Club 1 authority: ");
		oo.print(km.actualAuthority());
		oo.print(myCR);
		oo.print("and Work 1 should have become released and unreadable:");
		oo.print(" canRevise ");
		oo.print(work1.canRevise());
		oo.print(" canRead ");
		oo.print(work1.canRead());
		oo.print(myCR);
		oo.print(myCR);
		/* work2 removeStatusDetector: detect2. */
		/* work1 removeStatusDetector: detect1. */
		club1.release();
		club2.release();
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentKeyMaster, currentKeyMasterOldValue);
	}
	Someone.thingToDo();
	/* Clean up persistent information in Server */
/*
udanax-top.st:62356:WorksTester methodsFor: 'tests'!
{void} kmTestOn: oo {ostream reference}
	"Test the operation of KeyMasters"
	
	| km {FeKeyMaster} clubspec {FeWrapperSpec} test {FeClub}
	  club1 {FeClub} detect1 {FeStatusDetector} work1 {FeWork}
	  club2 {FeClub} detect2 {FeStatusDetector} work2 {FeWork}
	  desc {FeClubDescription}  |
	
	km := CurrentKeyMaster fluidGet copy.
	CurrentKeyMaster fluidBind: km during:
		[clubspec := FeWrapperSpec get: (WorksTester sequence: 'ClubDescription').
		test := (FeServer get: CurrentAuthor fluidGet) cast: FeClub.
		
		club1 := FeClub make: 
				(FeClubDescription make: FeSet make with: FeBooLockSmith make) edition.
		oo << 'Club1 ' << (FeServer iDOf: club1) << ' is initially '
			<< (clubspec wrap: club1 edition) << myCR
			<< 'and CurrentKeyMaster is ' << km actualAuthority << myCR << myCR.
		oo << 'Logged in as ' << ((FeServer login: (FeServer iDOf: club1)) cast: BooLock) boo.
		
		club2 := FeClub make: (FeClubDescription make: FeSet make with: FeBooLockSmith make) edition.
		oo << 'Club 2 ' << (FeServer iDOf: club2) << ' is initially '
			<< (clubspec wrap: club2 edition) << myCR
			<< 'and CurrentKeyMaster is ' << km actualAuthority << myCR << myCR.
		
		InitialEditClub fluidBind: (FeServer iDOf: club1) during:
		[InitialReadClub fluidBind: (FeServer iDOf: club1) during:
			[work1 := FeWork make: (FeEdition empty: IntegerSpace make)]].
		detect1 := (WorksTestStatusDetector make: oo with: '
Work 1').
		work1 addStatusDetector: detect1.
		oo << 'Giving Work 1 edit authority to Club 1' << myCR.
		work1 requestGrab.
		
		InitialEditClub fluidBind: (FeServer iDOf: club2) during:
			[work2 := FeWork make: (FeEdition empty: IntegerSpace make)].
		detect2 := (WorksTestStatusDetector make: oo with: '
Work 2').
		work2 addStatusDetector: detect2.
		oo << 'Giving Work 2 edit authority to Club 2' << myCR.
		work2 requestGrab.
		
		desc := (clubspec wrap: club1 edition) cast: FeClubDescription.
		club1 revise: (desc withMembership: (desc membership with: test)) edition.
		oo << 'Club 1 should now have Test as a member: '
			<< (clubspec wrap: club1 edition) << myCR
			<< 'So CurrentKeyMaster should have Club 1 authority: '
			<< km actualAuthority << myCR
			<< 'and Work 1 should have become grabbed: ' << work1 canRevise << myCR << myCR.
		
		desc := (clubspec wrap: club2 edition) cast: FeClubDescription.
		club2 revise: (desc withMembership: (desc membership with: club1)) edition.
		oo << 'Club 2 should now have Club 1 as a member: '
			<< (clubspec wrap: club2 edition) << myCR
			<< 'So CurrentKeyMaster should have Club 2 authority: '
			<< km actualAuthority << myCR
			<< 'and Work 2 should have become grabbed: ' << work2 canRevise << myCR << myCR.
		
		desc := (clubspec wrap: club2 edition) cast: FeClubDescription.
		club2 revise: (desc
			withMembership: ((desc membership
				without: club1) with: (FeServer get: FeServer publicClubID))) edition.
		oo << 'Club 2 should have Public but not Club 1 as a member: '
			<< (clubspec wrap: club2 edition) << myCR
			<< 'So CurrentKeyMaster should retain Club 2 authority: '
			<< km actualAuthority << myCR
			<< 'and Work 2 should remain grabbed: ' << work2 canRevise << myCR << myCR.
		
		km removeLogins: (FeServer publicClubID asRegion cast: IDRegion).
		oo << 'The combined KeyMaster should have lost Public & Club 2 authority: '
			<< ' login ' << km loginAuthority << myCR
			<< ' actual ' << km actualAuthority << myCR
			<< 'and Work 2 should have become released but readable:'
			<< ' canRevise ' << work2 canRevise
			<< ' canRead ' << work2 canRead << myCR << myCR.
		
		desc := (clubspec wrap: club1 edition) cast: FeClubDescription.
		club1 revise: (desc withMembership: (desc membership without: test)) edition.
		oo << 'Club 1 should no longer have Test as a member: '
			<< (clubspec wrap: club1 edition) << myCR
			<< 'So CurrentKeyMaster should not have Club 1 authority: '
			<< km actualAuthority << myCR
			<< 'and Work 1 should have become released and unreadable:'
			<< ' canRevise ' << work1 canRevise
			<< ' canRead ' << work1 canRead << myCR << myCR.
		
		"work2 removeStatusDetector: detect2."
		"work1 removeStatusDetector: detect1."
		club1 release.
		club2 release].
	self thingToDo. "Clean up persistent information in Server"!
*/
}
public void labelTestOn(PrintWriter oo) {
	FeEdition edition;
	FeEdition e1;
	FeEdition e2;
	FeEdition e3;
	FeEdition e4;
	FeEdition e1prime;
	FeEdition edition2;
	e1 = FeEdition.fromArray((WorksTester.string("First Edition")));
	e2 = FeEdition.fromArray((WorksTester.string("Second Edition")));
	e3 = FeEdition.fromArray((WorksTester.string("Third Edition")));
	e4 = FeEdition.fromArray((WorksTester.string("Fourth Edition")));
	edition = FeEdition.fromArray((PrimSpec.pointer().arrayWithThree(e1, e2, (FeWork.make(e1)))));
	oo.print("Labels:");
	oo.print(myCR);
	oo.print(" ");
	oo.print(e1.label());
	oo.print(" ");
	oo.print(e2.label());
	oo.print(" ");
	oo.print(e3.label());
	oo.print(" ");
	oo.print(e4.label());
	oo.print(myCR);
	oo.print("labelled e1: ");
	oo.print((edition.positionsLabelled(e1.label())));
	oo.print(myCR);
	e1prime = ((FeEdition) (edition.fetch((IntegerPos.make(0))))).with((IntegerPos.make(1)), FeRangeElement.placeHolder());
	edition2 = edition.with((IntegerPos.make(0)), e1prime);
	oo.print("edit e1: ");
	oo.print((edition2.positionsLabelled(e1.label())));
	oo.print(myCR);
	oo.print("labelled e2: ");
	oo.print((edition2.positionsLabelled(e2.label())));
	oo.print(myCR);
	oo.print("rebind e2: ");
	oo.print(((edition2.rebind((IntegerPos.make(1)), e3)).positionsLabelled(e2.label())));
	oo.print(myCR);
	oo.print("duplicate e1: ");
	oo.print(((edition2.with((IntegerPos.make(1)), e1)).positionsLabelled(e1.label())));
	oo.print(myCR);
	oo.print(myCR);
/*
udanax-top.st:62450:WorksTester methodsFor: 'tests'!
{void} labelTestOn: oo {ostream reference}
	|    edition {FeEdition} e1 {FeEdition} e2 {FeEdition} e3 {FeEdition} 
	  e4 {FeEdition} e1prime {FeEdition} edition2 {FeEdition} |
	e1 _ FeEdition fromArray: (WorksTester string: 'First Edition').
	e2 _ FeEdition fromArray: (WorksTester string: 'Second Edition').
	e3 _ FeEdition fromArray: (WorksTester string: 'Third Edition').
	e4 _ FeEdition fromArray: (WorksTester string: 'Fourth Edition').
	
	edition _ FeEdition fromArray: (PrimSpec pointer arrayWithThree: e1 with: e2 with: (FeWork make: e1)).
	
	oo << 'Labels:' << myCR.
	oo << ' ' << e1 label << ' ' << e2 label << ' ' << e3 label << ' ' << e4 label << myCR.
	oo << 'labelled e1: '<< (edition positionsLabelled: e1 label) << myCR.
	e1prime _ ((edition fetch: (IntegerPos make: IntegerVarZero)) cast: FeEdition) with: (IntegerPos make: 1) with: FeRangeElement placeHolder.
	
	edition2 _ edition with: (IntegerPos make: IntegerVarZero) with: e1prime.
	oo << 'edit e1: ' << (edition2 positionsLabelled: e1 label) << myCR.
	
	oo << 'labelled e2: ' << (edition2 positionsLabelled: e2 label) << myCR.
	oo << 'rebind e2: ' << ((edition2 rebind: (IntegerPos make: 1) with: e3) positionsLabelled: e2 label) << myCR.
	oo << 'duplicate e1: ' << ((edition2 with: (IntegerPos make: 1) with: e1) positionsLabelled: e1 label) << myCR.
	
	oo << myCR.!
*/
}
/**
 * Try making Editions in a variety of ways
 */
public void makeEditionTestOn(PrintWriter oo) {
	FeEdition edn;
	FeRangeElement place;
	FeDataHolder data;
	PrimArray bits;
	oo.print((edn = FeEdition.empty(SequenceSpace.make())));
	oo.print(myCR);
	oo.print((FeEdition.empty(IntegerSpace.make())));
	oo.print(myCR);
	oo.print((FeEdition.placeHolders((IntegerSpace.make().interval((IntegerPos.make(0)), (IntegerPos.make(10)))))));
	oo.print(myCR);
	oo.print((FeEdition.placeHolders(SequenceSpace.make().emptyRegion())));
	oo.print(myCR);
	oo.print((FeEdition.placeHolders(SequenceSpace.make().fullRegion())));
	oo.print(myCR);
	data = FeDataHolder.make((PrimIntValue.make(3)));
	place = FeRangeElement.placeHolder();
	oo.print((FeEdition.fromOne((IntegerPos.make(0)), edn)));
	oo.print(myCR);
	oo.print((FeEdition.fromOne((IntegerPos.make(1)), place)));
	oo.print(myCR);
	oo.print((FeEdition.fromOne((IntegerPos.make(2)), data)));
	oo.print(myCR);
	oo.print((FeEdition.fromAll((IntegerSpace.make().above((IntegerPos.make(10)), true)), edn)));
	oo.print(myCR);
	oo.print((FeEdition.fromAll((IntegerSpace.make().below((IntegerPos.make(100)), false)), place)));
	oo.print(myCR);
	oo.print((FeEdition.fromAll(IntegerSpace.make().emptyRegion(), place)));
	oo.print(myCR);
	oo.print((FeEdition.fromAll(IDSpace.unique().fullRegion(), data)));
	oo.print(myCR);
	oo.print((FeEdition.fromArray((WorksTester.string("")))));
	oo.print(myCR);
	oo.print((FeEdition.fromArray((WorksTester.string("hello world")))));
	oo.print(myCR);
	bits = WorksTester.string("hello world!");
	oo.print((FeEdition.fromArray(bits)));
	oo.print(myCR);
	oo.print((FeEdition.fromArray(bits, (IntegerSpace.make().interval((IntegerPos.make(10)), (IntegerPos.make(22)))))));
	oo.print(myCR
	/* << (FeEdition fromArray: bits
			with: NULL
			with: IntegerSpace make getDescending) << myCR
		<< (FeEdition fromArray: bits
			with: (IntegerSpace make interval: (IntegerPos make: 100) with: (IntegerPos make: 113))
			with: IntegerSpace make getDescending) << myCR */
	);
	oo.print("Making Editions test finished");
	oo.print(myCR);
	oo.print(myCR);
/*
udanax-top.st:62474:WorksTester methodsFor: 'tests'!
{void} makeEditionTestOn: oo {ostream reference}
	"Try making Editions in a variety of ways"
	
	| edn {FeEdition} place {FeRangeElement} data {FeDataHolder} bits {PrimArray} |
	oo << (edn := FeEdition empty: SequenceSpace make) << myCR
		<< (FeEdition empty: IntegerSpace make) << myCR.
	
	oo << (FeEdition placeHolders: (IntegerSpace make interval: (IntegerPos make: IntegerVarZero) with: (IntegerPos make: 10))) << myCR
		<< (FeEdition placeHolders: SequenceSpace make emptyRegion) << myCR
		<< (FeEdition placeHolders: SequenceSpace make fullRegion) << myCR.
	
	data := FeDataHolder make: (PrimIntValue make: 3).
	place := FeRangeElement placeHolder.
	oo << (FeEdition fromOne: (IntegerPos make: IntegerVarZero) with: edn) << myCR
		<< (FeEdition fromOne: (IntegerPos make: 1) with: place) << myCR
		<< (FeEdition fromOne: (IntegerPos make: 2) with: data) << myCR.
	
	oo << (FeEdition fromAll: (IntegerSpace make above: (IntegerPos make: 10) with: true) with: edn) << myCR
		<< (FeEdition fromAll: (IntegerSpace make below: (IntegerPos make: 100) with: false) with: place) << myCR
		<< (FeEdition fromAll: IntegerSpace make emptyRegion with: place) << myCR
		<< (FeEdition fromAll: IDSpace unique fullRegion with: data) << myCR.
	
	oo << (FeEdition fromArray: (WorksTester string: '')) << myCR.
	oo << (FeEdition fromArray: (WorksTester string: 'hello world')) << myCR.
	
	bits := WorksTester string: 'hello world!!'.
	oo << (FeEdition fromArray: bits) << myCR
		<< (FeEdition fromArray: bits with: (IntegerSpace make interval: (IntegerPos make: 10) with: (IntegerPos make: 22))) << myCR
		"<< (FeEdition fromArray: bits
			with: NULL
			with: IntegerSpace make getDescending) << myCR
		<< (FeEdition fromArray: bits
			with: (IntegerSpace make interval: (IntegerPos make: 100) with: (IntegerPos make: 113))
			with: IntegerSpace make getDescending) << myCR".
	
	oo << 'Making Editions test finished' << myCR << myCR.!
*/
}
public void ownerTestOn(PrintWriter oo) {
	FeWork work;
	FeClub club;
	FeEdition edition;
	club = (FeClub) (FeServer.get(FeServer.publicClubID()));
	oo.print("Club: ");
	oo.print(club);
	oo.print(" owned by: ");
	oo.print(club.owner());
	oo.print(myCR);
	Object initialOwnerOldValue = AboraBlockSupport.enterFluidBindDuring(InitialOwner, ((ID) CurrentAuthor.fluidGet()));
	try {
		work = FeWork.make((FeEdition.fromArray((WorksTester.string("The one I can change.")))));
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(InitialOwner, initialOwnerOldValue);
	}
	oo.print("Work: ");
	oo.print(work);
	oo.print(" owned by: ");
	oo.print(work.owner());
	oo.print(myCR);
	edition = FeEdition.fromArray((PrimSpec.pointer().arrayWithTwo(work, club)), ((WorksTester.sequence("changeable")).asRegion().with((WorksTester.sequence("permanent")))), SequenceSpace.make().ascending());
	oo.print("Set owners of: ");
	oo.print(edition);
	oo.print(myCR);
	oo.print("result: ");
	oo.print((edition.setRangeOwners(FeServer.publicClubID())));
	oo.print(myCR);
	oo.print("Club: ");
	oo.print(club);
	oo.print(" owned by: ");
	oo.print(club.owner());
	oo.print(myCR);
	oo.print("Work: ");
	oo.print(work);
	oo.print(" owned by: ");
	oo.print(work.owner());
	oo.print(myCR);
	oo.print(myCR);
/*
udanax-top.st:62511:WorksTester methodsFor: 'tests'!
{void} ownerTestOn: oo {ostream reference}
	|  work {FeWork} club {FeClub} edition {FeEdition} |
	club := (FeServer get: FeServer publicClubID) cast: FeClub.
	oo << 'Club: ' << club << ' owned by: ' << club owner << myCR.
	InitialOwner fluidBind: CurrentAuthor fluidGet during:
		[work _ FeWork make: (FeEdition fromArray: (WorksTester string: 'The one I can change.'))].
	oo << 'Work: ' << work << ' owned by: ' << work owner << myCR.
	
	edition _ FeEdition fromArray: (PrimSpec pointer arrayWithTwo: work with: club) 
		with: ((WorksTester sequence: 'changeable') asRegion with: (WorksTester sequence: 'permanent'))
		with: SequenceSpace make ascending.
	
	oo << 'Set owners of: ' << edition << myCR.
	oo << 'result: ' << (edition setRangeOwners: FeServer publicClubID) << myCR.
	oo << 'Club: ' << club << ' owned by: ' << club owner << myCR.
	oo << 'Work: ' << work << ' owned by: ' << work owner << myCR.
	
	oo << myCR.!
*/
}
/**
 * Test the sponsoring mechanism
 */
public void sponsorTestOn(PrintWriter oo) {
	FeClub club;
	FeClub testClub;
	FeEdition blank;
	FeWork w1;
	FeWork w2;
	testClub = (FeClub) (FeServer.get(((ID) CurrentAuthor.fluidGet())));
	club = (FeClub) FeClub.make((FeClubDescription.make((FeSet.make(((PtrArray) (PrimSpec.pointer().arrayWith((FeServer.get(((ID) CurrentAuthor.fluidGet())))))))), FeWallLockSmith.make())).edition());
	blank = FeEdition.fromArray((UInt8Array.string("blank")));
	w1 = FeWork.make(blank);
	FeServer.assignID(w1);
	w2 = FeWork.make(blank);
	FeServer.assignID(w1);
	oo.print("Initially ");
	oo.print(myCR);
	oo.print("sponsored by Test: ");
	oo.print(testClub.sponsoredWorks());
	oo.print(myCR);
	oo.print("sponsored by new: ");
	oo.print(club.sponsoredWorks());
	oo.print(myCR);
	oo.print("work 1 sponsors: ");
	oo.print(w1.sponsors());
	oo.print(myCR);
	oo.print("work 2 sponsors: ");
	oo.print(w2.sponsors());
	oo.print(myCR);
	w1.sponsor(((IDRegion) ((ID) CurrentAuthor.fluidGet()).asRegion()));
	w2.sponsor(((IDRegion) (((ID) CurrentAuthor.fluidGet()).asRegion().with((FeServer.iDOf(club))))));
	oo.print("After sponsoring ");
	oo.print(myCR);
	oo.print("sponsored by Test: ");
	oo.print(testClub.sponsoredWorks());
	oo.print(myCR);
	oo.print("sponsored by new: ");
	oo.print(club.sponsoredWorks());
	oo.print(myCR);
	oo.print("work 1 sponsors: ");
	oo.print(w1.sponsors());
	oo.print(myCR);
	oo.print("work 2 sponsors: ");
	oo.print(w2.sponsors());
	oo.print(myCR);
	w1.unsponsor(((IDRegion) ((ID) CurrentAuthor.fluidGet()).asRegion()));
	w2.unsponsor(((IDRegion) (((ID) CurrentAuthor.fluidGet()).asRegion().with((FeServer.iDOf(club))))));
	oo.print("After unsponsoring ");
	oo.print(myCR);
	oo.print("sponsored by Test: ");
	oo.print(testClub.sponsoredWorks());
	oo.print(myCR);
	oo.print("sponsored by new: ");
	oo.print(club.sponsoredWorks());
	oo.print(myCR);
	oo.print("work 1 sponsors: ");
	oo.print(w1.sponsors());
	oo.print(myCR);
	oo.print("work 2 sponsors: ");
	oo.print(w2.sponsors());
	oo.print(myCR);
	Someone.thingToDo();
	/* test authority checks */
	Someone.thingToDo();
	/* get rid of persistent info */
/*
udanax-top.st:62532:WorksTester methodsFor: 'tests'!
{void} sponsorTestOn: oo {ostream reference}
	"Test the sponsoring mechanism"
	
	| club {FeClub} testClub {FeClub} blank {FeEdition} w1 {FeWork} w2 {FeWork} |
	testClub := (FeServer get: CurrentAuthor fluidGet) cast: FeClub.
	club := FeClub make: 
			(FeClubDescription make: 
				(FeSet make: 
					((PrimSpec pointer arrayWith: (FeServer get: CurrentAuthor fluidGet)) cast: PtrArray))
		with: FeWallLockSmith make) edition.
	
	blank := FeEdition fromArray: (UInt8Array string:'blank').
	w1 := FeWork make: blank.
	FeServer assignID: w1.
	w2 := FeWork make: blank.
	FeServer assignID: w1.
	oo << 'Initially ' << myCR
		<< 'sponsored by Test: ' << testClub sponsoredWorks << myCR
		<< 'sponsored by new: ' << club sponsoredWorks << myCR
		<< 'work 1 sponsors: ' << w1 sponsors << myCR
		<< 'work 2 sponsors: ' << w2 sponsors << myCR.
	w1 sponsor: (CurrentAuthor fluidGet asRegion cast: IDRegion).
	w2 sponsor: ((CurrentAuthor fluidGet asRegion
					with: (FeServer iDOf: club)) cast: IDRegion).
	oo << 'After sponsoring ' << myCR
		<< 'sponsored by Test: ' << testClub sponsoredWorks << myCR
		<< 'sponsored by new: ' << club sponsoredWorks << myCR
		<< 'work 1 sponsors: ' << w1 sponsors << myCR
		<< 'work 2 sponsors: ' << w2 sponsors << myCR.
	w1 unsponsor: (CurrentAuthor fluidGet asRegion cast: IDRegion).
	w2 unsponsor: ((CurrentAuthor fluidGet asRegion
					with: (FeServer iDOf: club)) cast: IDRegion).
	oo << 'After unsponsoring ' << myCR
		<< 'sponsored by Test: ' << testClub sponsoredWorks << myCR
		<< 'sponsored by new: ' << club sponsoredWorks << myCR
		<< 'work 1 sponsors: ' << w1 sponsors << myCR
		<< 'work 2 sponsors: ' << w2 sponsors << myCR.
	
	self thingToDo. "test authority checks"
	self thingToDo. "get rid of persistent info"!
*/
}
public void transcludersBugTestOn(PrintWriter oo) {
	FeText text;
	FeEdition refs;
	FeWork work;
	FeFillRangeDetector detector;
	FeEdition container;
	FeText text2;
	PtrArray values;
	oo.print(myCR);
	oo.print(myCR);
	oo.print("Transcluders bug test");
	oo.print(myCR);
	oo.print(myCR);
	/* Test a bug in the transcluders mechanism:
		if E1 = {x -> E2}, then E1 transcluders will be triggered by an Edition containing E2 */
	text = FeText.make(((PrimDataArray) (WorksTester.string("oops"))));
	container = FeEdition.fromOne(IntegerPos.make(1), text.edition());
	refs = container.transcluders((FeWrapperSpec.get((WorksTester.sequence("HyperRef")))).filter());
	detector = WorksTestFillRangeDetector.make(oo, "Should not have been transcluded by ");
	refs.addFillRangeDetector(detector);
	work = FeWork.make((FeSingleRef.make(text.edition())).edition());
	refs.removeFillRangeDetector(detector);
	/* if E1 = {x -> E2, y -> E3}, then E1 transcluders may be triggered by another separately created Edition containing E1 & E2? */
	values = PtrArray.nulls(2);
	text2 = FeText.make(((PrimDataArray) (WorksTester.string("oops"))));
	values.store(0, text.edition());
	values.store(1, text2.edition());
	container = FeEdition.fromArray(values);
	refs = container.transcluders((FeWrapperSpec.get((WorksTester.sequence("HyperRef")))).filter());
	refs.addFillRangeDetector(detector);
	work.revise((FeEdition.fromArray(values)));
	refs.removeFillRangeDetector(detector);
/*
udanax-top.st:62573:WorksTester methodsFor: 'tests'!
{void} transcludersBugTestOn: oo {ostream reference}
	
	|  text {FeText} refs {FeEdition} work {FeWork}
	  detector {FeFillRangeDetector} container {FeEdition}
	  text2 {FeText} values {PtrArray} |
	oo << myCR << myCR << 'Transcluders bug test' << myCR << myCR.
	"Test a bug in the transcluders mechanism:
		if E1 = {x -> E2}, then E1 transcluders will be triggered by an Edition containing E2"
	text := FeText make: ((WorksTester string: 'oops') cast: PrimDataArray).
	container := FeEdition fromOne: 1 integer with: text edition.
	refs := container transcluders: (FeWrapperSpec get: (WorksTester sequence: 'HyperRef')) filter.
	detector := WorksTestFillRangeDetector make: oo
		with: 'Should not have been transcluded by '.
	refs addFillRangeDetector: detector.
	work := FeWork make: (FeSingleRef make: text edition) edition.
	refs removeFillRangeDetector: detector.
	
	"if E1 = {x -> E2, y -> E3}, then E1 transcluders may be triggered by another separately created Edition containing E1 & E2?"
	values := PtrArray nulls: 2.
	text2 := FeText make: ((WorksTester string: 'oops') cast: PrimDataArray).
	values at: UInt32Zero store: text edition.
	values at: 1 store: text2 edition.
	container := FeEdition fromArray: values.
	refs := container transcluders: (FeWrapperSpec get: (WorksTester sequence: 'HyperRef')) filter.
	refs addFillRangeDetector: detector.
	work revise: (FeEdition fromArray: values).
	refs removeFillRangeDetector: detector.!
*/
}
/**
 * Test the transclusions query
 */
public void transclusionsTestOn(PrintWriter oo) {
	FeText text;
	int n;
	FeEdition texts;
	FeEdition refs;
	FeFillRangeDetector detector;
	FeWork work;
	IntegerRegion interval;
	oo.print(myCR);
	oo.print(myCR);
	oo.print("Transclusions test");
	oo.print(myCR);
	oo.print(myCR);
	text = FeText.make(((PrimDataArray) (WorksTester.string("(abcdefghijklmnopqrstuvwxyz)"))));
	n = text.count();
	work = FeWork.make((FeSingleRef.make(text.edition())).edition());
	texts = text.edition().rangeTranscluders(null, (FeWrapperSpec.get((WorksTester.sequence("Text")))).filter());
	refs = text.edition().rangeTranscluders(null, (FeWrapperSpec.get((WorksTester.sequence("HyperRef")))).filter());
	detector = WorksTestFillRangeDetector.make(oo, "Transcluded by ");
	texts.addFillRangeDetector(detector);
	refs.addFillRangeDetector(detector);
	interval = IntegerSpace.make().interval((IntegerPos.make((n / 2))), (IntegerPos.make(n)));
	text = text.move(0, interval);
	work.revise((FeSingleRef.make(text.edition())).edition());
	text = text.extract(((IntegerRegion) (IntegerRegion.integerExtent(n / 4, n / 4)).complement()));
	work.revise((FeSingleRef.make(text.edition())).edition());
	text = text.insert(n / 2, (FeText.make(((PrimDataArray) ((WorksTester.string("[ABCDEFGHIJKLMNOPQRSTUVWXYZ]")))))));
	work.revise((FeSingleRef.make(text.edition())).edition());
	texts = text.edition().rangeTranscluders(interval, (FeWrapperSpec.get((WorksTester.sequence("Text")))).filter(), null, 0, texts);
	refs = text.edition().rangeTranscluders(interval, (FeWrapperSpec.get((WorksTester.sequence("HyperRef")))).filter(), null, 0, refs);
	text = text.extract((IntegerSpace.make().above((IntegerPos.make(n / 2)), true)));
	work.revise((FeSingleRef.make(text.edition())).edition());
	text = text.extract((IntegerSpace.make().below((IntegerPos.make(n)), false)));
	work.revise((FeSingleRef.make(text.edition())).edition());
	text = text.move(0, interval);
	work.revise((FeSingleRef.make(text.edition())).edition());
	texts.removeFillRangeDetector((detector));
	refs.removeFillRangeDetector((detector));
/*
udanax-top.st:62603:WorksTester methodsFor: 'tests'!
{void} transclusionsTestOn: oo {ostream reference}
	"Test the transclusions query"
	
	|  text {FeText} n {IntegerVar} texts {FeEdition} refs {FeEdition}
	  detector {FeFillRangeDetector} work {FeWork} interval {IntegerRegion} |
	oo << myCR << myCR << 'Transclusions test' << myCR << myCR.
	text := FeText make: ((WorksTester string: '(abcdefghijklmnopqrstuvwxyz)') cast: PrimDataArray).
	n := text count.
	work := FeWork make: (FeSingleRef make: text edition) edition.
	texts := text edition rangeTranscluders: NULL
		with: (FeWrapperSpec get: (WorksTester sequence: 'Text')) filter.
	refs := text edition rangeTranscluders: NULL
		with: (FeWrapperSpec get: (WorksTester sequence: 'HyperRef')) filter.
	detector := WorksTestFillRangeDetector make: oo with: 'Transcluded by '.
	texts addFillRangeDetector: detector.
	refs addFillRangeDetector: detector.
	interval _ IntegerSpace make interval: (IntegerPos make: (n // 2)) with: (IntegerPos make: n).
	text := text move: IntegerVarZero with: interval.
	work revise: (FeSingleRef make: text edition) edition.
	text := text extract: ((IntegerRegion integerExtent: n // 4 with: n // 4) complement cast: IntegerRegion).
	work revise: (FeSingleRef make: text edition) edition.
	text := text insert: n // 2 with: (FeText make: (((WorksTester string: '[ABCDEFGHIJKLMNOPQRSTUVWXYZ]')) cast: PrimDataArray)).
	work revise: (FeSingleRef make: text edition) edition.
	
	texts := text edition rangeTranscluders: interval
		with: (FeWrapperSpec get: (WorksTester sequence: 'Text')) filter
		with: NULL
		with: Int32Zero
		with: texts.
	refs := text edition rangeTranscluders: interval
		with: (FeWrapperSpec get: (WorksTester sequence: 'HyperRef')) filter
		with: NULL
		with: Int32Zero
		with: refs.
	
	text := text extract: (IntegerSpace make above: (IntegerPos make: n // 2) with: true).
	work revise: (FeSingleRef make: text edition) edition.
	text := text extract: (IntegerSpace make below: (IntegerPos make: n) with: false).
	work revise: (FeSingleRef make: text edition) edition.
	text := text move: IntegerVarZero with: interval.
	work revise: (FeSingleRef make: text edition) edition.
	
	texts removeFillRangeDetector: (detector cast: FeFillRangeDetector).
	refs removeFillRangeDetector: (detector cast: FeFillRangeDetector).!
*/
}
/**
 * Try the various operations on Works
 */
public void workTestOn(PrintWriter oo) {
	FeEdition e1;
	FeWork w1;
	FeKeyMaster km;
	e1 = FeEdition.fromArray((WorksTester.string("hello world")));
	w1 = FeWork.make(e1);
	dumpWorkOn(oo, "As newly created ", w1);
	w1.addStatusDetector((WorksTestStatusDetector.make(oo, "\n"+
"Work 1")));
	w1.release();
	dumpWorkOn(oo, "With authority restored", w1);
	w1.grab();
	dumpWorkOn(oo, "Grabbed", w1);
	Object currentKeyMasterOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentKeyMaster, ((BooLock) (FeServer.loginByName((Sequence.string("Test"))))).boo());
	try {
		w1.grab();
		dumpWorkOn(oo, "Grabbed again", w1);
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentKeyMaster, currentKeyMasterOldValue);
	}
	w1.requestGrab();
	dumpWorkOn(oo, "Grab requested again", w1);
	w1.release();
	dumpWorkOn(oo, "Released", w1);
	km = FeKeyMaster.makePublic();
	Object currentKeyMasterOldValue1 = AboraBlockSupport.enterFluidBindDuring(CurrentKeyMaster, km);
	try {
		w1.requestGrab();
		dumpWorkOn(oo, "Grab requested yet again", w1);
		km.incorporate(((BooLock) (FeServer.loginByName((Sequence.string("Test"))))).boo());
		dumpWorkOn(oo, "KeyMaster incorporated", w1);
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentKeyMaster, currentKeyMasterOldValue1);
	}
	km.removeLogins(((IDRegion) ((ID) CurrentAuthor.fluidGet()).asRegion()));
	dumpWorkOn(oo, "KeyMaster login removed", w1);
/*
udanax-top.st:62650:WorksTester methodsFor: 'tests'!
{void} workTestOn: oo {ostream reference}
	"Try the various operations on Works"
	
	| e1 {FeEdition} w1 {FeWork} km {FeKeyMaster} |
	e1 := FeEdition fromArray: (WorksTester string: 'hello world').
	w1 := FeWork make: e1.
	self dumpWorkOn: oo with: 'As newly created ' with: w1.
	
	w1 addStatusDetector: (WorksTestStatusDetector make: oo with: '
Work 1').
	w1 release.
	
	self dumpWorkOn: oo with: 'With authority restored' with: w1.
	w1 grab.
	self dumpWorkOn: oo with: 'Grabbed' with: w1.
	CurrentKeyMaster fluidBind: ((FeServer loginByName: (Sequence string: 'Test')) cast: BooLock) boo during:
		[w1 grab.
		self dumpWorkOn: oo with: 'Grabbed again' with: w1].
	w1 requestGrab.
	self dumpWorkOn: oo with: 'Grab requested again' with: w1.
	w1 release.
	self dumpWorkOn: oo with: 'Released' with: w1.
	km := FeKeyMaster makePublic.
	CurrentKeyMaster fluidBind: km during:
		[w1 requestGrab.
		self dumpWorkOn: oo with: 'Grab requested yet again' with: w1.
		km incorporate: ((FeServer loginByName: (Sequence string: 'Test')) cast: BooLock) boo.
		self dumpWorkOn: oo with: 'KeyMaster incorporated' with: w1].
	
	km removeLogins: (CurrentAuthor fluidGet asRegion cast: IDRegion).
	self dumpWorkOn: oo with: 'KeyMaster login removed' with: w1.!
*/
}
/**
 * Print the state and contents of a Work
 */
public void dumpWorkOn(PrintWriter oo, String tag, FeWork work) {
	oo.print(myCR);
	oo.print(tag);
	oo.print("[");
	if (work.canRead()) {
		oo.print(work.edition());
	}
	if (work.canRevise()) {
		oo.print(" (grabbed)");
	}
	oo.print("]");
/*
udanax-top.st:62685:WorksTester methodsFor: 'private:'!
{void} dumpWorkOn: oo {ostream reference} with: tag {char star} with: work {FeWork}
	"Print the state and contents of a Work"
	
	oo << myCR << tag << '['.
	work canRead ifTrue:
		[oo << work edition].
	work canRevise ifTrue:
		[oo << ' (grabbed)'].
	oo << ']'!
*/
}
public void restartWorksTester(Rcvr rcvr) {
	myConnection = null;
	myCR = "\n"+
"";
/*
udanax-top.st:62697:WorksTester methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartWorksTester: rcvr {Rcvr unused default: NULL}
	myConnection := NULL.
	myCR := '
'!
*/
}
public WorksTester(Rcvr receiver) {
	super(receiver);
	restartWorksTester(receiver);
/*
udanax-top.st:62705:WorksTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	self restartWorksTester: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:62709:WorksTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
/**
 * Looks up the ID of a named Club in the directory maintained by the System Admin Club.
 * Requires read permission on the directory. Blasts if there is no Club with that name.
 */
public static ID clubID(Sequence clubName) {
	return FeServer.iDOf((((FeWork) (FeServer.get(FeServer.clubDirectoryID()))).edition().get(clubName)));
/*
udanax-top.st:62721:WorksTester class methodsFor: 'server library'!
{ID} clubID: clubName {Sequence}
	"Looks up the ID of a named Club in the directory maintained by the System Admin Club. Requires read permission on the directory. Blasts if there is no Club with that name."
	
	^FeServer iDOf: (((FeServer get: FeServer clubDirectoryID) cast: FeWork) edition get: clubName)!
*/
}
public static IntegerPos integer(int val) {
	return IntegerPos.make(val);
/*
udanax-top.st:62726:WorksTester class methodsFor: 'server library'!
{IntegerPos} integer: val {IntegerVar}
	^ IntegerPos make: val!
*/
}
public static Sequence sequence(String string) {
	return Sequence.string(string);
/*
udanax-top.st:62730:WorksTester class methodsFor: 'server library'!
{Sequence} sequence: string {char star}
	^ Sequence string: string!
*/
}
public static PrimArray string(String string) {
	return UInt8Array.string(string);
/*
udanax-top.st:62734:WorksTester class methodsFor: 'server library'!
{PrimArray} string: string {char star}
	^UInt8Array string: string!
*/
}
public static void linkTimeNonInherited() {
	TheTester = null;
/*
udanax-top.st:62740:WorksTester class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheTester := NULL!
*/
}
/**
 * PrServer defineFluid: #ClientServer with: XuPromise emulsion with: [NULL]
 */
public static void staticTimeNonInherited() {
/*
udanax-top.st:62743:WorksTester class methodsFor: 'smalltalk: init'!
staticTimeNonInherited
	"PrServer defineFluid: #ClientServer with: XuPromise emulsion with: [NULL]"!
*/
}
public WorksTester() {
/*

Generated during transformation
*/
}
}
